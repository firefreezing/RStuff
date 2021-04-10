library(pacman)
p_load(ProjectTemplate)
p_load(ISLR)
p_load(randomForest)
p_load(caret)
p_load(AppliedPredictiveModeling)
p_load(tidyverse)
p_load(stringr)
p_load(plotROC)
p_load(e1071)
p_load(import)
p_load(foreach)

p_load(parallel)
p_load(doParallel)
cluster <- makeCluster(20)
registerDoParallel(cluster)
# load.project()

source("./0_accessory_fcts.r")
source("./1_data_processing.r")


# Random forest -----------------------------------------------------------


# 5-fold C-V for the LASSO model on the training dataset
fitCtr <- trainControl(
  # 5 fold CV
  method = "cv",
  number = 3,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = T,
  allowParallel=T
)


n_var <- length(x_list)

# ! per this post: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# the ntree is not an explicit tuning parameter in caret pkg. " It is less difficult or critical 
# to tune and could be limited more by compute time available more than anything". Therein, it is
# not available in caret for tuning.

tic <- Sys.time()

rfFit <- train(
  fmla_full_ml, 
  data = training,
  method = "parRF",
  trControl = fitCtr,
  metric = "ROC",
  ntree = 250,  # by default it runs 500 trees
  tuneGrid = expand.grid(.mtry = round(sqrt(n_var)))
)

toc <- Sys.time()
runtime_rf <- toc - tic  # about 1.5 hrs

runtime_rf

print(rfFit) 
plot(rfFit)

# get the variable importance list
var_imp_rf <- data.frame(
  importance = varImp(rfFit)$importance$Overall,
  var_name = rownames(varImp(rfFit)$importance)
) %>%
  arrange(desc(importance)) %>%
  mutate(var_name = factor(var_name, levels = var_name))

ggplot(var_imp_rf) +
  geom_point(aes(x = importance, y = var_name)) +
  labs(title = "Variable Importance from Random Forest",
       x = "Importance",
       y = "Variable names")


# add id to sorted var importance list
var_imp_rf$ID <- seq.int(nrow(var_imp_rf))




# remove the variables of small importance
x_list_final <- var_imp_rf %>%
  filter(ID <= 40 | var_name %in% var_ses) %>%
  .[["var_name"]] %>% as.vector()


fmla_final_rf <- fct_build_fmla(outcome_recode, x_list_final)  # fmla for the final RF model

n_var <- length(x_list_final)

tic <- Sys.time()
# fit the final model on the training data
rfFit_final <- train(
  fmla_final_rf, 
  data = training,
  method = "parRF",
  metric = "ROC",
  ntree = 250,  # by default it runs 500 trees
  tuneGrid = expand.grid(.mtry = round(sqrt(n_var))),
  trControl = fitCtr
)
toc <- Sys.time()
runtime_rf <- toc - tic  # about 1.5 hrs
runtime_rf


# end cluster
stopCluster(cluster)
registerDoSEQ()

# cross validated stats
perf_cv <- ROCR::prediction(predictions=rfFit_final$pred$yes, 
                            labels=ifelse(rfFit_final$pred$obs=="yes",1,0))
roc_perf_cv <- ROCR::performance(perf_cv, measure = "tpr", x.measure = "fpr")
plot(roc_perf_cv)
abline(a=0, b= 1)

auc_perf_cv <- ROCR::performance(perf_cv, measure = "auc")
auc_perf_cv@y.values

# optimal threshold based on ROC 
cost_perf_cv <- ROCR::performance(perf_cv, "cost", 
                                  cost.fp = 1, cost.fn = 1)
cut_pt <-perf_cv@cutoffs[[1]][which.min(cost_perf_cv@y.values[[1]])]

conf_mat_cv <- confusionMatrix(reference = ifelse(rfFit_final$pred$obs=="yes",1,0) %>% as.factor,
                               data = ifelse(rfFit_final$pred$yes > cut_pt, 1, 0) %>% as.factor,
                               positive = "1")
conf_mat_cv

# training stats
pred_train <- predict(rfFit_final, 
                      training %>% dplyr::select(one_of(x_list_final)), 
                      type = "prob")
perf_train <- ROCR::prediction(predictions=pred_train$yes,
                               labels=training[[outcome]])
roc_perf_train <- ROCR::performance(perf_train, measure = "tpr", x.measure = "fpr")
plot(roc_perf_train)
abline(a=0, b= 1)

auc_perf_train <- ROCR::performance(perf_train, measure = "auc")
auc_perf_train@y.values

conf_mat_train <- confusionMatrix(reference = training[[outcome]] %>% as.factor,
                                  data = ifelse(pred_train$yes > cut_pt, 1, 0) %>% as.factor,
                                  positive = "1")
conf_mat_train

# run the model on testing data
pred_test <- predict(rfFit_final, 
                     testing %>% dplyr::select(one_of(x_list_final)), 
                     type = "prob")
perf_test <- ROCR::prediction(predictions = pred_test$yes,
                              labels = testing[[outcome]])
roc_perf_test <- ROCR::performance(perf_test, measure = "tpr", x.measure = "fpr")
plot(roc_perf_test)
abline(a=0, b= 1)

auc_perf_test <- ROCR::performance(perf_test, measure = "auc")
auc_perf_test@y.values

conf_mat_test <- confusionMatrix(reference = testing[[outcome]] %>% as.factor,
                                 data = ifelse(pred_test$yes > cut_pt, 1, 0) %>% as.factor,
                                 positive = "1")
conf_mat_test

# rf model summary -----------------------------------------------------

# on the validated sample
rf_summary_cv <- list(
  auc = auc_perf_cv@y.values,
  consistency = conf_mat_cv$overall,
  byClass = conf_mat_cv$byClass,
  contTbl = conf_mat_cv$table
)

rf_summary_cv

# on the training sample
rf_summary_train <- list(
  auc = auc_perf_train@y.values,
  consistency = conf_mat_train$overall,
  byClass = conf_mat_train$byClass,
  contTbl = conf_mat_train$table
)

rf_summary_train

# on the testing sample
rf_summary_test <- list(
  auc = auc_perf_test@y.values,
  consistency = conf_mat_test$overall,
  byClass = conf_mat_test$byClass,
  contTbl = conf_mat_test$table
)

rf_summary_test

# RF model variable importance summary ------------------------------------
# get the variable importance list
var_imp_rf_final <- data.frame(
  importance = varImp(rfFit_final)$importance$Overall,
  var_name = rownames(varImp(rfFit_final)$importance)
) %>%
  arrange(desc(importance)) %>%
  mutate(var_name = factor(var_name, levels = var_name))

write.csv(var_imp_rf_final, "rf_final_frailty10_ntrees250_date.csv", row.names = FALSE)


# Reference ---------------------------------------------------------------

# Caret tutorial: https://topepo.github.io/caret/index.html
# discussion on backward selection: 
# on connecting aic and pvalue https://stats.stackexchange.com/questions/97257/stepwise-regression-in-r-critical-p-value/97309
# on issue of variable selection using pvalue: https://stackoverflow.com/questions/3701170/stepwise-regression-using-p-values-to-drop-variables-with-nonsignificant-p-value

