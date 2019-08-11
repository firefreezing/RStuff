########################################################################
# Nest function demo
#   
#   created 2019-08-11
########################################################################

library(pacman)
p_load(gapminder)
p_load(tidyverse)
p_load(broom)
p_load(caret)
p_load(mlbench)

# I. first quick example -------------------------------------------------------

# load data
gapminder <- gapminder %>% 
    mutate(year1950 = year - 1950)

# nested data
by_country <- gapminder %>%
    group_by(continent, country) %>%
    nest()
 
by_country$data[[1]]
 
# fit a model
country_mdl <- function(df){
    lm(lifeExp ~ year1950, data = df)
    }

names(by_country) 

models <- by_country %>%
    mutate(mod = map(data, country_mdl))
 
models <- models %>%
    mutate(glance = mod %>% map(broom::glance),
           rsq = glance %>% map_dbl("r.squared"),
           tidy = mod %>% map(broom::tidy),
           augment = mod %>% map(broom::augment))

# another example to directly calculate rsq
models2 <- by_country %>%
    mutate(rsq = map(data, ~cor(.x$lifeExp, .x$year1950)))

# use the models data to create plots
models %>%
    ggplot(aes(rsq, reorder(country, rsq))) +
    geom_point(aes(color = continent))

# unnest the data to return long dataframe

unnest(models, data)
unnest(models, rsq, .drop = T)  # the .drop argument to unnest is useful
unnest(models, tidy, .drop = T)


# II. ML model ------------------------------------------------------------

data(Sonar)

set.seed(998)

inGroup1 <- createDataPartition(Sonar$Class, p = .5, list = FALSE)

Sonar <- Sonar %>%
    mutate(id = 1:nrow(Sonar),
           source = ifelse(id %in% inGroup1, "group1", "group2"))

fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)

fct_fit_mdl <- function(data){
    mdlFit <- train(Class ~ ., data = data, 
                     method = "gbm", 
                     trControl = fitControl,
                     ## This last option is actually one
                     ## for gbm() that passes through
                     verbose = FALSE)
    return(mdlFit)
}

# fit GBM model on two groups simutaneously
model_ml <- Sonar %>%
    group_by(source) %>%
    nest() %>%
    mutate(fit = map(data, fct_fit_mdl))

model_ml2 <- model_ml %>%
    mutate(acc = map_dbl(fit, ~max(.x$results$Accuracy)))

# output the optimal accuracy
model_ml2$acc

# References --------------------------------------------------------------

# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html