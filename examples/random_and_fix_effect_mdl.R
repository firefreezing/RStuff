library(tidyverse)
library(lme4)

# https://ourcodingclub.github.io/tutorials/mixed-models/

# simulate the data:
# 20 hospitals, each has 100 patients
n_h <- 20
n_p <- 100

# fix effect model
# y_ij = a_i + eps_ij, where a_i is a fixed effect and eps_ij ~ N(0, .5)
set.seed(2020)

a <- rnorm(n_h, 3, 2)
print(a)

dat_fix <- data.frame(
  y = rep(a, each = n_p) + rnorm(n_h*n_p, 0, .5),
  hosp_id = rep(str_pad(1:n_h, 2, side = "left", pad = "0"), each = n_p)
) %>%
  mutate(hosp_id = hosp_id %>%  
           as.factor %>% relevel(ref = "01"))

mdl_fix_effect <- lm(y ~ hosp_id, data = dat_fix)
summary(mdl_fix_effect)

mdl_rand_effect <- lmer(y ~ (1 | hosp_id), data = dat_fix)
summary(mdl_rand_effect)
