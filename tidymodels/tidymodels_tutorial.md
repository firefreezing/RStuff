tidymodels
================
firefreezing
4/10/2021

### Nex steps:

1.  try the workflow: add\_recipe, add\_model, fine\_tune, etc.
2.  try both training-validate-test, and cv + test
3.  set a workflow to screen many models
4.  benchmarking (running time)

### Useful functions:

  - `prep()`: execute the transformations from the recipe on top of the
    data that is supplied
  - `bake()`: turn the testing data into a preprocessed testing data
    following the recipe
  - `juice()`: turn the training data into a preprocessed training data
    following the recipe

<!-- end list -->

``` r
library(tidymodels)

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results
library(vip)
```

## Data preparation

You can also embed plots, for example:

    FALSE [1] 50000    23

``` r
glimpse(hotels)
```

    FALSE Rows: 50,000
    FALSE Columns: 23
    FALSE $ hotel                          <fct> City_Hotel, City_Hotel, Resort_Hotel, R…
    FALSE $ lead_time                      <dbl> 217, 2, 95, 143, 136, 67, 47, 56, 80, 6…
    FALSE $ stays_in_weekend_nights        <dbl> 1, 0, 2, 2, 1, 2, 0, 0, 0, 2, 1, 0, 1, …
    FALSE $ stays_in_week_nights           <dbl> 3, 1, 5, 6, 4, 2, 2, 3, 4, 2, 2, 1, 2, …
    FALSE $ adults                         <dbl> 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 1, 2, …
    FALSE $ children                       <fct> none, none, none, none, none, none, chi…
    FALSE $ meal                           <fct> BB, BB, BB, HB, HB, SC, BB, BB, BB, BB,…
    FALSE $ country                        <fct> DEU, PRT, GBR, ROU, PRT, GBR, ESP, ESP,…
    FALSE $ market_segment                 <fct> Offline_TA/TO, Direct, Online_TA, Onlin…
    FALSE $ distribution_channel           <fct> TA/TO, Direct, TA/TO, TA/TO, Direct, TA…
    FALSE $ is_repeated_guest              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    FALSE $ previous_cancellations         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    FALSE $ previous_bookings_not_canceled <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    FALSE $ reserved_room_type             <fct> A, D, A, A, F, A, C, B, D, A, A, D, A, …
    FALSE $ assigned_room_type             <fct> A, K, A, A, F, A, C, A, D, A, D, D, A, …
    FALSE $ booking_changes                <dbl> 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    FALSE $ deposit_type                   <fct> No_Deposit, No_Deposit, No_Deposit, No_…
    FALSE $ days_in_waiting_list           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    FALSE $ customer_type                  <fct> Transient-Party, Transient, Transient, …
    FALSE $ average_daily_rate             <dbl> 80.75, 170.00, 8.00, 81.00, 157.60, 49.…
    FALSE $ required_car_parking_spaces    <fct> none, none, none, none, none, none, non…
    FALSE $ total_of_special_requests      <dbl> 1, 3, 2, 1, 4, 1, 1, 1, 1, 1, 0, 1, 0, …
    FALSE $ arrival_date                   <date> 2016-09-01, 2017-08-25, 2016-11-19, 20…

### Data splitting & resampling

``` r
set.seed(123)

splits <- initial_split(hotels, prop = .8, strata = "children")

hotel_train <- training(splits)
hotel_test <- testing(splits)
```

Training data and testing data have similar proportion by `children`,
thanks to the `strata` argument.

``` r
hotel_train %>%
  count(children) %>%
  mutate(prop = n/sum(n))
```

    FALSE # A tibble: 2 x 3
    FALSE   children     n   prop
    FALSE   <fct>    <int>  <dbl>
    FALSE 1 children  3251 0.0813
    FALSE 2 none     36750 0.919

``` r
hotel_test %>%
  count(children) %>%
  mutate(prop = n/sum(n))
```

    FALSE # A tibble: 2 x 3
    FALSE   children     n   prop
    FALSE   <fct>    <int>  <dbl>
    FALSE 1 children   787 0.0787
    FALSE 2 none      9212 0.921

## Recipe: data preprocessing

``` r
holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(children ~ ., data = hotel_train) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date, holidays = holidays) %>% 
  step_rm(arrival_date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

lr_recipe
```

    FALSE Data Recipe
    FALSE 
    FALSE Inputs:
    FALSE 
    FALSE       role #variables
    FALSE    outcome          1
    FALSE  predictor         22
    FALSE 
    FALSE Operations:
    FALSE 
    FALSE Date features from arrival_date
    FALSE Holiday features from arrival_date
    FALSE Delete terms arrival_date
    FALSE Dummy variables from all_nominal(), -all_outcomes()
    FALSE Zero variance filter on all_predictors()
    FALSE Centering and scaling for all_predictors()

**prep**

`prep` executes the transformations on top of the data.

``` r
lr_recipe_exe <- lr_recipe %>% prep
```

**bake**

``` r
# "juice" the processed training data
hotel_train_clean <- juice(lr_recipe_exe)

# "bake" the processed testing data
hotel_test_clean <- lr_recipe_exe %>%
  bake(new_data = hotel_test)
```

## Fit the prediction model

``` r
cores <- parallel::detectCores()
cores
```

    FALSE [1] 8

It takes 18.9s to run the model on single core It takes 5.9s to run the
model on an 8 core machine using `num.threads = cores`

``` r
start_time <- Sys.time()

hotel_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>%
  fit(children ~ ., data = hotel_train_clean)

end_time <- Sys.time()

end_time - start_time
```

    FALSE Time difference of 18.68943 secs

## Model performance

``` r
hotel_ranger %>%
  predict(hotel_test_clean) %>%
  bind_cols(hotel_test_clean) %>%
  metrics(truth = children, estimate = .pred_class)
```

    FALSE # A tibble: 2 x 3
    FALSE   .metric  .estimator .estimate
    FALSE   <chr>    <chr>          <dbl>
    FALSE 1 accuracy binary         0.943
    FALSE 2 kap      binary         0.483

## Reference

  - An R community blog for a gentle introduction:
    <https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/>
  - Tidymodels official tutorial: <https://www.tidymodels.org/start/>
  - Tidy Modeling with R: <https://www.tmwr.org/>
