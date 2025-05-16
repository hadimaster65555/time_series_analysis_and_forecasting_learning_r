# load library
library(tidymodels)
library(modeltime)
library(modeltime.resample)
library(tidyverse)
library(timetk)

m750 %>%
  plot_time_series(date, value, .interactive = FALSE)

# Split Data 90/10
splits <- initial_time_split(m750, prop = 0.9)

# arima model
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))

# arima boost
arima_boost_spec <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost")

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num)

model_fit_arima_boosted <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(arima_boost_spec) %>%
  fit(training(splits))

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

model_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

# linear regression
lm_spec <- linear_reg() %>%
  set_engine("lm")

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

model_fit_lm <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(lm_spec) %>%
  fit(training(splits))

# modeltime table
models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_lm,
  model_fit_mars
)

# cross-validation
resamples_tscv <- time_series_cv(
  data        = m750,
  assess      = "3 years",
  # initial     = "6 years",
  skip        = "1 years",
  cumulative = T,
  slice_limit = 4
)

resamples_tscv %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .facet_ncol = 2, .interactive = FALSE)

resamples_fitted <- models_tbl %>%
  modeltime_fit_resamples(
    resamples = resamples_tscv,
    control   = control_resamples(verbose = TRUE)
  )

resamples_fitted %>%
  plot_modeltime_resamples(
    .point_size  = 3, 
    .point_alpha = 0.8,
    .interactive = FALSE
  )
