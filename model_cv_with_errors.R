library(fpp3)
library(tsibbledata)
library(TSstudio)

cyclical_features <- function(data, col_name = "quarter") { # col_name is now optional
  # Check if the input data is NULL
  if (is.null(data)) {
    warning("Input data is NULL. Returning NULL.")
    return(NULL) # Return NULL, not a dataframe with NA values.
  }
  
  # Determine the type of input data and convert to numeric quarter
  if (inherits(data, "Date") || inherits(data, "POSIXct")) {
    # Extract quarter from date objects
    quarter_num <- quarter(data)
  } else if (is.numeric(data)) {
    # Assume numeric data represents the quarter (1-4)
    quarter_num <- data
    # Validate that the numeric input is within the valid range for quarters.
    if (any(quarter_num < 1 | quarter_num > 4, na.rm = TRUE)) {
      warning("Numeric input contains values outside the range 1-4, which are invalid for quarters. Returning NA for those values.")
      # No need to stop, just return NA for invalid values
    }
  } else {
    stop("Input data must be of class Date, POSIXct, or numeric.")
  }
  
  # Calculate sine and cosine transformations
  quarter_rad <- (quarter_num - 1) * 2 * pi / 4  # Convert quarter to radians, handle 0-based indexing
  quarter_sin <- sin(quarter_rad)
  quarter_cos <- cos(quarter_rad)
  
  # Create the data frame.  Important: use col_name
  df <- data.frame(
    !!sym(paste0(col_name, "_sin")) := quarter_sin, # Use := for assignment within the data.frame
    !!sym(paste0(col_name, "_cos")) := quarter_cos
  )
  return(df)
}

train_data %>% 
  mutate(
    quart = lubridate::quarter(Quarter)
  ) %>% 
  mutate(
    sin_res = sin((quart - 1) * 2 * pi / 4),
    cos_res = cos((quart - 1) * 2 * pi / 4)
  )

cyclical_features(
  train_data %>% mutate(Quarter = as_date(Quarter)), col_name = "Quarter"
)

train_data %>% 
  # select(Consumption ~ Income + Savings) %>% 
  stretch_tsibble(.init = 180, .step=1) -> expanding_window_cv

  # cyclic_encoding(times, c("day", "week", "month"))
  

fit_model <- expanding_window_cv %>% 
  model(
    ARIMA(Consumption ~ Income + lag(Income) + Savings + lag(Savings) + Unemployment + lag(Consumption)),
    ARIMA(Consumption ~ Income + Savings + Unemployment + lag(Consumption)),
    ARIMA(
      Consumption ~ lag(Income) + lag(Unemployment) + lag(Savings) + lag(Consumption)
    ),
    ARIMA(
      Consumption ~ lag(Production) + Income + 
        Savings + Unemployment + lag(Income)
    ),
    ETS(Consumption),
    THETA(Consumption),
    TSLM(Consumption ~ trend())
  )

test <- new_data(expanding_window_cv, n=5) %>%
  # Add in covariates from corresponding month
  left_join(train_data, by="Quarter")

fc <- forecast(fit_model, new_data = test) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  as_fable(response = "Consumption", distribution="Consumption")

fc %>% 
  accuracy(train_data, by=c("h",".model")) %>% 
  group_by(.model) %>% 
  mutate(indeks = row_number()) %>% 
  ungroup() %>% 
  ggplot(aes(x = indeks, y = MAPE)) +
  geom_line() +
  facet_wrap(. ~ .model, scales = "free_y")

