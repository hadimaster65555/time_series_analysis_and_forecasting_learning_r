library(fpp3)
library(tsibbledata)

us_change |>
  pivot_longer(c(Consumption, Income),
               names_to = "var", values_to = "value") |>
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income",
       y = "Quarterly % change")

us_change %>% 
  tail(8) -> test_data

us_change %>% 
  anti_join(test_data) -> train_data

fit <- train_data %>% 
  model(ARIMA(Consumption ~ Income + Savings + Unemployment))

report(fit)

forecast(fit, new_data = test_data) -> forecast_result
  
accuracy(forecast_result, test_data)

forecast_result %>% 
  autoplot(us_change %>% tail(20)) +
  labs(y = "Percentage change")


# us_change %>% 
#   gg_season(y = Consumption, period = 4)
# 
# times <- ymd_hms("2019-01-01 00:00:00") + hours(0:23)
# cyclic_encoding(times, c("day", "week", "month"))
# plot(cyclic_encoding(times, "1d"))
# 
# 
# vic_elec %>% 
#   gg_season(Demand, period = "year")
