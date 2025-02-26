library(fpp3)

# 7.2

#a. 
olympic_running |>  
  ggplot(aes(x = Year, y = Time, colour = Sex)) +  
  geom_line() +  
  labs(title = "Olympic Running Results by Event") +
  facet_wrap(~Length, scales = "free_y")

# The winning time generally decreased for all event lengths over time. However, the finish times seem to be experience less substantial improvement in recent years.
# All of the data is not connected, which highlights missing values from years when the Olympics was not held.

#b.
fit_olympic_running <- olympic_running %>% 
  group_by(Sex, Length) %>%
  model(TSLM(Time ~ trend()))

tidy(fit_olympic_running) %>% 
  print(n = Inf) # This is to view all rows

# The average rate per year decrease of finish time for each event can be derived from the "estimate" column of the fit from the trend row.

#c.
augment(fit_olympic_running) %>%
  ggplot(aes(x = Year, y = .innov, colour = Sex)) +
  geom_line() +
  labs(title = "Residuals of Olympic Running Results by Event") +
  facet_wrap(~Length, scales = "free_y")

# The residual plots for each year appear to be biased and not random. 
# Therefore, it can be assumed that information is still contained in the model, and that a linear regression is not suitable for forecasting this data.

#d. 
fit_olympic_running %>%
  forecast(h = 1) %>% # last data point recorded was 2016, so next data point is 2020
  mutate(PI = hilo(Time, 95)) %>%
  select(-.model) %>%
  print(n = Inf, width = Inf) # printing all rows and columns

# The PI columns outlines the finish times for each event within a 95% confidence interval.
# This forecast is assuming a linear trend, which I mentioned in part a that the running times do not follow a linear trend for the entire data set. 
# This forecast also assumed that the residuals are normally distributed, and that there is no autocorrelation, which I think is not true and had mentioned in part c.


# 7.5
us_gasoline_2004 <- us_gasoline %>%
  filter(year(Week) < 2005)

autoplot(us_gasoline_2004, Barrels)+
  labs(y = "Barrels (millions)", 
       title = "Barrels of Gasoline Supplied in US")

#weekly data, so max k = 26
fourier_model_1 <- us_gasoline_2004 |>
  model(TSLM(Barrels ~ trend() + fourier(K = 1)))
fourier_model_2 <- us_gasoline_2004 |>
  model(TSLM(Barrels ~ trend() + fourier(K = 5)))
fourier_model_3 <- us_gasoline_2004 |>
  model(TSLM(Barrels ~ trend() + fourier(K = 10)))
fourier_model_4 <- us_gasoline_2004 |>
  model(TSLM(Barrels ~ trend() + fourier(K = 15)))
fourier_model_5 <- us_gasoline_2004 |>
  model(TSLM(Barrels ~ trend() + fourier(K = 20)))
fourier_model_6 <- us_gasoline_2004 |>
  model(TSLM(Barrels ~ trend() + fourier(K = 25)))

autoplot(us_gasoline_2004, Barrels)+
  labs(y = "Barrels (millions)", 
       title = "Barrels of Gasoline Supplied in US") +  
  autolayer(fitted(fourier_model_1), series = "K=1", color = "red") +
  autolayer(fitted(fourier_model_2), series = "K=5", color = "green") +
  autolayer(fitted(fourier_model_3), series = "K=10", color = "orange") +
  autolayer(fitted(fourier_model_4), series = "K=15", color = "purple") +
  autolayer(fitted(fourier_model_5), series = "K=20", color = "cyan") +
  autolayer(fitted(fourier_model_6), series = "K=25", color = "magenta")

glance(fourier_model_1) %>%
  select(CV, AICc)
glance(fourier_model_2) %>%
  select(CV, AICc)
glance(fourier_model_3) %>%
  select(CV, AICc)
glance(fourier_model_4) %>%
  select(CV, AICc)
glance(fourier_model_5) %>%
  select(CV, AICc)
glance(fourier_model_6) %>%
  select(CV, AICc)
# Regardless of CV or AICC, fourier_model_3 has the lowest of both values.

gg_tsresiduals(fourier_model_3)

augment(fourier_model_3) %>%
  features(.innov, ljung_box, lag = 52, dof = 16)

# The test displays a p-value that is very low, which means that there is still correlation in the residuals. 

us_gasoline_2005_only <- us_gasoline %>%
  filter(year(Week) == 2005)

fourier_model_3_fc <- fourier_model_3 %>%
  forecast(h = 52)

autoplot(us_gasoline_2005_only, Barrels)+
  labs(y = "Barrels (millions)", 
       title = "Barrels of Gasoline Supplied in US") +  
  autolayer(fourier_model_3_fc, series = "K=10", color = "orange") +
  autolayer(us_gasoline_2005_only, Barrels)
  

#7.6
afghanistan <- global_economy %>%
  filter(Country=="Afghanistan") %>%
  mutate(Population = Population / 10^6) # in millions

afghanistan %>%
  autoplot(Population) +
  labs(title= "Afghanistan Population",
       y = "Population (in millions)")

# Yes, the Soviet-Afghan war happened during the 1980s, and a fairly considerable drop in population occurred during that time in Afghanistan.

afghanistan_trend_linear <- afghanistan %>%
  model(TSLM(Population ~ trend())) %>%
  report()

afghanistan_trend_piecewise <- afghanistan %>%
  model(TSLM(Population ~ trend(knots = c(1980, 1989)))) %>%
  report()


autoplot(afghanistan, Population) +
  labs(title = "Afghanistan Population with Linear and Picewise (1980, 1989) Trends",
       y = "Population (in millions)") +
  autolayer(fitted(afghanistan_trend_linear), series = "Linear Trend", color = "blue") +
  autolayer(fitted(afghanistan_trend_piecewise), series = "Piecewise Trend", color = "red")

afghanistan_forecast_linear <- forecast(afghanistan_trend_linear, h = 5)
afghanistan_forecast_piecewise <- forecast(afghanistan_trend_piecewise, h = 5)

autoplot(afghanistan, Population) +
  labs(title = "Afghanistan Population with Forecasts",
       y = "Population (in millions)") +
  autolayer(fitted(afghanistan_trend_linear), series = "Linear Trend", color = "blue") +
  autolayer(fitted(afghanistan_trend_piecewise), series = "Piecewise Trend", color = "red") +
  autolayer(afghanistan_forecast_linear, series = "Linear Forecast", color = "blue") +
  autolayer(afghanistan_forecast_piecewise, series = "Piecewise Forecast", color = "red")
