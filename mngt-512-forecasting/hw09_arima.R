library(fpp3)

#9.2
amazon_close <- gafa_stock %>%
  filter(Symbol == "AMZN")

amazon_close %>%
  gg_tsdisplay(Close, plot_type='partial') +
  labs(title = "Amazon Stock Price at Close")

amazon_close %>%
  features(Close, unitroot_ndiffs) 

#9.5
A3349767W_data <- aus_retail %>%
  filter(`Series ID`=="A3349767W")

A3349767W_data %>%
  gg_tsdisplay(Turnover, plot_type='partial') +
  labs(title = "Retail Turnover (Series ID = A3349767W)")

lambda <- A3349767W_data %>%
  features(Turnover, features = guerrero) %>%
  pull(lambda_guerrero)

A3349767W_data %>%
  autoplot(box_cox(Turnover,lambda))+
  labs(title = "Transformed Retail Turnover with lambda = 0.083 (Series ID = A3349767W)")

A3349767W_data %>%
  features(box_cox(Turnover, lambda), unitroot_nsdiffs)

A3349767W_data %>%
  mutate(A3349767W_data_diff = difference(box_cox(Turnover,lambda))) %>%
  features(A3349767W_data_diff, unitroot_ndiffs)

A3349767W_data %>%
  mutate(A3349767W_data_diff = difference(box_cox(Turnover,lambda))) %>%
  autoplot(A3349767W_data_diff)

#9.7
#a.
aus_airpassengers %>%
  autoplot(Passengers) +
  labs(title = "Australian Airline Passangers")

aus_airpassengers_fit <- aus_airpassengers %>%
  model(ARIMA(Passengers)) 

report(aus_airpassengers_fit)

aus_airpassengers_fit %>% 
  forecast(h=10) %>%
  autoplot(aus_airpassengers) +
  labs(title = "Australian Airline Passangers - Forcasted 10 years with a ARIMA(0,2,1)")

aus_airpassengers_fit %>% 
  gg_tsresiduals() +
  labs(title = "Residuals for 10 year forcast with ARIMA(0,2,1)")

#c.
aus_airpassengers_fit_c <-aus_airpassengers %>%
  model(ARIMA(Passengers ~ 1 + pdq(0,1,0)))

aus_airpassengers_fit_c %>% 
  forecast(h=10) %>%
  autoplot(aus_airpassengers) +
  labs(title = "Australian Airline Passangers - Forcasted 10 years with a 1 + ARIMA(0,1,0)")

aus_airpassengers_fit_c %>% 
  gg_tsresiduals() +
  labs(title = "Residuals for 10 year forcast with 1 + ARIMA(0,1,0)")

report(aus_airpassengers_fit_c)

#d.
aus_airpassengers_fit_d <- aus_airpassengers %>%
  model(ARIMA(Passengers ~ 0 + pdq(2,1,2)))

aus_airpassengers_fit_d %>% 
  forecast(h=10) %>%
  autoplot(aus_airpassengers) +
  labs(title = "Australian Airline Passangers - Forcasted 10 years with a ARIMA(2,1,2)")

aus_airpassengers_fit_d %>% 
  gg_tsresiduals() +
  labs(title = "Residuals for 10 year forcast with ARIMA(2,1,2)")

report(aus_airpassengers_fit_d)

#e.
aus_airpassengers_fit_e <- aus_airpassengers %>%
  model(ARIMA(Passengers ~ 1 + pdq(0,2,1)))

aus_airpassengers_fit_e %>% 
  forecast(h=10) %>%
  autoplot(aus_airpassengers) +
  labs(title = "Australian Airline Passangers - Forcasted 10 years with a 1 + ARIMA(0,2,1)")

aus_airpassengers_fit_e %>% 
  gg_tsresiduals() +
  labs(title = "Residuals for 10 year forcast with 1 + ARIMA(0,2,1)")

report(aus_airpassengers_fit_e)

#9.9
from_japan <- aus_arrivals %>% 
  filter(Origin=='Japan')

from_japan %>% 
  autoplot(Arrivals) + 
  labs(title = "Australian Arrivals from Japan")

from_japan %>%
  gg_tsdisplay(difference(Arrivals, 4),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

from_japan %>%
  gg_tsdisplay(difference(Arrivals, 4) |> difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

from_japan_fit <- from_japan %>%
  model(ARIMA(Arrivals ~ pdq(0,1,1) + PDQ(1,1,1)))
  
report(from_japan_fit)

from_japan_fit2 <- from_japan %>%
  model(ARIMA(Arrivals))

report(from_japan_fit2)
