library(fpp3)

#10.2
fit <- souvenirs %>% 
  mutate (festival = month (Month) == 3 & year (Month) != 1987) %>% 
  model(reg = TSLM(log(Sales) ~ trend() + season () + festival), 
        dynreg = ARIMA(log(Sales) ~ trend() + season() + festival))

tidy(fit) %>% 
  print (n=50)

future_data <- new_data(souvenirs, 36) %>%
  mutate(festival = month(Month) == 3)

fc <- fit %>% forecast(new_data = future_data)

autoplot(fc, souvenirs)

fit %>% select(dynreg) %>% gg_tsresiduals()

fit %>% select(dynreg) %>% 
  augment() %>% 
  features(.innov, ljung_box, lag = 24) %>% # doing 2*season
  print()

fit %>% select(dynreg) %>% 
  augment() %>% 
  ACF(.innov) %>% 
  autoplot()


aus_adj <- aus_accommodation %>%
  mutate(Real_Takings = Takings / (CPI / 100))  # Adjust for inflation

aus_adj %>%
  autoplot(Real_Takings) +
  labs(title = "CPI-Adjusted Accommodation Takings by State",
       y = "Real Takings (Million AUD)",
       x = "Year")

aus_accommodation <- aus_accommodation %>%
  mutate(Quarter = yearquarter(Date))

knot <- yearquarter("2008 Q1")

aus_adj <- aus_accommodation %>%
  mutate(
    trend_1 = as.numeric(Quarter - knot) * (Quarter < knot),
    trend_2 = as.numeric(Quarter - knot) * (Quarter >= knot)
  )

aus_adj <- aus_adj %>%
  mutate(Real_Takings = Takings / (CPI / 100))

fit <- aus_adj %>%
  model(
    dyn_reg = ARIMA(Real_Takings ~ season())
  )

report(fit)

fit %>%
  augment() %>%
  features(.innov, ljung_box, lag = 24)

cpi_fc <- aus_accommodation %>%
  model(ARIMA(CPI)) %>%
  forecast(h = 24)

adjusted_fc <- fit %>%
  forecast(h = 24)

adjusted_fc_tibble <- as_tibble(adjusted_fc)
cpi_fc_tibble <- as_tibble(cpi_fc)

forecasted_takings <- adjusted_fc %>%
  as_tibble() %>%
  mutate(Takings_Forecast = .mean * (cpi_fc$.mean / 100))

combined_data <- bind_rows(
  aus_adj %>% 
    select(State, Date, Real_Takings) %>%
    rename(Takings = Real_Takings) %>%
    mutate(Forecast = "Original"),
  forecasted_takings_tsibble %>%
    select(State, Date, Takings_Forecast) %>%
    rename(Takings = Takings_Forecast) %>%
    mutate(Forecast = "Forecast")
)

combined_data %>%
  ggplot(aes(x = Date, y = Takings, color = Forecast)) +
  geom_line() +
  facet_wrap(~ State) +
  labs(title = "CPI-Adjusted Accommodation Takings: Original and Forecasted",
       y = "Takings (Million AUD)",
       x = "Year") +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red"))





retail <- aus_retail %>%
  filter(`Series ID` == "A3349767W")

lambda_retail <- retail %>%
  features(Turnover, guerrero)

models <- retail %>%
  model(
    Model_K1 = ARIMA(log(Turnover) ~ trend() + fourier(K = 1) +
                         pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)),
    Model_K2 = ARIMA(log(Turnover) ~ trend() + fourier(K = 2) +
                         pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)),
    Model_K3 = ARIMA(log(Turnover) ~ trend() + fourier(K = 3) +
                         pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)),
    Model_K4 = ARIMA(log(Turnover) ~ trend() + fourier(K = 4) +
                         pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)),
    Model_K5 = ARIMA(log(Turnover) ~ trend() + fourier(K = 5) +
                         pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)),
    Model_K6 = ARIMA(log(Turnover) ~ trend() + fourier(K = 6) +
                         pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1))
  )

best_model <- transmute(models, best = Model_K6)

gg_tsresiduals(best_model)


new_models <- retail %>%
  model(
    Model_K6 = ARIMA(log(Turnover) ~ trend() + fourier(K = 6) +
                      pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)),
    ARIMA = ARIMA(log(Turnover)),
    ETS = ETS(Turnover)
  )

fc <- new_models %>%
  forecast(h=24)

autoplot(fc) +
  autolayer(filter(retail, year(Month) > 2012))
