library(fpp3)

# 8.5

help("global_economy")
argentina <- global_economy %>%
  filter(Country=="Argentina")

argentina %>%
  autoplot(Exports) +
  labs(y="% of GDP", title="Argentina Exports")

argentina %>%
  autoplot(Exports) +
  autolayer(argentina, GDP/10E9, color = "red") + # GDP in 10 billion units
  labs(title="Argentina Exports and Growth")


argentina_fit_ANN <- argentina %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))

argentina_fc_ANN <- argentina_fit_ANN %>%
  forecast(h = 10)

argentina_fc_ANN %>%
  autoplot(argentina) +
  labs(y="% of GDP", title="Argentina Exports", subtitle = "ETS(A,N,N)")

accuracy(argentina_fit_ANN)

argentina_fit_AAN <- argentina %>%
  model(ETS(Exports ~ error("A") + trend("A") + season("N")))

argentina_fc_AAN <- argentina_fit_AAN %>%
  forecast(h = 10)

argentina_fc_AAN %>%
  autoplot(argentina) +
  labs(y="% of GDP", title="Argentina Exports", subtitle = "ETS(A,A,N)")

accuracy(argentina_fit_AAN)

# using HILO
intervals_ets_ann <- argentina_fc_ANN %>%
  hilo(95) %>%
  slice(1) %>%
  pull(`95%`)

intervals_ets_aan <- argentina_fc_AAN %>%
  hilo(95) %>%
  slice(1) %>%
  pull(`95%`)

# USING MANUAL CALC
residuals_ets_ann <- augment(argentina_fit_ANN) %>%
  pull(.resid)

residuals_ets_aan <- augment(argentina_fit_AAN) %>%
  pull(.resid)

std_dev_ann <- sd(residuals_ets_ann, na.rm = TRUE)
std_dev_aan <- sd(residuals_ets_aan, na.rm = TRUE)

first_forecast_ann <- argentina_fc_ANN$.mean[1]
first_forecast_aan <- argentina_fc_AAN$.mean[1]

z_value <- 1.96  # 95% 

lower_ann <- first_forecast_ann - z_value * std_dev_ann
upper_ann <- first_forecast_ann + z_value * std_dev_ann

lower_aan <- first_forecast_aan - z_value * std_dev_aan
upper_aan <- first_forecast_aan + z_value * std_dev_aan

#ANN
print(intervals_ets_ann)
print(paste(lower_ann, upper_ann))

#AAN
print(intervals_ets_aan)
print(paste(lower_aan, upper_aan))

# 8.11

Arrivals_NZ_12_Q3 <- aus_arrivals %>% 
  filter (Origin == "NZ" & (Quarter <= yearquarter("2012 Q3")))

Arrivals_NZ_12_Q3 %>%
  autoplot(Arrivals) +
  labs(y="Arrivals", title="Arrivals to NZ - until 2012 Q3")

Arrivals_NZ_12_Q3_train <- Arrivals_NZ_12_Q3 %>%
  filter(Quarter <= max(Quarter) - 8) # 2 years

Arrivals_NZ_12_Q3_fit <- Arrivals_NZ_12_Q3_train %>%
  model(ETS(Arrivals ~ error("M") +  trend("A") + season("M")) )

Arrivals_NZ_12_Q3_fc <- Arrivals_NZ_12_Q3_fit %>% 
  forecast(h = 16) # 4 years

Arrivals_NZ_12_Q3_fc %>%
  autoplot(aus_arrivals) +
  labs(y="Arrivals", title="Arrivals to NZ - until 2012 Q3 - Forecasted 2010 Q3 - 2014 Q3")



Arrivals_NZ_12_Q3_fit_all <- Arrivals_NZ_12_Q3_train %>%
  model(
    Arrivals_NZ_12_Q3_fit_AUTO = ETS(Arrivals),
    Arrivals_NZ_12_Q3_fit_AUTO_log = ETS(log(Arrivals) ~ error("A") + trend("A") + season("A")),
    Arrivals_NZ_12_Q3_fit_SNAIVE = SNAIVE(Arrivals),
    Arrivals_NZ_12_Q3_fit_stl_decomp_log = decomposition_model(STL(log(Arrivals)), ETS(season_adjust)))

Arrivals_NZ_12_Q3_fc_all <- Arrivals_NZ_12_Q3_fit_all %>%
  forecast(h = 8)

Arrivals_NZ_10_Q4_to_12_Q3 <- aus_arrivals %>% 
  filter(Origin == "NZ", 
         Quarter >= yearquarter("2010 Q4"), 
         Quarter <= yearquarter("2012 Q3"))

autoplot(Arrivals_NZ_12_Q3_fc_all) +
  autolayer(Arrivals_NZ_10_Q4_to_12_Q3, Arrivals) +
  labs(title = "Forecasts for New Zealand Arrivals", y = "Arrivals")

accuracy(Arrivals_NZ_12_Q3_fit_all)

# assume the STL DECOMP is the best model since it has the smallest RMSE
Arrivals_NZ_12_Q3_fit_stl_decomp_log <- Arrivals_NZ_12_Q3_train %>%
  model(decomposition_model(STL(log(Arrivals)), ETS(season_adjust)))

Arrivals_NZ_12_Q3_fit_stl_decomp_log %>%
  gg_tsresiduals()

Arrivals_NZ_12_Q3_cv <- Arrivals_NZ_12_Q3 %>%
  slice(1:(n()-3)) %>% #so data lines up
  stretch_tsibble(.init = 36, .step = 3)

Arrivals_NZ_12_Q3_cv %>%
  model(
    Arrivals_NZ_12_Q3_fit_AUTO_cv = ETS(Arrivals),
    Arrivals_NZ_12_Q3_fit_AUTO_log_cv = ETS(log(Arrivals) ~ error("A") + trend("A") + season("A")),
    Arrivals_NZ_12_Q3_fit_SNAIVE_cv = SNAIVE(Arrivals),
    Arrivals_NZ_12_Q3_fit_stl_decomp_log_cv = decomposition_model(STL(log(Arrivals)), ETS(season_adjust))) %>%
  forecast(h = 3) %>%  
  accuracy(Arrivals_NZ_12_Q3) 


# 8.13

beer_fc_all <- aus_production %>%
  slice(1:(nrow(aus_production)-12)) %>% #remove 3 years
  model(
    ETS = ETS(Beer),
    SNAIVE = SNAIVE(Beer),
    STL_Decomp = decomposition_model(STL(log(Beer)),
                                          ETS(season_adjust))) %>%  
  forecast(h = "3 years")

autoplot(beer_fc_all) +
  autolayer(filter_index(aus_production, "2000 Q1" ~ .),  # For easier viewing of forecasts
    Beer)

beer_fc_all %>% 
  accuracy(aus_production)




drug_subsidies <- PBS %>%
  filter(ATC2 %in% c("A10", "H02")) %>%
  group_by(ATC2) %>%
  summarise(Cost = sum(Cost))

cortico <- drug_subsidies %>%
  filter(ATC2 %in% 'H02')

cortico_guerrero <- cortico %>%
  features(Cost, features = guerrero) %>%
  pull(lambda_guerrero)

cortico_fc <- cortico %>% 
  filter(Month < max(Month) - 36) %>% # back 3 years
  model(
    ETS = ETS(Cost),
    SNAIVE = SNAIVE(Cost),
    STL_Decomp = decomposition_model(STL(box_cox(log(Cost), cortico_guerrero)), ETS(season_adjust))) %>%
  forecast(h = "3 years")

diabetes <- drug_subsidies %>%
  filter(ATC2 %in% 'A10')

diabetes_guerrero <- diabetes %>%
  features(Cost, features = guerrero) %>%
  pull(lambda_guerrero)

diabetes_fc <- diabetes %>%
  filter(Month < max(Month) - 36) %>% # back 3 years
  model(
    ETS = ETS(Cost),
    SNAIVE = SNAIVE(Cost),
    STL_Decomp = decomposition_model(STL(box_cox(log(Cost), diabetes_guerrero)), ETS(season_adjust))) %>%
  forecast(h = "3 years")

drugs_all <- bind_rows(cortico_fc, diabetes_fc)

drugs_all %>%
  autoplot(drug_subsidies)

cortico_fc %>%
  accuracy(cortico)

diabetes_fc %>%
  accuracy(diabetes)


aus_bricks <- aus_production %>% 
  filter(!is.na(Bricks))

bricks_fc_all <- aus_bricks  %>%
  slice(1:(nrow(aus_bricks )-12)) %>% # back 3 years
  model(
    ETS = ETS(Bricks),
    SNAIVE = SNAIVE(Bricks),
    STL_Decomp = decomposition_model(STL(log(Bricks)),
                                          ETS(season_adjust))) %>%  
  forecast(h = "3 years")

autoplot(bricks_fc_all) +
  autolayer(filter_index(aus_bricks, "2000 Q1" ~ .),  # For easier viewing of forecasts
            Bricks)

bricks_fc_all %>%
  accuracy(aus_bricks)


aus_food <- aus_retail %>%
  filter(Industry=="Food retailing") %>%
  summarise(Turnover=sum(Turnover))

food_turnover <- aus_food %>% 
  features(Turnover, features = guerrero) %>%
  pull(lambda_guerrero)

aus_food_fc <- aus_food %>%
  filter(Month < max(Month) - 36) %>% # back 3 years
  model(ETS = ETS(Turnover),
        SNAIVE = SNAIVE(Turnover),
        STL_Decomp = decomposition_model(STL(box_cox(log(Turnover),
                                                     food_turnover)),
                                         ETS(season_adjust))
  ) %>%
  forecast(h = "3 years")

autoplot(aus_food_fc) +
  autolayer(filter_index(aus_food, "2010 Q1" ~ .),  # For easier viewing of forecasts
            Turnover)

aus_food_fc %>%
  accuracy(aus_food)


aus_tourism <- tourism %>%
  summarise(Trips = sum(Trips))

tourism_trips_fit <- aus_tourism %>%
  model(ETS(Trips))

tourism_trips_fc <- tourism_trips_fit %>%
  forecast(h=20)# 5 years

autoplot(tourism_trips_fc) +
  autolayer(aus_tourism, Trips)+
  labs(title = "Australian Tourism Forecast")

gafa_group <- gafa_stock %>%
  group_by(Symbol) %>%
  mutate(trading_day = row_number()) %>%
  ungroup() %>%
  as_tsibble(index = trading_day, regular = TRUE)

gafa_fit <- gafa_group %>%
  model(ETS(Close))

gafa_fc <- gafa_fit %>%
  forecast(h=100) #100 days

autoplot(gafa_fc) +
  autolayer(filter_index(gafa_group, 750 ~ .),  # For easier viewing of forecasts
            Close) +
  labs(title = "GAFA Close Forecast")

Lynx_fit <- pelt %>%
  model(ETS(Lynx))

Lynx_fc <- Lynx_fit %>%
  forecast(h=10) # 10 years

autoplot(Lynx_fc) +
  autolayer(pelt, Lynx)+
  labs(title = "Lynx Pelts Traded Forecast")


