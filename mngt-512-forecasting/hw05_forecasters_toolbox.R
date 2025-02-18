library(fpp3)

#5.1

# a.
aus_population <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(Population = Population / 10^6) # in millions

#Plotting to view
# aus_population %>%
#  autoplot(Population/10E6) # in millions

aus_population_fc <- aus_population %>%
  model(RW(Population ~ drift())) %>%
    forecast(h = 3)

aus_population %>%
  autoplot(Population) +
  autolayer(aus_population_fc) +
  labs(title = "Australia Population Forcasted (3 Years Out)",
       y = "People (in millions)")


# b.

aus_bricks <- aus_production %>%
  filter(!is.na(Bricks))

#Plotting to view
# aus_bricks %>% 
#   autoplot(Bricks)

aus_bricks_fc <- aus_bricks %>%
  model(SNAIVE(Bricks)) %>%
  forecast(h = 12)

aus_bricks %>%
  autoplot(Bricks) +
  autolayer(aus_bricks_fc) +
  labs(title = "Australian Brick Production Forcasted (3 Years Out)")

# c.

Lambs_NSW <- aus_livestock %>%
  filter(State == "New South Wales", 
         Animal == "Lambs") %>%
  mutate(Count = Count / 10^6) # in millions

#Plotting to view
# Lambs_NSW %>%
#   autoplot(Count) # in millions 
  
Lambs_NSW_fc <- Lambs_NSW %>%
  model(NAIVE(Count)) %>%
  forecast(h = 24)

Lambs_NSW %>%
  autoplot(Count) + # in millions 
  autolayer(Lambs_NSW_fc) +
  labs(title = "Lambs in New South Wales Forecasted (2 Years Out)",
       y = "Lambs (in millions)")

# D.

#help(hh_budget)
#Plotting to view
# hh_budget %>%
#   autoplot(Wealth)

hh_wealth_fc <- hh_budget %>%
  model(RW(Wealth ~ drift())) %>%
  forecast(h = 5)

hh_budget %>%
  autoplot(Wealth) +
  autolayer(hh_wealth_fc) +
  labs(title = "Household Wealth Forecasted (5 Years Out)")

#E. 

#help(aus_retail)
Takeaway_food <- aus_retail %>%
  filter(Industry == 'Takeaway food services') %>%
  select(Month, Turnover) 

Takeaway_food <- Takeaway_food[,1:2] %>%
  group_by(Month) %>%
  summarise(Turnover = sum(Turnover)) %>%
  as_tsibble(index = Month)

#Plotting to view
# Takeaway_food %>%
#  autoplot(Turnover)

Takeaway_food_fc <- Takeaway_food %>%
  model(RW(Turnover ~ drift())) %>%
  forecast(h = 36)

Takeaway_food %>%
  autoplot(Turnover) +
  autolayer(Takeaway_food_fc) +
  labs(title = "Australian Takeaway Food Turnover Forcasted (3 Years Out)" )




# 5.4

#help(global_economy)
aus_exports <- global_economy %>%
  filter(!is.na(Exports)) %>%
  filter(Country == "Australia")

#aus_exports %>%
#  autoplot(Exports)

aus_exports_fc <- aus_exports %>%
  model(NAIVE(Exports))

aus_exports_fc %>% 
  gg_tsresiduals() +
  ggtitle("Residual Plots for Australian Exports")

aus_exports_fc %>% 
  forecast() %>% 
  autoplot(aus_exports) +
  ggtitle("Australian Exports")



aus_bricks <- aus_production %>%
  filter(!is.na(Bricks))

#aus_bricks %>% 
#  autoplot(Bricks)

aus_bricks_fc <- aus_bricks %>%
  model(SNAIVE(Bricks))

aus_bricks_fc %>% 
  gg_tsresiduals() +
  ggtitle("Residual Plots for Australian Bricks Production")

aus_bricks_fc %>% 
  forecast() %>% 
  autoplot(aus_bricks) +
  ggtitle("Australian Bricks Production")



# 5.10

Takeaway_food <- aus_retail %>%
  filter(Industry == 'Takeaway food services') %>%
  select(Month, Turnover) 

Takeaway_food <- Takeaway_food[,1:2] %>%
  group_by(Month) %>%
  summarise(Turnover = sum(Turnover)) %>%
  as_tsibble(index = Month)

cutoff_date <- yearmonth(max(Takeaway_food$Month)) - 48 # equals 4 years

Takeaway_food_train <- Takeaway_food %>%
  filter(Month < cutoff_date)

Takeaway_food_test <- Takeaway_food %>%
  filter(Month >= cutoff_date)

Takeaway_food_train_fc <- Takeaway_food_train %>%
  model(
#    Mean = MEAN(Turnover), # Not appropriate for this data.
    `Naïve` = NAIVE(Turnover),
    `Seasonal naïve` = SNAIVE(Turnover),
    Drift = RW(Turnover ~ drift())
  ) %>%
  forecast(h = 48)

Takeaway_food_train %>%
  autoplot(Turnover) +
  autolayer(Takeaway_food_train_fc) +
  autolayer(Takeaway_food_test) +
  labs(title = "Australian Takeaway Food Turnover - Comparing a 4 year forecast to actual data" )

accuracy(Takeaway_food_train_fc, Takeaway_food_test)

Takeaway_food_train_fc_drift <- Takeaway_food_train %>%
  model(Drift = RW(Turnover ~ drift()))

Takeaway_food_train_fc_drift %>% 
  gg_tsresiduals() +
  ggtitle("Residual Plots for Australian Takeaway Food Turnover")


# 5.12

gold_coast <- tourism %>%
  filter(Region == "Gold Coast") %>%
  index_by(Quarter) %>% 
  summarise(Total_Trips = sum(Trips)) %>%
  as_tsibble(index = Quarter)

cutoff_date <- yearquarter(max(gold_coast$Quarter))

gc_train_1 <- gold_coast %>% 
  filter(Quarter <= cutoff_date - 4)
gc_train_2 <- gold_coast %>% 
  filter(Quarter <= cutoff_date - 8)
gc_train_3 <- gold_coast %>% 
  filter(Quarter <= cutoff_date - 12)

gc_pseudo_test <- gold_coast %>% 
  filter(Quarter >= cutoff_date - 12)

gc_fc_1 <- gc_train_1%>%
  model(Snaive = SNAIVE(Total_Trips))%>%
  forecast(h = 4)
gc_fc_2 <- gc_train_2%>%
  model(Snaive = SNAIVE(Total_Trips))%>%
  forecast(h = 4)
gc_fc_3 <- gc_train_3%>%
  model(Snaive = SNAIVE(Total_Trips))%>%
  forecast(h = 4)

gc_fc_1 %>%
  accuracy(gold_coast)
gc_fc_2 %>%
  accuracy(gold_coast)
gc_fc_3 %>%
  accuracy(gold_coast)

gold_coast %>%
  autoplot(Total_Trips) +
  autolayer(gc_fc_1, color = "blue") +  
  autolayer(gc_fc_2, color = "red") +  
  autolayer(gc_fc_3, color = "green") + 
  autolayer(gc_pseudo_test) + # this is so i can plot the actual data in front of the forecast.
  labs(
    title = "Trips to Gold Coast with Historical Forecasts From 1, 2, and 3 Years Ago",
    y = "Trips (in Thousands)")
