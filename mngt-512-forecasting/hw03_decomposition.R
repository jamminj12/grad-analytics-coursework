library(fpp3)

## Ch03 EXERCISES ##

# 2.

#original
global_economy %>%
  filter(Country == "United States") %>%
  autoplot(GDP) + 
  labs(title = "United States GDP", y = "$ USD")

#transformed
global_economy %>%
  filter(Country == "United States") %>%
  autoplot(GDP/1000000000000) + # 1 to 1 trillion 
  labs(title= "United States GDP", 
       y = "$ USD (Trillions)") 
# I do not think a transformation is needed for this plot since the increasing trend is easily seen. 
# However, the units on the y-axis were originally in scientific notation, which did not look as clean, so I rescaled the data, which did not change the shape of the original trend. 


#original
aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers",
         State == "Victoria") %>%
  autoplot(Count) +
  labs(title= "Slaughter of Victoria Bulls, Bullocks, and Steers") 
# I do not think a transformation is needed for this data, since the downward trend can be seen with the default scaling.



#original
vic_elec %>% 
  autoplot(Demand) +
  labs(title= "Electriciy Demand for Victoria, AUS",
       y = "Demand (MWh)") 

by_date_vic_elec <- vic_elec %>%
  group_by(Date) %>%
  mutate(Demand = sum(Demand)) %>%
  distinct(Date, Demand)

#transformed
by_date_vic_elec %>%
  mutate(Date = yearmonth(Date)) %>%
  group_by(Date) %>%
  summarise(Demand = sum(Demand)) %>%
  as_tsibble(index = Date) %>%
  autoplot(Demand/1000) + # From MWh to GWh
  labs(title= "Electriciy Demand for Victoria, AUS - Monthly", 
       y = "Demand (GWh)")
# I transformed the data by changing the half-hourly data to monthly data. This helps visualize the seasonal trends thorughout the year.
# I also scaled the Demand value by making it in GWh instead of MWh. 

#original
aus_production %>%
  autoplot(Gas) +
  labs(title = 
         "Australian Quarterly Gas Production")

lambda <- aus_production |>
  features(Gas, features = guerrero) |>
  pull(lambda_guerrero)

#transformed
aus_production |>
  autoplot(box_cox(Gas, lambda)) +
  labs(y = 
         "Transformed gas production",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))

# This example was provided in Section 3.1 of the textbook, so I applied the same Box-Cox transformation they used. 
# The best lambda value using the guerrero feature came to be 0.11



# 3.

canadian_gas %>%
  autoplot(Volume) +
  labs(title = "Canadian Monthly Gas Production")

lambda <- canadian_gas %>%
  features(Volume, features = guerrero) %>%
  pull(lambda_guerrero)

canadian_gas %>%
  autoplot(box_cox(Volume, lambda)) +
  labs(y = "Transformed gas production",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))

# The Box-Cox transformation is not helpful with this data set because the goal of finding the ideal lambda value should eliminate any variation in the size of the seasonal change.  
# Even with the ideal lambda value of 0.58, there is still variation present in the early 1980s time frame.


# 8.

set.seed(54321)
myseries <- aus_retail |>
  filter(`Series ID` == "A3349767W") # The ‘Clothing, footwear and personal accessory retailing’ industry

autoplot(myseries)+
  labs(title = 
         "AUS Retail Turnover - Clothing, footwear and personal accessory retailing")

x11_dcmp <- myseries %>%
  model(x11 = X_13ARIMA_SEATS(Turnover ~ x11())) %>%
  components()

autoplot(x11_dcmp) +
  labs(title = 
         "Decomposition of 'AUS Retail Turnover - Clothing, footwear and personal accessory retailing' using X-11.")

# The biggest features that I noticed when doing the X-11 transformation is the considerable dip in the yearly seasonal peaks of the mid 2000s. This can originally not be seen in the original data. 


# 9.

# a.
# The first noticeable feature of the decomposition is the bery consistent increase of the number of civilian labor force in Australia.
# The decomposition also highlights the seasonality more clearly. The peak increase in the labor force occurring in March, September, and December. However, the scale on the left side of the plot is significantly larger than the original plot, which indicates to me that seasonality may have a much smaller impact on the data set.
# The remainder also highlights a large negative spike in the early 1990s, which seems to indicate a pause in the hiring and a disruption in the seasonality. The remainder column makes this feature much more pronounced than in the first value plot.

# b.
# Yes, the recession is highlighted in the remainder plot where there is a sharp negative spike during that timeframe. 