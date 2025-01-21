library(fpp3)

## Ch02 EXERCISES ##

# 1.

data("aus_production")
help("aus_production")
# Time interval: Quarterly
aus_production|>
  autoplot(Bricks)+
  labs(y = "Bricks (million units)", 
       title = "Australian Clay Brick Production")

data("pelt")
help("pelt")
# Time interval: Yearly
hare_lynx <- pivot_longer(pelt, cols = c(Hare, Lynx), names_to = "Species", values_to = "Pelts")
autoplot(hare_lynx, Pelts)+
  labs(y = "Pelts", 
       title = "Pelts traded by Hudson Bay Company")

data("gafa_stock")
help("gafa_stock")
# Time interval: Daily
gafa_stock|>
  autoplot(Close)+
  labs(y = "Close ($USD)", 
       title = "GAFA stock Price at Close (2014-2018)")

data("vic_elec")
help("vic_elec")
# Time interval: Half-hourly
vic_elec|>
  autoplot(Demand)+
  labs(y = "Demand", 
       title = "Electriciy Demand for Victoria, AUS")

vic_elec_june_2012 <- vic_elec |>
  filter(year(Date) == 2012 & month(Date) == 6)

vic_elec_june_2012|>
  autoplot(Demand)+
  labs(y = "Demand", 
       title = "Electriciy Demand for Victoria, AUS")


# 3.

tute1 <- readr::read_csv("data/tute1.csv")
#View(tute1)

mytimeseries <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)

# Mutate transforms the "Quarter" column into a recognized date format so that it can be used for time-series plotting.

mytimeseries %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

# without facet_grid
mytimeseries %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line()

# pivot_longer allows for the three variable columns from tute1 to be merged into one "name" column, and one "value" column, so that they can be plotted together.
# facet_grid divides the plot into multiple subplots. For this example, it is splitting the plot based on data with the same name and for them to be on their own axes.


# 8. 

# “Total Private” Employed from us_employment
help("us_employment")
data("us_employment")

Total_Private <- us_employment |>
  filter(Title == "Total Private")

autoplot(Total_Private, Employed)+
  labs(y= "Private Employed",
       title = "“Total Private” Employed in US")
gg_season(Total_Private, Employed)+
  labs(y= "Private Employed",
       title = "“Total Private” Employed in US")
gg_subseries(Total_Private, Employed)+
  labs(y= "Private Employed",
       title = "“Total Private” Employed in US")
gg_lag(Total_Private, Employed)+
  labs(y= "Private Employed",
       title = "“Total Private” Employed in US")
Total_Private |>
  ACF(Employed, lag_max = 60) |> # 60 is 5 years
  autoplot() +
  labs(title = "Total number of private employed")

# There is a very noticeable upward trend, and the graph tends to follow the same rate of increase for the entire data set.
# Slight differences can be seen, such as in the 2008 time frame, where the economy went though recession.
# There is seasonality, seen in the gg_season plot, as they repeat the same trend each year and correlate between months.


# Bricks from aus_production
help("aus_production")
data("aus_production")

autoplot(aus_production, Bricks)+
  labs(y = "Bricks (million units)", 
       title = "Australian Clay Brick Production")
gg_season(aus_production, Bricks)+
  labs(y = "Bricks (million units)", 
       title = "Australian Clay Brick Production")
gg_subseries(aus_production, Bricks)+
  labs(y = "Bricks (million units)", 
       title = "Australian Clay Brick Production")
gg_lag(aus_production, Bricks)+
  labs(y = "Bricks (million units)", 
       title = "Australian Clay Brick Production")
aus_production |>
  ACF(Bricks, lag_max = 40) |> # 40 is 10 years
  autoplot() +
  labs(title = "Australian Clay Brick Production")

# There are noticeable periods of seasonality, which is best depicted in the gg_subseries plot. 
# Q1 lags behind the other three quarters in brick production for almost all years recorded.


# Hare from pelt
help("pelt")
data("pelt")

autoplot(pelt, Hare)+
  labs(y = "Hare", 
       title = "Hare pelts traded by Hudson Bay Company")
gg_season(pelt, Hare)+
  labs(y = "Hare", 
       title = "Hare pelts traded by Hudson Bay Company")
gg_subseries(pelt, Hare)+
  labs(y = "Hare", 
       title = "Hare pelts traded by Hudson Bay Company")
gg_lag(pelt, Hare)+
  labs(y = "Hare", 
       title = "Hare pelts traded by Hudson Bay Company")
pelt |>
  ACF(Hare, lag_max = 10) |> # 10 is 10 years
  autoplot() +
  labs(title = "Hare pelts traded by Hudson Bay Company")

# The data experiences a noticeable cyclic trend for the entire data range. 
# The ACF plot highlights that every 5 years, we see a statistically significant negative correlation between data values.
# However, we see that correlation return to positive 10 years later, which suggests the cyclic nature of the data.


# “H02” Cost from PBS
help("PBS")
data("PBS")

H02 <- PBS |>
  filter(ATC2 == "H02")

autoplot(H02, Cost)+
  labs(y = "Cost ($AUD)", 
       title = "Cost of H02 in Australia")
gg_season(H02, Cost)+
  labs(y = "Cost ($AUD)", 
       title = "Cost of H02 in Australia")
gg_subseries(H02, Cost)+
  labs(y = "Cost ($AUD)", 
       title = "Cost of H02 in Australia")

filtCC <- H02|>
  filter(Concession =='Concessional', Type =='Co-payments')
filtCS <- H02|>
  filter(Concession =='Concessional', Type =='Safety net')
filtGC <- H02|>
  filter(Concession =='General', Type =='Co-payments')
filtGS <- H02|>
  filter(Concession =='General', Type =='Safety net')


gg_lag(filtCC, Cost)+
  labs(y = "Cost ($AUD)", 
       title = "Cost of H02 in Australia (Concessional; Co-payments)")
gg_lag(filtCS, Cost)+
  labs(y = "Cost ($AUD)", 
       title = "Cost of H02 in Australia (Concessional; Safety net)")
gg_lag(filtGC, Cost)+
  labs(y = "Cost ($AUD)", 
       title = "Cost of H02 in Australia (General; Co-payments)")
gg_lag(filtGS, Cost)+
  labs(y = "Cost ($AUD)", 
       title = "Cost of H02 in Australia (General; Safety net)")
H02 |>
  ACF(Cost, lag_max = 60) |> # 60 is 5 years
  autoplot() +
  labs(title = "Cost of H02 in Australia")

# us_gasoline
help("us_gasoline")
data("us_gasoline")

autoplot(us_gasoline, Barrels)+
  labs(y = "Barrels (millions)", 
       title = "Barrels of Gasoline Supplied in US")
gg_season(us_gasoline, Barrels)+
  labs(y = "Barrels (millions)", 
       title = "Barrels of Gasoline Supplied in US")
gg_subseries(us_gasoline, Barrels)+
  labs(y = "Barrels (millions)", 
       title = "Barrels of Gasoline Supplied in US")
gg_lag(us_gasoline, Barrels)+
  labs(y = "Barrels (millions)", 
       title = "Barrels of Gasoline Supplied in US")
us_gasoline |>
  ACF(Barrels, lag_max = 52) |> # 52 is 1 year
  autoplot() +
  labs(title = "Barrels of Gasoline Supplied in US")



# 11.

dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))

dgoog |>
  ACF(difference(Close), lag_max = 100) |> # 30 is ~1 month
  autoplot() +
  labs(title = "Google Stock Price")
autoplot(dgoog, difference(Close))+
  labs(y = "Close ($USD)", 
       title = "Google Stock Price")
