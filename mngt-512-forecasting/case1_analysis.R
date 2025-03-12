library(fpp3)

MEV_raw_data <- readr::read_csv("data/Case 1 data students.csv")

###################################################################################

# Req 2: Clean and preprocess the data
MEV_data <- MEV_raw_data %>%
  mutate(
    ReceiveDate = as.Date(ReceiveDate, format = "%m/%d/%Y"),
    ApprovedDate = as.Date(ApprovedDate, format = "%m/%d/%Y"),
    CloseDate = as.Date(CloseDate, format = "%m/%d/%Y"),
    ApprovalDelayWeeks = ApprovalWeek - ReceiveWeek,
    ClosedWeek = floor(as.numeric(difftime(CloseDate, as.Date("2008-01-01"), units = "weeks"))) + 1 #found this manipulation on substack. Could have easily made my own column in excel, but wanted to have a "clean" original dataset.
  ) %>%
  filter(!is.na(ReceiveWeek),
         ApprovalDelayWeeks >= 0 | is.na(ApprovalDelayWeeks)) # Removing data points that gave me negative approval delay. 

# using this tsibble to match all future data points on a single "Week" column
all_weeks <- tsibble(
  Week = seq(min(MEV_data$ReceiveWeek, na.rm = TRUE), 109, by = 1), # Week 109 = (January 30, 2010)
  index = Week
)

approved_data <- MEV_data %>%
  filter(Status == "Approved") %>%
  group_by(ApprovalWeek) %>%
  summarise(
    NewApproved = n(),
    EstdValueApproved = sum(EstdValue, na.rm = TRUE)
  ) %>%
  rename(Week = ApprovalWeek) 

closed_counts <- MEV_data %>%
  filter(Status == "Closed") %>%
  group_by(ClosedWeek) %>%
  summarise(NewClosed = n()) %>%
  rename(Week = ClosedWeek)

received_counts <- MEV_data %>%
  group_by(ReceiveWeek) %>%
  summarise(NewReceived = n()) %>%
  rename(Week = ReceiveWeek)

weekly_data <- all_weeks %>%
  left_join(received_counts, by = "Week") %>%
  left_join(approved_data, by = "Week") %>%
  left_join(closed_counts, by = "Week") %>%
  mutate(
    NewReceived = replace(NewReceived, is.na(NewReceived), 0), 
    NewApproved = replace(NewApproved, is.na(NewApproved), 0),
    NewClosed = replace(NewClosed, is.na(NewClosed), 0),
    EstdValueApproved = replace(EstdValueApproved, is.na(EstdValueApproved), 0),
    CumReceived = cumsum(NewReceived), # function to aggregate a total number over the weeks
    CumApproved = cumsum(NewApproved),
    CumClosed = cumsum(NewClosed),
    CurrentActive = CumReceived - CumApproved - CumClosed
  ) %>%
  as_tsibble(index = Week)

###################################################################################

# Req 1: Evaluate plots of the time series
weekly_data_long <- weekly_data %>%
  pivot_longer(cols = c(NewApproved, EstdValueApproved), 
               names_to = "Metric", 
               values_to = "Value")

ggplot(weekly_data, aes(x = Week, y = NewApproved)) +
  geom_line() +
  labs(title = "Weekly Approved Cases",
       x = "Week", y = "Count") 

ggplot(weekly_data, aes(x = Week, y = EstdValueApproved)) +
  geom_line(color = "red") +
  labs(title = "Weekly Estimated Value of Approvals",
       x = "Week", y = "Estimated Value")

new_count_long <- weekly_data %>%
  select(Week, NewReceived, NewApproved, NewClosed) %>%
  pivot_longer(cols = c(NewReceived, NewApproved, NewClosed),
               names_to = "Status",
               values_to = "Count")

ggplot(new_count_long, aes(x = Week, y = Count, color = Status)) +
  geom_line() +
  labs(title = "Case Status by Week",
       x = "Week",
       y = "Count",
       color = "Status") +
  scale_color_manual(values = c("NewReceived" = "blue",
                                "NewApproved" = "red", 
                                "NewClosed" = "black"))

status_long <- weekly_data %>%
  select(Week, CurrentActive, CumApproved, CumClosed, CumReceived) %>%
  pivot_longer(cols = c(CurrentActive, CumApproved, CumClosed, CumReceived),
               names_to = "Status",
               values_to = "Count")

ggplot(status_long, aes(x = Week, y = Count, color = Status)) +
  geom_line() +
  labs(title = "Cumulative Counts of Cases Over Weeks",
       x = "Week",
       y = "Count",
       color = "Status") +
  scale_color_manual(values = c("CurrentActive" = "green", 
                                "CumApproved" = "blue", 
                                "CumClosed" = "red", 
                                "CumReceived" = "black"))

###################################################################################

# Req 3: Create training and test sets
train <- weekly_data %>% filter(Week <= 91)
test <- weekly_data %>% filter(Week > 91)

###################################################################################

# Req 4: Evaluate for seasonality and trend
train %>%
  ACF(EstdValueApproved) %>%
  autoplot()
  
decomp <- train %>%
  model(STL(EstdValueApproved ~ trend())) %>% # I did not see any seasonality, so no seasonal component recorded.
  components()

autoplot(decomp) +
  labs(title = "STL Decomposition of Estimated Value Approved")

###################################################################################

# Req 5: Create forecasts
# a) Benchmark methods
fit_bench <- train %>%
  model(
    Mean = MEAN(EstdValueApproved),
    Naive = NAIVE(EstdValueApproved),
    Drift = RW(EstdValueApproved ~ drift())
  ) #Snaive not included due to no seasonality seen

fc_bench <- fit_bench %>% 
  forecast(h = 31)  # Forecast to week 122 (31 weeks from week 91); Week 122 = (April 25, 2010)

# b) Time series regression
fit_reg <- train %>%
  model(
    Linear = TSLM(EstdValueApproved ~ trend()),
    Piecewise = TSLM(EstdValueApproved ~ trend(knots = c(30, 45, 75))) #from visual inspection
  ) #Exponential not included due to the graph not experiencing a "leveling off" or exponential growth.

fc_reg <- fit_reg %>% 
  forecast(h = 31)

autoplot(train, EstdValueApproved) +
  autolayer(fc_bench, level = NULL) +
  autolayer(fc_reg, level = NULL) +
  labs(title = "Forecasts for Estimated Value Approved",
       y = "Estimated Value", color = "Method") +
  scale_color_manual(values = c("Mean" = "blue", 
                                "Naive" = "red", 
                                "Drift" = "green",
                                "Linear" = "purple", 
                                "Piecewise" = "orange"))

###################################################################################

# Req 6: Evaluate residuals
# Benchmark models
fit_bench %>% 
  select(Mean) %>% 
  gg_tsresiduals()

fit_bench %>% 
  select(Naive) %>% 
  gg_tsresiduals()

fit_bench %>% 
  select(Drift) %>% 
  gg_tsresiduals()

# Regression models
fit_reg %>% 
  select(Linear) %>% 
  gg_tsresiduals()

fit_reg %>% 
  select(Piecewise) %>% 
  gg_tsresiduals()

# Ljung-Box test for autocorrelation
fit_bench %>%
  augment() %>%
  group_by(.model) %>%
  features(.innov, ljung_box, lag = 10)

fit_reg %>%
  augment() %>%
  group_by(.model) %>%
  features(.innov, ljung_box, lag = 10)

###################################################################################

# Req 7 & 8: Produce forecasts and evaluate accuracy
accuracy_bench <- accuracy(fc_bench, test)
accuracy_reg <- accuracy(fc_reg, test)
print(accuracy_bench)
print(accuracy_reg)

###################################################################################

# Building off of Best Forecast for Active Cases
facility_summary <- MEV_data %>%
  filter(Status == "Approved") %>%
  group_by(Facility, TranType) %>%
  summarise(
    TotalApproved = n(),
    AvgEstdValue = mean(EstdValue, na.rm = TRUE),
    TotalEstdValue = sum(EstdValue, na.rm = TRUE),
    AvgApprovalDelay = mean(ApprovalDelayWeeks, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(facility_summary, aes(x = Facility, y = TotalEstdValue, fill = TranType)) +
  geom_bar(stat = "identity", position = "dodge") + #bar chart in R
  labs(title = "Total Estimated Value by Facility and Transaction Type",
       x = "Facility", y = "Total Estimated Value", fill = "Transaction Type")

# Since Knox does not have any current approved cases up until this point, I am using the global delay.
global_avg_delay <- MEV_data %>%
  filter(Status == "Approved") %>%
  summarise(GlobalAvgDelay = mean(ApprovalDelayWeeks, na.rm = TRUE)) %>%
  pull(GlobalAvgDelay)

approval_rates_delays <- MEV_data %>%
  group_by(Facility) %>%
  summarise(
    TotalCases = n(),
    ApprovedCases = sum(Status == "Approved"),
    ApprovalRate = ApprovedCases / TotalCases,
    AvgApprovalDelay = if_else(sum(Status == "Approved") > 0, 
                               mean(ApprovalDelayWeeks[Status == "Approved"], na.rm = TRUE), # I am using this if_else statement find the average delay by facility.
                               global_avg_delay) # Since Knox does not have any current approved cases up until this point, I am using the global delay.
  )

active_cases <- MEV_data %>%
  filter(Status == "Active") %>%
  select(Facility, ReceiveWeek, EstdValue) %>%
  mutate(
    DelayDays = approval_rates_delays$AvgApprovalDelay[match(Facility, approval_rates_delays$Facility)] * 7, # adding this as a datapoint for future manipulation
    ExpectedApprovalWeek = ReceiveWeek + round(DelayDays / 7) # could easily have just added delay week to receive week. 
  ) %>%
  left_join(approval_rates_delays %>% 
              select(Facility, ApprovalRate), by = "Facility") %>%
  mutate(
    AdjustedEstdValue = EstdValue * ApprovalRate
  )

piecewise_fc <- fc_reg %>% # taking the mean of my "best" model into a variable
  filter(.model == "Piecewise") %>%
  as_tibble() %>%
  select(Week, .mean) %>%
  rename(PiecewiseValue = .mean)

forecast_weeks <- tibble(Week = seq(92, 122, by = 1)) %>% #sequencing like I did for all_weeks
  left_join(piecewise_fc, by = "Week") %>%
  mutate(PiecewiseValue = replace(PiecewiseValue, is.na(PiecewiseValue), 0))

active_adjustment <- active_cases %>% # adjusting based on the unique delay by facility
  group_by(ExpectedApprovalWeek) %>%
  summarise(ActiveEstdValue = sum(AdjustedEstdValue, na.rm = TRUE)) %>%
  rename(Week = ExpectedApprovalWeek)

adjusted_forecast <- forecast_weeks %>%
  left_join(active_adjustment, by = "Week") %>%
  mutate(
    ActiveEstdValue = replace(ActiveEstdValue, is.na(ActiveEstdValue), 0), 
    AdjustedPiecewise = PiecewiseValue + ActiveEstdValue
  )

historical_approval <- weekly_data %>% # Defining this again, but just denoting it as historical data.
  select(Week, EstdValueApproved) %>%
  rename(EstdValue = EstdValueApproved) %>%
  mutate(Type = "Historical Approval")

forecast_plot_data <- adjusted_forecast %>%
  select(Week, PiecewiseValue, AdjustedPiecewise) %>%
  rename(EstdValue = AdjustedPiecewise) %>%
  mutate(Type = "Adjusted Piecewise with Active Cases") %>% # So I don't confuse with the actual Piecewise forecast
  bind_rows(historical_approval)

ggplot(forecast_plot_data, aes(x = Week, y = EstdValue, color = Type)) +
  geom_line() +
  labs(title = "Historical Approvals and Adjusted Piecewise Forecast with Active Cases",
       x = "Week", y = "Estimated Value", color = "Data Type") +
  scale_color_manual(values = c("Historical Approval" = "red", "Adjusted Piecewise with Active Cases" = "purple"))
