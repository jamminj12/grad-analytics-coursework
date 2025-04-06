library(fpp3)

MEV_raw_data <- readr::read_csv("data/Case 1 data students.csv")

########################################################################################################

# Req 1: From Case 1 Submission
MEV_data <- MEV_raw_data %>%
  mutate(
    ReceiveDate = as.Date(ReceiveDate, format = "%m/%d/%Y"),
    ApprovedDate = as.Date(ApprovedDate, format = "%m/%d/%Y"),
    CloseDate = as.Date(CloseDate, format = "%m/%d/%Y"),
    ApprovalDelayWeeks = ApprovalWeek - ReceiveWeek,
    ClosedWeek = floor(as.numeric(difftime(CloseDate, as.Date("2008-01-01"), units = "weeks"))) + 1
  ) %>%
  filter(!is.na(ReceiveWeek),
         ApprovalDelayWeeks >= 0 | is.na(ApprovalDelayWeeks))

all_weeks <- tsibble(
  Week = seq(min(MEV_data$ReceiveWeek, na.rm = TRUE), 122, by = 1),
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
    CumReceived = cumsum(NewReceived),
    CumApproved = cumsum(NewApproved),
    CumClosed = cumsum(NewClosed),
    CurrentActive = CumReceived - CumApproved - CumClosed
  ) %>%
  as_tsibble(index = Week) %>%
  filter(Week <= 109)

ggplot(weekly_data, aes(x = Week, y = EstdValueApproved)) +
  geom_line(color = "red") +
  labs(title = "Weekly Estimated Value of Approvals",
       x = "Week", y = "Estimated Value")

########################################################################################################
# Transform the Data

lambda_opt <- weekly_data %>%
  features(EstdValueApproved, guerrero) %>%
  pull(lambda_guerrero)

print(lambda_opt)

transformed_data <- weekly_data %>%
  mutate(EstdValueApproved_transformed = box_cox(EstdValueApproved, lambda_opt))

ggplot(transformed_data, aes(x = Week, y = EstdValueApproved_transformed)) +
  geom_line() +
  labs(title = "Box-Cox Transformed Estimated Value",
       x = "Week",
       y = "Transformed Estimated Value")

########################################################################################################

# Req 2: Training and Test Sets
train <- weekly_data %>% filter(Week <= 91)
test <- weekly_data %>% filter(Week > 91 & Week <= 109) # making sure to denote ending at week 109 because i have weeks going out to 122 with no data for this set

# External Predictor: U.S. Unemployment Rate; https://data.bls.gov/timeseries/LNS14000000
# Monthly data from Jan 2008 to Dec 2010. Note: actual data only spans May 5, 2008, to January 30, 2010
unemp_data <- tibble(
  YearMonth = c(
    "2008-01", "2008-02", "2008-03", "2008-04", "2008-05", "2008-06",
    "2008-07", "2008-08", "2008-09", "2008-10", "2008-11", "2008-12",
    "2009-01", "2009-02", "2009-03", "2009-04", "2009-05", "2009-06",
    "2009-07", "2009-08", "2009-09", "2009-10", "2009-11", "2009-12",
    "2010-01", "2010-02", "2010-03", "2010-04", "2010-05", "2010-06",
    "2010-07", "2010-08", "2010-09", "2010-10", "2010-11", "2010-12"
  ),
  UnemploymentRate = c(
    5.0, 4.9, 5.1, 5.0, 5.4, 5.6, 5.8, 6.1, 6.1, 6.5, 6.8, 7.3, #2008
    7.8, 8.3, 8.7, 9.0, 9.4, 9.5, 9.5, 9.6, 9.8, 10.0, 9.9, 9.9, #2009
    9.8, 9.8, 9.9, 9.9, 9.6, 9.4, 9.4, 9.5, 9.5, 9.4, 9.8, 9.3 #2010
  ),
  Week = seq(1, by = 4.34524, length.out = 36) #https://www.google.com/search?q=month+to+weeks&sca_esv=668bf940e0a90fce&rlz=1C1GCEU_enUS1066US1067&sxsrf=AHTn8zpCR4PE8c0UKj6P1dG_--05-nL9Rw%3A1743207604930&ei=tDznZ8e8OMe8p84PsbrRoAM&ved=0ahUKEwjH5r6wgq6MAxVH3skDHTFdFDQQ4dUDCBA&uact=5&oq=month+to+weeks&gs_lp=Egxnd3Mtd2l6LXNlcnAiDm1vbnRoIHRvIHdlZWtzMgsQABiABBiRAhiKBTIFEAAYgAQyCxAAGIAEGJECGIoFMgYQABgWGB4yBhAAGBYYHjIGEAAYFhgeMgYQABgWGB4yBhAAGBYYHjIGEAAYFhgeMgYQABgWGB5IxhNQAFjVEnABeAGQAQCYAbMBoAHJC6oBBDEwLjW4AQPIAQD4AQGYAhCgAq0MwgIEECMYJ8ICChAjGIAEGCcYigXCAg4QABiABBixAxiDARiKBcICDhAuGIAEGLEDGIMBGIoFwgIOEC4YgAQYsQMY0QMYxwHCAgoQABiABBhDGIoFwgIIEAAYgAQYsQPCAg0QABiABBixAxhDGIoFwgINEAAYgAQYsQMYFBiHAsICERAuGIAEGJIDGMcBGI4FGK8BwgIIEAAYgAQYkgPCAggQABiABBjJA8ICChAAGIAEGBQYhwKYAwCSBwM5LjegB-B8&sclient=gws-wiz-serp
)

unemp_interpolated <- tibble(Week = 1:109) %>%
  mutate(UnemploymentRate = approx(unemp_data$Week, unemp_data$UnemploymentRate, xout = Week, rule = 2)$y) %>%
  as_tsibble(index = Week)

combined_data <- weekly_data %>%
  left_join(unemp_interpolated, by = "Week") %>%
  filter(Week <= 109)

scale_factor_for_plot <- max(combined_data$EstdValueApproved, na.rm = TRUE) / max(combined_data$UnemploymentRate, na.rm = TRUE)

unemp_value_plot <- ggplot(combined_data, aes(x = Week)) +
  geom_line(aes(y = EstdValueApproved, color = "Estimated Value")) +
  geom_line(aes(y = UnemploymentRate * scale_factor_for_plot, color = "Unemployment Rate")) +
  scale_y_continuous(
    name = "Estimated Value ($)",
    sec.axis = sec_axis(~ . / scale_factor_for_plot, name = "Unemployment Rate (%)") # using this function for a second axis
  ) +
  labs(title = "Actual Estimated Value vs Unemployment Rate", x = "Week", color = "Variable") +
  scale_color_manual(values = c("Estimated Value" = "black", "Unemployment Rate" = "purple")) +
  theme(legend.position = "bottom")

unemp_value_plot

train <- train %>% 
  select(-contains("UnemploymentRate")) %>% # using this to fix the UnemploymentRate.x and UnemploymentRate.y thing
  left_join(unemp_interpolated, by = "Week")
test <- test %>% 
  select(-contains("UnemploymentRate")) %>% 
  left_join(unemp_interpolated, by = "Week")

########################################################################################################

# ACF/PACF plot to determine if difference is needed
weekly_data %>% 
  features(EstdValueApproved, unitroot_kpss)

weekly_data %>% 
  features(EstdValueApproved, unitroot_ndiffs)

weekly_data %>% 
  gg_tsdisplay(EstdValueApproved, plot_type='partial')

weekly_data %>% 
  gg_tsdisplay(difference(EstdValueApproved), plot_type='partial')

########################################################################################################
# Req 3: Create models from material learned in chapters 8 – 10. Also adding in my TSLM fit from Case 1 with Box-Cox transformation
fit_ETS <- train %>%
  model(AutoETS = ETS(box_cox(EstdValueApproved, lambda_opt)))
report(fit_ETS)
fit_holt <- train %>%
  model(Holt = ETS(box_cox(EstdValueApproved, lambda_opt) ~ error("A") + trend("A") + season("N")))
fit_autoarima <- train %>%
  model(AutoARIMA = ARIMA(box_cox(EstdValueApproved, lambda_opt)))
report(fit_autoarima)
fit_arima510 <- train %>%
  model(ARIMA510 = ARIMA(box_cox(EstdValueApproved, lambda_opt) ~ pdq(5,1,0)))
report(fit_arima510)
fit_arima014 <- train %>%
  model(ARIMA014 = ARIMA(box_cox(EstdValueApproved, lambda_opt) ~ pdq(0,1,4)))
report(fit_arima014)
fit_dynreg <- train %>%
  model(DynReg = ARIMA(box_cox(EstdValueApproved, lambda_opt) ~ UnemploymentRate + pdq(d = 1)))
report(fit_dynreg)
fit_piecewise <- train %>%
  model(Piecewise = TSLM(box_cox(EstdValueApproved, lambda_opt) ~ trend(knots = c(30, 45, 75))))

########################################################################################################

# Req 4: Evaluate Models
# 4.1: Residual Analysis 
gg_tsresiduals(fit_ETS) + ggtitle("AutoETS Residuals")
gg_tsresiduals(fit_holt) + ggtitle("Holt Residuals")
gg_tsresiduals(fit_autoarima) + ggtitle("AutoARIMA Residuals")
gg_tsresiduals(fit_arima510) + ggtitle("ARIMA(5,1,0) Residuals")
gg_tsresiduals(fit_arima014) + ggtitle("ARIMA(0,1,4) Residuals")
gg_tsresiduals(fit_dynreg) + ggtitle("DynReg Residuals")
gg_tsresiduals(fit_piecewise) + ggtitle("Piecewise Residuals")

# 4.2: Ljung-Box Test
ljung_box_results <- bind_rows(
  fit_ETS %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "AutoETS"),
  fit_holt %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "Holt"),
  fit_autoarima %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "AutoARIMA"),
  fit_arima510 %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "ARIMA510"),
  fit_arima014 %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "ARIMA014"),
  fit_dynreg %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "DynReg"),
  fit_piecewise %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "Piecewise")
) %>%
  select(Model, lb_pvalue)

ljung_box_results

# Going to assume we do not know future unemployment rate
last_unemp_rate <- unemp_interpolated %>%
  filter(Week == 109) %>%
  pull(UnemploymentRate)

future_data <- new_data(train, 31) %>%
  mutate(
    Week = seq(92, 122, by = 1),
    UnemploymentRate = if_else(
      Week <= 109,
      approx(unemp_data$Week, unemp_data$UnemploymentRate, xout = Week, rule = 2)$y,
      last_unemp_rate # assuming we don't know future unemployment rates
    )
  ) %>%
  as_tsibble(index = Week)

########################################################################################################

# Req 5: Forecasts
fc_ETS <- fit_ETS %>% forecast(h = 31)
fc_holt <- fit_holt %>% forecast(h = 31)
fc_autoarima <- fit_autoarima %>% forecast(h = 31)
fc_arima510 <- fit_arima510 %>% forecast(h = 31)
fc_arima014 <- fit_arima014 %>% forecast(h = 31)
fc_dynreg <- fit_dynreg %>% forecast(new_data = future_data)
fc_piecewise <- fit_piecewise %>% forecast(h = 31)

# Accuracy Metrics
RMSE_ETS <- fc_ETS %>% accuracy(test) %>% select(.model, RMSE)
RMSE_holt <- fc_holt %>% accuracy(test) %>% select(.model, RMSE)
RMSE_autoarima <- fc_autoarima %>% accuracy(test) %>% select(.model, RMSE)
RMSE_arima510 <- fc_arima510 %>% accuracy(test) %>% select(.model, RMSE)
RMSE_arima014 <- fc_arima014 %>% accuracy(test) %>% select(.model, RMSE)
RMSE_dynreg <- fc_dynreg %>% accuracy(test) %>% select(.model, RMSE)
RMSE_piecewise <- fc_piecewise %>% accuracy(test) %>% select(.model, RMSE)

AICc_ETS <- fit_ETS %>% glance() %>% select(.model, AICc)
AICc_holt <- fit_holt %>% glance() %>% select(.model, AICc)
AICc_autoarima <- fit_autoarima %>% glance() %>% select(.model, AICc)
AICc_arima510 <- fit_arima510 %>% glance() %>% select(.model, AICc)
AICc_arima014 <- fit_arima014 %>% glance() %>% select(.model, AICc)
AICc_dynreg <- fit_dynreg %>% glance() %>% select(.model, AICc)
AICc_piecewise <- fit_piecewise %>% glance() %>% select(.model, AICc)

accuracy_metrics <- bind_rows(RMSE_ETS, RMSE_holt, RMSE_autoarima, RMSE_arima510, RMSE_arima014, RMSE_dynreg, RMSE_piecewise) %>%
  left_join(bind_rows(AICc_ETS, AICc_holt, AICc_autoarima, AICc_arima510, AICc_arima014, AICc_dynreg, AICc_piecewise), by = ".model")

accuracy_metrics

forecast_plot <- ggplot() +
  autolayer(train, EstdValueApproved, color = "black") +
  autolayer(fc_ETS, color = "green", level = NULL) +
  autolayer(fc_arima510, color = "yellow", level = NULL) +
  autolayer(fc_dynreg, color = "red", level = NULL) +
  autolayer(fc_piecewise, color = "blue", level = NULL) +
  autolayer(test, EstdValueApproved, color = "black") +
  labs(title = "Forecasts vs Actual Estimated Value", y = "Estimated Value", color = "Model") +
  scale_color_manual(values = c("AutoETS" = "green", "ARIMA510" = "yellow",
                                "DynReg" = "red", "Piecewise" = "blue", 
                                "Actual" = "black"))

forecast_plot

########################################################################################################

# Req 6: Preferred Model
preferred_model <- accuracy_metrics %>%
  arrange(RMSE)

preferred_model

########################################################################################################

# Trying to expand dataset by leveraging Active cases
global_stats <- MEV_data %>%
  summarise(
    GlobalMedianApprovalDelay = median(ApprovalDelayWeeks, na.rm = TRUE),
    GlobalMaxApprovalDelay = max(ApprovalDelayWeeks, na.rm = TRUE, default = 0),
    GlobalApprovalRate = sum(Status == "Approved", na.rm = TRUE) / 
      (sum(Status == "Approved", na.rm = TRUE) + sum(Status == "Closed", na.rm = TRUE)),
    AvgValuePerCase = mean(EstdValue, na.rm = TRUE)
  )

facility_stats <- MEV_data %>%
  group_by(Facility) %>%
  summarise(
    ApprovalRate = sum(Status == "Approved", na.rm = TRUE) / 
      (sum(Status == "Approved", na.rm = TRUE) + sum(Status == "Closed", na.rm = TRUE)),
    MedianApprovalDelay = median(ApprovalDelayWeeks, na.rm = TRUE),
    MaxApprovalDelay = max(ApprovalDelayWeeks, na.rm = TRUE, default = 0),
    AvgValuePerApproval = mean(EstdValue[Status == "Approved"], na.rm = TRUE, default = 0)
  ) %>%
  mutate(
    MedianApprovalDelay = if_else(is.na(MedianApprovalDelay) | MedianApprovalDelay <= 0, 
                                  global_stats$GlobalMedianApprovalDelay, MedianApprovalDelay),
    MaxApprovalDelay = if_else(is.na(MaxApprovalDelay) | MedianApprovalDelay <= 0, 
                               global_stats$GlobalMaxApprovalDelay, MaxApprovalDelay),
    ApprovalRate = if_else(is.na(ApprovalRate) | ApprovalRate == 0, 
                           global_stats$GlobalApprovalRate, ApprovalRate),
    AvgValuePerApproval = if_else(is.na(AvgValuePerApproval) | AvgValuePerApproval == 0, 
                                  global_stats$AvgValuePerCase, AvgValuePerApproval)
  )

active_adjustment <- MEV_data %>%
  filter(Status == "Active" & ReceiveWeek <= 109) %>%
  left_join(facility_stats, by = "Facility") %>%
  mutate(
    ExpectedApprovalWeek = ReceiveWeek + round(MedianApprovalDelay),
    ExpectedValue = EstdValue * ApprovalRate
  )

active_contributions <- active_adjustment %>%
  group_by(ExpectedApprovalWeek) %>%
  summarise(ActiveCaseValue = sum(ExpectedValue, na.rm = TRUE), .groups = "drop") %>%
  rename(Week = ExpectedApprovalWeek) 

weekly_data_adjusted <- all_weeks %>%
  left_join(weekly_data, by = "Week") %>%
  left_join(active_contributions, by = "Week") %>%
  mutate(
    NewReceived = replace(NewReceived, is.na(NewReceived), 0),
    NewApproved = replace(NewApproved, is.na(NewApproved), 0),
    NewClosed = replace(NewClosed, is.na(NewClosed), 0),
    EstdValueApproved = replace(EstdValueApproved, is.na(EstdValueApproved), 0),
    CumReceived = cumsum(NewReceived),
    CumApproved = cumsum(NewApproved),
    CumClosed = cumsum(NewClosed),
    CurrentActive = CumReceived - CumApproved - CumClosed,
    ActiveCaseValue = replace(ActiveCaseValue, is.na(ActiveCaseValue), 0),
    AdjustedEstdValueFuture = EstdValueApproved + ActiveCaseValue 
  ) %>%
  as_tsibble(index = Week)

adjusted_plot <- ggplot(weekly_data_adjusted, aes(x = Week)) +
  geom_line(aes(y = EstdValueApproved, color = "Approved Value")) +
  geom_line(aes(y = AdjustedEstdValueFuture, color = "Adjusted Value (with Future Active Cases)")) +
  labs(title = "Estimated Value with Active Case Adjustments",
       y = "Estimated Value ($)", color = "Variable") +
  scale_color_manual(values = c("Approved Value" = "black",
                                "Adjusted Value (with Future Active Cases)" = "red")) +
  theme(legend.position = "bottom")

adjusted_plot

# Req 2: Training and Test Sets using AdjustedEstdValueFuture
train_adjusted <- weekly_data_adjusted %>% filter(Week <= 109)
test_adjusted <- weekly_data_adjusted %>% filter(Week > 109)

unemp_interpolated <- tibble(Week = 1:122) %>%
  mutate(UnemploymentRate = approx(unemp_data$Week, unemp_data$UnemploymentRate, xout = Week, rule = 2)$y) %>%
  as_tsibble(index = Week)

train_adjusted <- train_adjusted %>% 
  select(-matches("UnemploymentRate")) %>% 
  left_join(unemp_interpolated, by = "Week")
test_adjusted <- test_adjusted %>% 
  select(-matches("UnemploymentRate")) %>% 
  left_join(unemp_interpolated, by = "Week")

########################################################################################################

lambda_opt_adj <- weekly_data_adjusted %>%
  features(EstdValueApproved, guerrero) %>%
  pull(lambda_guerrero)

print(lambda_opt_adj)

transformed_data_adj <- weekly_data_adjusted %>%
  mutate(AdjustedEstdValueFuture_transformed = box_cox(AdjustedEstdValueFuture, lambda_opt))

ggplot(transformed_data_adj, aes(x = Week, y = AdjustedEstdValueFuture_transformed)) +
  geom_line() +
  labs(title = "Box-Cox Transformed Estimated Value",
       x = "Week",
       y = "Transformed Estimated Value")

########################################################################################################

# Req 3: Fit Models Separately using AdjustedEstdValueFuture
fit_ETS_adj <- train_adjusted %>%
  model(AutoETS = ETS(box_cox(AdjustedEstdValueFuture, lambda_opt)))
report(fit_ETS_adj)
fit_holt_adj <- train_adjusted %>%
  model(Holt = ETS(box_cox(AdjustedEstdValueFuture, lambda_opt) ~ error("A") + trend("A") + season("N")))
fit_autoarima_adj <- train_adjusted %>%
  model(AutoARIMA = ARIMA(box_cox(AdjustedEstdValueFuture, lambda_opt)))
report(fit_autoarima_adj)
fit_arima510_adj <- train_adjusted %>%
  model(ARIMA510 = ARIMA(box_cox(AdjustedEstdValueFuture, lambda_opt) ~ pdq(5,1,0))) # SIG LAG AT PACF 5
report(fit_arima510_adj)
fit_arima014_adj <- train_adjusted %>%
  model(ARIMA014 = ARIMA(box_cox(AdjustedEstdValueFuture, lambda_opt) ~ pdq(0,1,4))) # SIG LAG AT ACF 4
report(fit_arima014_adj)
fit_dynreg_adj <- train_adjusted %>%
  model(DynReg = ARIMA(box_cox(AdjustedEstdValueFuture, lambda_opt) ~ UnemploymentRate + pdq(d = 1)))
report(fit_dynreg_adj)
fit_piecewise_adj <- train_adjusted %>%
  model(Piecewise = TSLM(box_cox(AdjustedEstdValueFuture, lambda_opt) ~ trend(knots = c(30, 45, 75))))

########################################################################################################

# Req 4: Evaluate Preferred Models
# 4.1: Residual Analysis with gg_tsresiduals
gg_tsresiduals(fit_ETS_adj) + ggtitle("AutoETS Residuals (Adjusted)")
gg_tsresiduals(fit_holt_adj) + ggtitle("Holt Residuals (Adjusted)")
gg_tsresiduals(fit_autoarima_adj) + ggtitle("AutoARIMA Residuals (Adjusted)")
gg_tsresiduals(fit_arima510_adj) + ggtitle("ARIMA(5,1,0) Residuals (Adjusted)")
gg_tsresiduals(fit_arima014_adj) + ggtitle("ARIMA(0,1,4) Residuals (Adjusted)")
gg_tsresiduals(fit_dynreg_adj) + ggtitle("DynReg Residuals (Adjusted)")
gg_tsresiduals(fit_piecewise_adj) + ggtitle("Piecewise Residuals (Adjusted)")

# 4.2: Ljung-Box Test for autocorrelation
ljung_box_results_adj <- bind_rows(
  fit_ETS_adj %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "AutoETS"),
  fit_holt_adj %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "Holt"),
  fit_autoarima_adj %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "AutoARIMA"),
  fit_arima510_adj %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "ARIMA510"),
  fit_arima014_adj %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "ARIMA014"),
  fit_dynreg_adj %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "DynReg"),
  fit_piecewise_adj %>% augment() %>% features(.innov, ljung_box, lag = 10) %>% mutate(Model = "Piecewise")
) %>%
  select(Model, lb_stat, lb_pvalue)

ljung_box_results_adj

last_unemp_rate <- unemp_interpolated %>%
  filter(Week == 109) %>%
  pull(UnemploymentRate)

future_data_adj <- new_data(train, 13) %>%
  mutate(
    Week = seq(110, 122, by = 1),
    UnemploymentRate = if_else(
      Week <= 109,
      approx(unemp_data$Week, unemp_data$UnemploymentRate, xout = Week, rule = 2)$y,
      last_unemp_rate # assuming we don't know future unemployment rates
    )
  ) %>%
  as_tsibble(index = Week)

########################################################################################################

# Req 5: Forecasts
fc_ETS_adj <- fit_ETS_adj %>% forecast(h = 13)
fc_holt_adj <- fit_holt_adj %>% forecast(h = 13)
fc_autoarima_adj <- fit_autoarima_adj %>% forecast(h = 13)
fc_arima510_adj <- fit_arima510_adj %>% forecast(h = 13)
fc_arima014_adj <- fit_arima014_adj %>% forecast(h = 13)
fc_dynreg_adj <- fit_dynreg_adj %>% forecast(new_data = future_data_adj)
fc_piecewise_adj <- fit_piecewise_adj %>% forecast(h = 13)

RMSE_ETS_adj <- fc_ETS_adj %>% accuracy(test_adjusted) %>% select(.model, RMSE)
RMSE_holt_adj <- fc_holt_adj %>% accuracy(test_adjusted) %>% select(.model, RMSE)
RMSE_autoarima_adj <- fc_autoarima_adj %>% accuracy(test_adjusted) %>% select(.model, RMSE)
RMSE_arima510_adj <- fc_arima510_adj %>% accuracy(test_adjusted) %>% select(.model, RMSE)
RMSE_arima014_adj <- fc_arima014_adj %>% accuracy(test_adjusted) %>% select(.model, RMSE)
RMSE_dynreg_adj <- fc_dynreg_adj %>% accuracy(test_adjusted) %>% select(.model, RMSE)
RMSE_piecewise_adj <- fc_piecewise_adj %>% accuracy(test_adjusted) %>% select(.model, RMSE)

AICc_ETS_adj <- fit_ETS_adj %>% glance() %>% select(.model, AICc)
AICc_holt_adj <- fit_holt_adj %>% glance() %>% select(.model, AICc)
AICc_autoarima_adj <- fit_autoarima_adj %>% glance() %>% select(.model, AICc)
AICc_arima510_adj <- fit_arima510_adj %>% glance() %>% select(.model, AICc)
AICc_arima014_adj <- fit_arima014_adj %>% glance() %>% select(.model, AICc)
AICc_dynreg_adj <- fit_dynreg_adj %>% glance() %>% select(.model, AICc)
AICc_piecewise_adj <- fit_piecewise_adj %>% glance() %>% select(.model, AICc)

accuracy_metrics_adj <- bind_rows(RMSE_ETS_adj, RMSE_holt_adj, RMSE_autoarima_adj, RMSE_arima510_adj, RMSE_arima014_adj, RMSE_dynreg_adj, RMSE_piecewise_adj) %>%
  left_join(bind_rows(AICc_ETS_adj, AICc_holt_adj, AICc_autoarima_adj, AICc_arima510_adj, AICc_arima014_adj, AICc_dynreg_adj, AICc_piecewise_adj), by = ".model")

accuracy_metrics_adj

forecast_plot_adj <- ggplot() +
  autolayer(train_adjusted, AdjustedEstdValueFuture, color = "black") +
  autolayer(fc_ETS_adj, color = "green", level = NULL) +
  autolayer(fc_arima510_adj, color = "yellow", level = NULL) +
  autolayer(fc_dynreg_adj, color = "red", level = NULL) +
  autolayer(fc_piecewise_adj, color = "blue", level = NULL) +
  autolayer(test_adjusted, AdjustedEstdValueFuture, color = "black") +
  labs(title = "Forecasts vs Actual Adjusted Estimated Value Future", y = "Adjusted Estimated Value ($)", color = "Model") +
  scale_color_manual(values = c("AutoETS" = "green", "ARIMA510" = "yellow",
                                "DynReg" = "red", "Piecewise" = "blue", 
                                "Actual" = "black"))

forecast_plot_adj

########################################################################################################

# Req 6: Preferred Model
preferred_model_adj <- accuracy_metrics_adj %>%
  arrange(RMSE)

preferred_model_adj




