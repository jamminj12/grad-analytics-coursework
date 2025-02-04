library(fpp3)
library(glue)

# Part 1: Review and comment on the STL-based features for the holiday series
view(tourism)
holiday_tourism_features <- tourism %>%
  filter(Purpose == "Holiday") %>%
  features(Trips, feat_stl) %>%
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = factor(glue("Q{seasonal_peak_year}")),
    seasonal_trough_year = factor(glue("Q{seasonal_trough_year}")),
  )

view(holiday_tourism_features)
holiday_tourism_features %>%
  GGally::ggpairs(cardinality_threshold = 76) 
  # I removed the Region, State, and Purpose columns from the features plot because they clogged the plot window with unnecessary info about the data. 

# Part 2: Identify the peak quarters for holidays in each state.
peak_holiday_quarters_by_state <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips)) %>% 
  features(Trips, feat_stl) %>%
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_peak_year = factor(glue("Q{seasonal_peak_year}"))
  ) %>%
  select(State, seasonal_peak_year)

print(peak_holiday_quarters_by_state)




library(broom)

view(PBS)
PBS_features <- PBS %>%
  features(Cost, feat_stl)  %>% # received multiple errors when trying feature_set(pkgs = "feasts"). Opted to use feat_stl instead of trying to fix errors.
  na.omit() # using this to eliminate na values.

PBS_PCA_output <- PBS_features %>%
  select(-Concession, -Type, -ATC1, -ATC2) %>%
  prcomp(scale = TRUE) %>%
  augment(PBS_features)

PBS_PCA_output %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point() +
  theme(aspect.ratio = 1)

# pulling outliers out
PBS_PCA_outliers <- PBS_PCA_output %>%
  filter(.fittedPC1 < -4) # picked these values arbitrarily based on the output of the PCA plot.
PBS_PCA_outliers %>%
  select(ATC1, ATC2, Type, Concession, .fittedPC1, .fittedPC2)

PBS %>% 
  autoplot(Cost) +
  theme(legend.position = "none")

PBS_PCA_outliers %>%
  left_join(PBS, by = c("Concession", "Type", "ATC1", "ATC2"), multiple = "all") %>%
  mutate(Series = glue("{Concession}", "{Type}", "{ATC1}", "{ATC2}", .sep = "\n\n")) %>%
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying PBS time series in PC space")

