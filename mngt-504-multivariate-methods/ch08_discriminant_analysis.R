library(MASS)
library(candisc)
library(ggplot2)
library(klaR)


file_path <- "data/Cost_food.csv"
cost_food <- read.csv(file_path, header = TRUE)
feature_data <- cost_food[, c("CSTS", "CASF", "CLNS", "CVEG", "CFRT", "COFT")]
group <- as.factor(cost_food$Group)
countries <- cost_food$Country

##############################################################################

# Canonical Discriminant Analysis Using lda

# see pg. 158
lda_model <- lda(feature_data, group)
print(lda_model)

# Proportion of trace
lda_summary <- summary(lda_model)
cat("Proportion of Trace:\n", lda_model$svd^2 / sum(lda_model$svd^2), "\n")

# Classification table
lda_pred <- predict(lda_model)
classification_table <- table(group, lda_pred$class)
classification_table

# Jackknife classification
lda_model_cv <- lda(feature_data, group, CV = TRUE)
jackknife_table <- table(group, lda_model_cv$class)
jackknife_table

# Scores for plotting
lda_scores <- predict(lda_model)$x

##############################################################################

# Canonical Discriminant Analysis Using candisc

candisc_model <- candisc(lm(cbind(CSTS, CASF, CLNS, CVEG, CFRT, COFT) ~ group, data = cost_food))
print(candisc_model)

# Eigenvalues
cat("\nEigenvalues:\n", candisc_model$eigenvalues, "\n")

# pg. 158 and 159
raw_coeffs <- candisc_model$coeffs.raw
std_coeffs <- candisc_model$coeffs.std
cat("\nRaw Canonical Coefficients:\n")
print(raw_coeffs)
cat("\nStandardized Canonical Coefficients:\n")
print(std_coeffs)

# table 8.5
correlations <- candisc_model$structure
cat("\nCorrelations between Original Variables and Canonical Scores:\n")
print(correlations)

##############################################################################

# Plotting Canonical Discriminant Functions

# Figure 8.1a
plot(candisc_model, which = c(1, 2), main = "LD1 vs LD2 with Centroids")

# Figure 8.1b
lda_scores_df <- data.frame(LD1 = lda_scores[,1], LD2 = lda_scores[,2], Group = group, Country = countries)
ggplot(lda_scores_df, aes(x = LD1, y = LD2, color = Group, label = Country)) +
  geom_point(size = 3) +
  geom_text(hjust = 0.5, vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "LD1 vs LD2 by Country", x = "LD1", y = "LD2")

# Figure 8.2a
plot(candisc_model, which = c(1, 3), main = "LD1 vs LD3 with Centroids")

# Figure 8.2b
lda_scores_df <- data.frame(LD1 = lda_scores[,1], LD3 = lda_scores[,3], Group = group, Country = countries)
ggplot(lda_scores_df, aes(x = LD1, y = LD3, color = Group, label = Country)) +
  geom_point(size = 3) +
  geom_text(hjust = 0.5, vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "LD1 vs LD3 by Country", x = "LD1", y = "LD3")

##############################################################################

# ### PICK UP HERE ### #


# Logistic Regression for Two-Group Discrimination

# EU95 vs NEU
logistic_data <- cost_food[cost_food$Group %in% c("EU95", "NEU"), ]
logistic_data$Y <- ifelse(logistic_data$Group == "EU95", 1, 0)

logistic_model <- glm(Y ~ CSTS + CASF + CLNS + CVEG + CFRT + COFT, 
                      family = binomial(link = "logit"), data = logistic_data)

summary_logistic <- summary(logistic_model)
cat("\nLogistic Regression Summary:\n")
print(summary_logistic)

# Chi-squared test for model significance
null_model <- glm(Y ~ 1, family = binomial(link = "logit"), data = logistic_data)
anova_test <- anova(null_model, logistic_model, test = "Chisq")
cat("\nChi-squared Test for Model Significance:\n")
print(anova_test)

# Plot logistic regression probabilities (similar to the provided plot)
logistic_pred <- predict(logistic_model, type = "response")
logistic_plot_data <- data.frame(
  Order = 1:nrow(logistic_data),  # X-axis: order of skulls (1 to n)
  Probability = logistic_pred,
  Group = logistic_data$Group,
  Label = ifelse(logistic_data$Group == "EU95", "EU95", "NEU")  # "E" for EU95, "N" for NEU
)

# Calculate average probabilities for each group
avg_prob <- aggregate(Probability ~ Group, data = logistic_plot_data, mean)

# Create the plot
ggplot(logistic_plot_data, aes(x = Order, y = Probability)) +
  geom_point(aes(color = Group, shape = Group), size = 3) +  # Points for each observation
  geom_text(aes(label = Label), hjust = 0.5, vjust = -0.5, size = 3) +  # Labels "E" or "N"
  geom_hline(data = avg_prob, aes(yintercept = Probability, linetype = Group), color = "black", size = 0.5) +  # Average lines
  scale_linetype_manual(values = c("solid", "dashed")) +  # Solid for EU95, dashed for NEU
  scale_color_manual(values = c("EU95" = "black", "NEU" = "black")) +  # Black points for both groups
  scale_shape_manual(values = c("EU95" = 16, "NEU" = 1)) +  # Different shapes (filled circle for EU95, open circle for NEU)
  theme_minimal() +
  labs(title = "Logistic Regression Probabilities for EU95 vs NEU", 
       x = "Countries in Data Order", 
       y = "Probability of EU95", 
       linetype = "Average probability", 
       color = "Group", 
       shape = "Group") +
  guides(color = "none", shape = "none")

##############################################################################

# Stepwise and Quadratic Discriminant Analysis

# Stepwise discriminant analysis
stepclass_model <- stepclass(feature_data, group, method = "lda", direction = "forward")

# Quadratic discriminant analysis -- not working for current dataset.
qda_model <- qda(feature_data, group)


