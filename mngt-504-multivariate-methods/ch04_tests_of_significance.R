
# All functions from Appendix: Tests of Significance in R

library(Hotelling)
library(car)
library(biotools)

file_path <- "data/Sparrows.csv"
sparrows <- read.csv(file_path, header = TRUE)

#############################################################################

# A.1 Univariate Two-Sample t-test in R

t.test(X1 ~ Survivorship, data = sparrows, var.equal = TRUE)
# p-value of 0.3258 -> no real relationship between two variables

#############################################################################

# A.2 Multivariate Two-Sample Tests with Hotelling's T^2

A2 <- hotelling.test(X1 + X2 + X3 + X4 + X5 ~ Survivorship, data = sparrows)
print(A2)
# p-value (0.7622) -> no significant relationship between the 5 variables and survival

#############################################################################

# A.3 Comparison of Variation of Two Samples for the Univariate Case

# F-test
var.test(X1 ~ Survivorship, data = sparrows)
# p-value (0.4788), variances (1.36), confidence interval (0.57 to 3.07)

# Levene's Test
leveneTest(X1 ~ Survivorship, data = sparrows)
# p-value (0.235)

#############################################################################

# A.4 Comparison of Variation of Two Multivariate Samples

adm <- sparrows
adm[, 3:7] <- abs(scale(sparrows[, 3:7], center = apply(sparrows[, 3:7], 2, median), scale = FALSE))

A4 <- hotelling.test(X1 + X2 + X3 + X4 + X5 ~ Survivorship, data = adm)
print (A4)
# p-value (0.4024)

#############################################################################

# A.5 Comparison of Several Multivariate Means: One-Factor MANOVA

manova_obj <- manova(cbind(X1, X2, X3, X4, X5) ~ Survivorship, data = sparrows)
manova_obj

# Wilks' Lambda test
summary(manova_obj, test = "Wilks")

#############################################################################

# A.6 Testing the Equality of Several Covaraince Matrices

boxM(sparrows[, 3:7], sparrows$Survivorship)
# p-value = 0.7933