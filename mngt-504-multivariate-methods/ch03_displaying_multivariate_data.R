
# All functions from Appendix: Displaying Multivariate Data in R

library(car)
library(aplpack)
library(graphics) 
library(scatterplot3d)
library(rgl)


file_path <- "data/Sparrows.csv"
sparrows <- read.csv(file_path, header = TRUE)


# A.1 Two-dimensional scatter plot
plot(sparrows$X1, sparrows$X2,
     col = 1,
     pch = ifelse(sparrows$Survivorship == "S", 15, 0),
     xlab = "Total Length",
     ylab = "Alar Extent",
     main = "2D Scatter Plot of Sparrow Measurements")

legend("topright", 
       legend = c("Survived", "Not Survived"), 
       pch = c(15, 0), 
       col = 1)

#############################################################################

# A.2  Three-dimensional scatter plots
# option 1 (use package scatterplot3d)
scatterplot3d(sparrows$X1, sparrows$X2, sparrows$X3,
              pch = ifelse(sparrows$Survivorship == "S", 15, 0),
              xlab = "Total Length", 
              ylab = "Alar Extent", 
              zlab = "Length of Humerus")

legend("topright", 
       legend = c("Survived", "Not Survived"), 
       pch = c(15, 0), 
       col = 1)

# option 2 (use package car) 
scatter3d(x = sparrows$X2,
          y = sparrows$X3,
          z = sparrows$X1,
          point.col = "black",
          point.shape = ifelse(sparrows$Survivorship == "S", 15, 0),
          surface = FALSE,
          ellipsoid = FALSE,
          axis.scales = TRUE,
          xlab = "Alar Extent", 
          ylab = "Length of Humerus", 
          zlab = "Total Length")

# option 3 (use package rgl)
plot3d(sparrows$X1, sparrows$X2, sparrows$X3,
       col = ifelse(sparrows$Survivorship == "S", "blue", "red"),
       size = 5,
       xlab = "Total Length",
       ylab = "Alar Extent",
       zlab = "Length of Humerus")

#############################################################################

# A.3 Draftsman’s plots
pairs(sparrows[, c("X1", "X2", "X3")],
      pch = ifelse(sparrows$Survivorship == "S",15,0),
      main = "Draftsman’s Plot of Sparrow Measurements")

scatterplotMatrix(~ X1 + X2 + X3 | Survivorship,
                  data = sparrows,
                  main = "Enhanced Scatterplot Matrix of Sparrow Measurements")

#############################################################################

# A.4 Representation of individual data points: Chernoff faces and stars
# Chernoff faces using aplpack
faces(sparrows[, c("X1", "X2", "X3", 
                   "X4", "X5")], 
      face.type = 1, 
      main = "Chernoff Faces for Sparrow Data")

# Stars plot using the graphics package
stars(sparrows[, c("X1", "X2", "X3", 
                   "X4", "X5")], 
      key.loc = c(1, 1), 
      main = "Star Plot for Sparrow Data")

# Variant with circular sectors
stars(sparrows[, c("X1", "X2", "X3", 
                   "X4", "X5")], 
      key.loc = c(1, 1), 
      draw.segments = TRUE, 
      main = "Segment Star Plot for Sparrow Data")

#############################################################################

# A.5 Profiles of variables
vars <- c("X1", "X2", "X3", "X4", "X5")

group_means <- t(sapply(split(sparrows[, vars], sparrows$Survivorship), 
                        function(df) colMeans(df, na.rm = TRUE)))

overall_means <- colMeans(sparrows[, vars], na.rm = TRUE)
ordered_vars <- vars[order(overall_means)]
group_means <- group_means[, ordered_vars]

# Line Plot
plot(1:length(ordered_vars), group_means["S", ], 
     type = "n", ylim = range(group_means), 
     xaxt = "n", 
     xlab = "Variables", 
     ylab = "Measurement Size",
     main = "Profile Plot by Survivorship")

axis(1, at = 1:length(ordered_vars), labels = ordered_vars)

lines(1:length(ordered_vars), group_means["S", ], col = "blue", lwd = 2, type = "b")
lines(1:length(ordered_vars), group_means["NS", ], col = "red", lwd = 2, type = "b")

legend("topleft", legend = c("Survived", "Not Survived"),
       col = c("blue", "red"), lty = 1, lwd = 2)


# Bar Plot
group_means_t <- t(group_means)

barplot(group_means_t,
        beside = TRUE,,
        names.arg = c("Survived", "Not Survived"),
        xlab = "Variables",
        ylab = "Measurement Size",
        main = "Mean Measurements by Variable and Survivorship",
        legend.text = c("Survived", "Not Survived"),
        args.legend = list(x = "topleft", bty = "n"))

