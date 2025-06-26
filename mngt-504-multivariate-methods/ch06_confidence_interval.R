# Load required libraries
library(ggplot2)
library(corrplot)
library(reshape2)

# Load the data
file_path <- "data/Sparrows.csv"
sparrows <- read.csv(file_path, header = TRUE)

# Select the feature columns
feature_data <- sparrows[, c("X1", "X2", "X3", "X4", "X5")]

# Scale the data
scaled_data <- scale(feature_data)

# Perform PCA
pca_result <- prcomp(scaled_data, center = FALSE, scale. = FALSE)  # Data is already scaled

# Extract loadings
loadings <- pca_result$rotation

# Function to create a PCA loadings heatmap
create_pca_loadings_heatmap <- function(loadings, feature_names, title, vmin = -1, vmax = 1, absolute = FALSE) {
  # If absolute, take absolute values of loadings
  if (absolute) {
    loadings <- abs(loadings)
    vmin <- 0
  }
  
  # Convert loadings to a data frame for plotting
  loadings_df <- as.data.frame(loadings)
  loadings_df$Feature <- feature_names
  loadings_melt <- melt(loadings_df, id.vars = "Feature", variable.name = "PC", value.name = "Loading")
  
  # Create heatmap using ggplot2
  p <- ggplot(loadings_melt, aes(x = PC, y = Feature, fill = Loading)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                         midpoint = 0, limits = c(vmin, vmax), 
                         name = "Loading") +
    theme_minimal(base_size = 15) +
    labs(title = title, x = "Principal Component", y = "Feature") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Plot the value of the coefficients
p1 <- create_pca_loadings_heatmap(
  loadings,
  colnames(feature_data),
  title = "PCA Loadings Heatmap",
  vmin = -1,
  vmax = 1,
  absolute = FALSE
)

# Plot the absolute value of the coefficients
p2 <- create_pca_loadings_heatmap(
  loadings,
  colnames(feature_data),
  title = "PCA Loadings Heatmap (Absolute Values)",
  vmin = 0,
  vmax = 1,
  absolute = TRUE
)

# Display the plots
print(p1)
print(p2)

# Optional: Save the plots to files
ggsave("pca_loadings_heatmap.png", p1, width = 8, height = 6)
ggsave("pca_loadings_absolute_heatmap.png", p2, width = 8, height = 6)