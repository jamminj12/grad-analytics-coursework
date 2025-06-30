library(ggplot2)
library(corrplot)
library(reshape2)
library(patchwork)

#file_path <- "data/Cost_food.csv"
#cost_food <- read.csv(file_path, header = TRUE)
#feature_data <- cost_food[, c("CSTS", "CASF", "CLNS", "CVEG", "CFRT", "COFT")]

file_path <- "data/Sparrows.csv"
sparrows <- read.csv(file_path, header = TRUE)
feature_data <- sparrows[, c("X1", "X2", "X3", "X4", "X5")]

#file_path <- "data/raw_pga_536_2021.csv"
#masters <- read.csv(file_path, header = TRUE)
#feature_data <- na.omit(masters[, c("sg_putt", "sg_arg", "sg_app", "sg_ott", "sg_t2g")])
#feature_data <- na.omit(masters[, c("sg_putt", "sg_arg", "sg_app", "sg_ott", "sg_t2g", "sg_total", "driving_dist", "driving_acc")])

scaled_data <- scale(feature_data)

pca_result <- prcomp(scaled_data, center = FALSE, scale. = FALSE)
loadings <- pca_result$rotation
biplot(pca_result, main = "Biplot: prcomp")
pca_summary <- summary(pca_result)
var_proportions <- pca_summary$importance["Proportion of Variance", ]

create_pca_loadings_heatmap <- function(loadings, feature_names, title, vmin = -1, vmax = 1, absolute = FALSE, var_proportions) {
  if (absolute) {
    loadings <- abs(loadings)
    vmin <- 0
  }
  
  loadings_df <- as.data.frame(loadings)
  loadings_df$Feature <- feature_names
  loadings_melt <- melt(loadings_df, id.vars = "Feature", variable.name = "PC", value.name = "Loading")
  
  pc_levels <- colnames(loadings)
  loadings_melt$PC_numeric <- as.numeric(factor(loadings_melt$PC, levels = pc_levels))
  var_cumsum <- cumsum(var_proportions) - var_proportions / 2
  loadings_melt$PC_scaled <- var_cumsum[as.character(loadings_melt$PC)]
  
  # Create heatmap
  p <- ggplot(loadings_melt, aes(x = PC_scaled, y = Feature, fill = Loading)) +
    geom_tile(width = var_proportions[as.character(loadings_melt$PC)]) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                         midpoint = 0, limits = c(vmin, vmax), 
                         name = "Loading") +
    scale_x_continuous(
      breaks = var_cumsum,
      labels = paste0(pc_levels, "\n(", round(var_proportions * 100, 1), "%)")
    ) +
    theme_minimal(base_size = 15) +
    labs(title = title, x = "Principal Component (Proportion of Variance)", y = "Feature") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines")
    ) +
    coord_fixed(ratio = 1 / length(feature_names))  # Adjust aspect ratio for square tiles
  
  return(p)
}

# normal
p1 <- create_pca_loadings_heatmap(
  loadings,
  colnames(feature_data),
  title = "PCA Loadings Heatmap",
  vmin = -1,
  vmax = 1,
  absolute = FALSE,
  var_proportions = var_proportions
)

# abs value
p2 <- create_pca_loadings_heatmap(
  loadings,
  colnames(feature_data),
  title = "PCA Loadings Heatmap (Absolute Values)",
  vmin = 0,
  vmax = 1,
  absolute = TRUE,
  var_proportions = var_proportions
)

print(p1)
# For Diet Data:
# All 5 variables negative. Higher PC1 scores correspond to lower costs across most food groups
# PC2 contrasts high costs of vegetables and fruits with low costs of oils and fats.

# For Sparrow Data:
# all 5 variables positive. overall body size. larger sparrows have higher PC1 score
# PC2 contrasts keel of sternum length with beak and head length and alar extent.

# For Golf Data:
# PC1 shows skill from teeing off to landing on the green
# Pc2 shows putting skill
# Pc3 shows off the tee skill
# pc4 shows around the green skill


#sg_putt: strokes gained putting
#sg_arg: around the green
#sg_app: approach shots
#sg_ott: off the tee
#sg_t2g: tee to green

print(p2)


### Correlation Matrix Plot
cor_matrix <- cor(feature_data)

corrplot(cor_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black",        
         number.cex = 0.7,             
         title = "Correlation Matrix", 
         mar = c(0,0,1,0))            

