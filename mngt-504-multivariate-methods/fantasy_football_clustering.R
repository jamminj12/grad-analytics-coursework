library(ffanalytics)
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(ggrepel)

# Scrape ESPN projections
my_scrape <- scrape_data(src = c("CBS"), 
                         pos = c("QB", "RB", "WR", "TE", "DST"),
                         season = 2025, 
                         week = NULL)

# Extract QB data and select relevant columns
qb_data <- my_scrape[["QB"]]

qb_cluster_data <- qb_data %>%
  select(player, pass_att, pass_comp, pass_yds, pass_tds, rush_att, rush_yds, rush_tds) %>%
  drop_na()

qb_players <- qb_cluster_data$player

# Define fantasy football-based weights
weights <- c(pass_att = 0.0, 
             pass_comp = 0.0, 
             pass_yds = 0.04, 
             pass_tds = 4, 
             rush_att = 0.0, 
             rush_yds = 0.1, 
             rush_tds = 6)

nonzero_weights <- weights[weights != 0] #added this because i was getting errors of zero values
qb_features <- qb_cluster_data[names(nonzero_weights)]

qb_matrix <- as.matrix(qb_features)
qb_weighted <- qb_matrix %*% diag(as.numeric(nonzero_weights))
qb_scaled_weighted <- scale(qb_weighted)
rownames(qb_scaled_weighted) <- qb_cluster_data$player

qb_dist <- dist(qb_scaled_weighted, method = "euclidean")
hc_qb <- hclust(dist(qb_scaled_weighted), method = "average")

sug_qb_clus <- get_suggested_clusters(hc_qb, qb_players, "QB - Average Linkage", hc_qb$order)
qb_clusters <- cutree(hc_qb, k = sug_qb_clus$k)
names(qb_clusters) <- rownames(qb_scaled_weighted)

dend_qb <- as.dendrogram(hc_qb)
dend_qb <- set(dend_qb, "labels", qb_players)
dend_qb <- color_branches(dend_qb, k = sug_qb_clus$k) #colroed trees
dend_qb <- set(dend_qb, "labels_cex", 0.5)

plot(dend_qb, main = "QB - Average Linkage Dendrogram (Weighted Features)",
     xlab = "", ylab = "Distance", cex = 0.5)






# PCA on scaled, weighted data
pca_result <- prcomp(qb_scaled_weighted, scale. = FALSE)
pca_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  player = qb_cluster_data$player,
  cluster = as.factor(qb_clusters[qb_cluster_data$player])
)

# PCA scatter plot with cluster coloring and labels
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster, label = player)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "QB Clustering via PCA (Weighted Fantasy Features)",
       x = "Principal Component 1",
       y = "Principal Component 2")

