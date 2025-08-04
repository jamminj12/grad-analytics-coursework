library(stats)
library(dendextend)
library(cluster)
library(factoextra) # found this one
library(ggrepel) # for txt labels

file_path <- "data/Cost_food.csv"
cost_food <- read.csv(file_path, header = TRUE)
feature_data <- cost_food[, c("CSTS", "CASF", "CLNS", "CVEG", "CFRT", "COFT")]
countries <- cost_food$Country

feature_data_std <- scale(feature_data)

# Euclidean distance matrix
d.mat <- dist(feature_data_std, method = "euclidean")

# Hierarchical Clustering: Nearest-Neighbor (Single Linkage)
clus_single <- hclust(d.mat, method = "single")
dend_single <- as.dendrogram(clus_single)
ordered_countries_sing <- countries[clus_single$order]
labels(dend_single) <- ordered_countries_sing
dend_single <- set(dend_single, "labels_cex", 0.5)

################################################################################

# Nearest-Neighbor Dendrogram -> cut at distance 2.5
cut_dist <- 2.5
plot(dend_single, main = "Nearest-Neighbor Dendrogram", xlab = "Countries",
     ylab = "Distance", horiz = FALSE, cex = 0.5)
rect.hclust(clus_single, h = cut_dist, border = "red")
abline(h = cut_dist, lty = 2, col = "blue")
clusters_single <- cutree(clus_single, h = cut_dist)
cat(sprintf("Nearest-Neighbor Clusters at distance %.1f:\n", cut_dist), table(clusters_single))

################################################################################

# Hierarchical Clustering: Furthest-Neighbor (Complete Linkage)
clus_complete <- hclust(d.mat, method = "complete")
dend_complete <- as.dendrogram(clus_complete)
ordered_countries_comp <- countries[clus_complete$order]
labels(dend_complete) <- ordered_countries_comp
dend_complete <- set(dend_complete, "labels_cex", 0.5)
plot(dend_complete, main = "Furthest-Neighbor Dendrogram", xlab = "Countries", ylab = "Distance", hang = -1)

################################################################################

# Hierarchical Clustering: Average Linkage (UPGMA)
clus_upgma <- hclust(d.mat, method = "average")
dend_upgma <- as.dendrogram(clus_upgma)
ordered_countries_upgma <- countries[clus_upgma$order]
labels(dend_upgma) <- ordered_countries_upgma
dend_upgma <- set(dend_upgma, "labels_cex", 0.5)
dend_upgma %>% 
  set("labels_col", "black") %>% 
  set("branches_col", "black") %>%
  plot(main = "Average Linkage (UPGMA) Dendrogram", xlab = "Countries", ylab = "Distance")

################################################################################

# Hierarchical Clustering with agnes
clus_agnes <- agnes(d.mat, method = "average")
dend_agnes <- as.dendrogram(clus_agnes)
ordered_countries_agnes <- countries[clus_agnes$order]
labels(dend_agnes) <- ordered_countries_agnes
dend_agnes <- set(dend_agnes, "labels_cex", 0.5)
cat("Agglomerative Coefficient from agnes:\n", clus_agnes$ac)  
plot(dend_agnes, main = "AGNES Average Linkage Dendrogram", xlab = "Countries", ylab = "Distance")

################################################################################

# K-means Clustering
num_clusters <- 4
kmeans_result <- kmeans(feature_data_std, centers = num_clusters, nstart = 10)
cat(sprintf("K-means Clustering with %.0f clusters:\n",num_clusters), table(kmeans_result$cluster))

tot_var <- sum(kmeans_result$totss)
within_var <- sum(kmeans_result$withinss)
percent_var <- (1 - within_var / tot_var) * 100
cat(sprintf("Percentage of variation accounted for by %.0f clusters:",num_clusters), percent_var, "%\n")

cluster_assignments <- data.frame(Country = countries, Kmeans_Cluster = kmeans_result$cluster)
cat("K-means Cluster Assignments:\n")
print(cluster_assignments)

# K-means clusters w/ first two principal components
pca <- prcomp(feature_data_std, scale. = FALSE)
pca_data <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], Cluster = as.factor(kmeans_result$cluster), Country = countries)
plot(pca_data$PC1, pca_data$PC2, col = pca_data$Cluster, pch = 19, 
     main = "K-means Clusters on First Two Principal Components",
     xlab = "PC1", ylab = "PC2")
text(pca_data$PC1, pca_data$PC2, labels = pca_data$Country, pos = 3, cex = 0.6)


# with "cluster ellipses"
ellipse_cluster_plot <- fviz_cluster(kmeans_result, data = feature_data_std,
                          ellipse.type = "norm",
                          geom = "point",
                          repel = FALSE,
                          show.clust.cent = FALSE,
                          main = "K-means Clusters on First Two Principal Components") 
ellipse_cluster_plot + 
  geom_text_repel(data = pca_data, aes(x = PC1, y = PC2, label = Country),
                  size = 3, max.overlaps = 100)


################################################################################


################################################################################


get_suggested_clusters <- function(hclust_obj, countries, title, order) {
  dend <- as.dendrogram(hclust_obj)
  ordered_labels <- countries[order]
  labels(dend) <- ordered_labels
  dend <- set(dend, "labels_cex", 0.5)
  
  heights <- hclust_obj$height
  print(heights)
  inconsistency <- c(NA, diff(heights))
  print(inconsistency)
  largest_jump_index <- which.max(inconsistency)
  print(largest_jump_index)
  suggested_clusters <- (length(hclust_obj$order) - largest_jump_index) + 1
  cut_height <- heights[largest_jump_index]-0.00001
  
  plot(dend, main = paste(title, "Dendrogram w/ Suggested Clusters"), xlab = "Countries", ylab = "Distance")
  rect.hclust(hclust_obj, h = cut_height, border = "red")
  abline(h = cut_height, col = "blue", lty = 2)
  
  cat(sprintf("\n[%s]\n", title))
  cat(sprintf("Suggested clusters: %d (cut height: %.2f)\n", suggested_clusters, cut_height))
  return(list(clusters = cutree(hclust_obj, h = cut_height), h = cut_height, k = suggested_clusters))
}

sug_single <- get_suggested_clusters(clus_single, countries, "Single Linkage",clus_single$order)
sug_complete <- get_suggested_clusters(clus_complete, countries, "Complete Linkage",clus_complete$order)
sug_upgma <- get_suggested_clusters(clus_upgma, countries, "UPGMA (Average Linkage)",clus_upgma$order)
agnes_hclust <- as.hclust(clus_agnes)  # Convert agnes to hclust
sug_agnes <- get_suggested_clusters(agnes_hclust, countries, "AGNES Average Linkage",agnes_hclust$order)

################################################################################

# Divisive clustering
clus_diana <- diana(feature_data_std)

dend_diana <- as.dendrogram(clus_diana)
dend_diana <- set(dend_diana, "labels", countries)
dend_diana <- set(dend_diana, "labels_cex", 0.5)
plot(dend_diana, main = "DIANA (Divisive Clustering) Dendrogram", 
     xlab = "Countries", ylab = "Divisive Coefficient")

cat("Divisive Coefficient from DIANA:\n", clus_diana$dc, "\n")

diana_hclust <- as.hclust(clus_diana)
sug_diana <- get_suggested_clusters(diana_hclust, countries, "DIANA Divisive Clustering",diana_hclust$order)

################################################################################

cluster_compare <- data.frame(
  Country = countries,
  Single = sug_single$clusters,
  Complete = sug_complete$clusters,
  UPGMA = sug_upgma$clusters,
  AGNES = sug_agnes$clusters,
  DIANA = sug_diana$clusters,
  Kmeans = kmeans_result$cluster
)
print(cluster_compare)

