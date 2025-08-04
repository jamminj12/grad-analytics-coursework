# Load Libraries
library(ffanalytics)
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(ggrepel)

# Scrape 2025 Projections
my_scrape <- scrape_data(src = c("ESPN"),
                         pos = c("QB", "RB", "WR", "TE"),
                         season = 2025,
                         week = NULL)

# Fantasy Weighting Function
get_weighted_features <- function(data, position) {
  if (position == "QB") {
    weights <- c(pass_att = 0, pass_comp = 0, pass_yds = 0.04, pass_tds = 4,
                 rush_att = 0, rush_yds = 0.1, rush_tds = 6)
    features <- data %>%
      select(player, pass_att, pass_comp, pass_yds, pass_tds,
             rush_att, rush_yds, rush_tds) %>%
      drop_na()
    
  } else if (position %in% c("RB", "WR", "TE")) {
    weights <- c(rush_att = 0, rush_yds = 0.1, rush_tds = 6,
                 rec = 0.5, rec_yds = 0.1, rec_tds = 6)
    features <- data %>%
      select(player, rush_att, rush_yds, rush_tds,
             rec, rec_yds, rec_tds) %>%
      drop_na()
    
  } else {
    stop("Position not supported.")
  }
  
  # Matrix transformation and weighting
  feat_matrix <- as.matrix(features[, -1])
  scaled <- scale(feat_matrix)
  weighted <- scaled %*% diag(as.numeric(weights))
  
  list(
    player = features$player,
    data = weighted
  )
}

# Analysis Function
analyze_position <- function(data, position, n_clusters = 4) {
  weighted_result <- get_weighted_features(data, position)
  player_names <- weighted_result$player
  weighted_data <- weighted_result$data
  
  # Clustering
  dists <- dist(weighted_data)
  hc <- hclust(dists, method = "complete")
  clusters <- cutree(hc, k = n_clusters)
  
  # PCA
  pca <- prcomp(weighted_data, scale. = FALSE)
  pc_df <- data.frame(
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    player = player_names,
    cluster = as.factor(clusters)
  )
  
  # Outlier detection (based on PCA distance from center)
  pc_df$distance <- sqrt(pc_df$PC1^2 + pc_df$PC2^2)
  pc_df <- pc_df %>%
    arrange(desc(distance)) %>%
    mutate(outlier_rank = row_number())
  
  # Plot
  p <- ggplot(pc_df, aes(x = PC1, y = PC2, color = cluster, label = player)) +
    geom_point(size = 3) +
    geom_text_repel(max.overlaps = 15, size = 3) +
    theme_minimal() +
    labs(title = paste(position, "PCA Clustering with Fantasy Scoring"),
         x = "PC1", y = "PC2")
  
  list(
    plot = p,
    outliers = pc_df %>% slice(1:5),
    tiers = pc_df %>% arrange(cluster, desc(distance))
  )
}

# Run Analysis by Position
qb_result <- analyze_position(my_scrape[["QB"]], "QB")
rb_result <- analyze_position(my_scrape[["RB"]], "RB")
wr_result <- analyze_position(my_scrape[["WR"]], "WR")
te_result <- analyze_position(my_scrape[["TE"]], "TE")

# === RESULTS ===

# View Plots
print(qb_result$plot)
print(rb_result$plot)
print(wr_result$plot)
print(te_result$plot)

# View Top 5 Outliers
cat("\n--- QB Outliers ---\n"); print(qb_result$outliers)
cat("\n--- RB Outliers ---\n"); print(rb_result$outliers)
cat("\n--- WR Outliers ---\n"); print(wr_result$outliers)
cat("\n--- TE Outliers ---\n"); print(te_result$outliers)

# View Top Players by Tier
cat("\n--- Top 3 Players per Tier (QB) ---\n")
print(qb_result$tiers %>% group_by(cluster) %>% slice_max(order_by = distance, n = 3))
cat("\n--- Top 3 Players per Tier (RB) ---\n")
print(rb_result$tiers %>% group_by(cluster) %>% slice_max(order_by = distance, n = 3))
cat("\n--- Top 3 Players per Tier (WR) ---\n")
print(wr_result$tiers %>% group_by(cluster) %>% slice_max(order_by = distance, n = 3))
cat("\n--- Top 3 Players per Tier (TE) ---\n")
print(te_result$tiers %>% group_by(cluster) %>% slice_max(order_by = distance, n = 3))


analyze_flex <- function(rb_data, wr_data, te_data, n_clusters = 5) {
  # Combine and clean data
  all_flex <- bind_rows(rb_data, wr_data, te_data)
  
  features <- all_flex %>%
    select(player, pos, rush_att, rush_yds, rush_tds,
           rec, rec_yds, rec_tds) %>%
    drop_na()
  
  # Scale and weight
  flex_matrix <- as.matrix(features[, 3:8])
  scaled <- scale(flex_matrix)
  weighted <- scaled %*% diag(as.numeric(flex_weights))
  
  # Clustering
  dists <- dist(weighted)
  hc <- hclust(dists, method = "complete")
  clusters <- cutree(hc, k = n_clusters)
  
  # PCA
  pca <- prcomp(weighted, scale. = FALSE)
  pc_df <- data.frame(
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    player = features$player,
    pos = features$pos,
    cluster = as.factor(clusters)
  )
  
  pc_df$distance <- sqrt(pc_df$PC1^2 + pc_df$PC2^2)
  pc_df <- pc_df %>%
    arrange(desc(distance)) %>%
    mutate(outlier_rank = row_number())
  
  # Plot
  p <- ggplot(pc_df, aes(x = PC1, y = PC2, color = cluster, shape = pos, label = player)) +
    geom_point(size = 3) +
    geom_text_repel(max.overlaps = 15, size = 3) +
    theme_minimal() +
    labs(title = "FLEX Clustering: RB + WR + TE", x = "PC1", y = "PC2")
  
  list(
    plot = p,
    outliers = pc_df %>% slice(1:5),
    tiers = pc_df %>% arrange(cluster, desc(distance))
  )
}

flex_weights <- c(
  rush_att = 0,       # optional usage signal
  rush_yds = 0.1,     # 1 pt / 10 yds
  rush_tds = 6,       # 6 pts / TD
  rec = 0.5,          # half-PPR
  rec_yds = 0.1,      # 1 pt / 10 yds
  rec_tds = 6         # 6 pts / TD
)

flex_result <- analyze_flex(my_scrape[["RB"]], my_scrape[["WR"]], my_scrape[["TE"]])

# View Flex Results
print(flex_result$plot)

cat("\n--- FLEX Outliers ---\n")
print(flex_result$outliers)

cat("\n--- Top 5 per Tier (FLEX) ---\n")
print(flex_result$tiers %>% group_by(cluster) %>% slice_max(order_by = distance, n = 5))


cat("\n=== TOP PASS-CATCHING RBs (2025 Projection) ===\n")

pass_catching_rbs <- my_scrape[["RB"]] %>%
  select(player, team, rush_att, rush_yds, rush_tds,
         rec_tgt, rec, rec_yds, rec_tds) %>%
  drop_na(rec, rec_tgt, rec_yds) %>%
  mutate(
    rec_efficiency = rec / rec_tgt,        # Catch rate
    yards_per_rec = rec_yds / rec,         # Efficiency
    pass_catch_score = rec * 0.5 + rec_yds * 0.1 + rec_tds * 6  # Fantasy weighting
  ) %>%
  arrange(desc(pass_catch_score))

# Print Top 10
print(pass_catching_rbs %>%
        select(player, team, rec_tgt, rec, rec_yds, rec_tds, rec_efficiency, pass_catch_score) %>%
        slice(1:10))
