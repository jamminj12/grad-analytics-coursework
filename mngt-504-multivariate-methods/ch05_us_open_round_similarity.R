
# How similar the rounds of two golfers were to each other

library(pheatmap)

scores <- data.frame(
  Player = c("J.J. Spaun", "Robert MacIntyre", "Viktor Hovland", "Carlos Ortiz", 
             "Tyrrell Hatton", "Cameron Young", "Jon Rahm", "Scottie Scheffler", 
             "Sam Burns", "Russell Henley"),
  Hole1 = c(5, 4, 4, 4, 4, 4, 4, 4, 5, 4),
  Hole2 = c(5, 4, 4, 4, 4, 4, 4, 4, 5, 4),
  Hole3 = c(5, 4, 4, 4, 4, 4, 3, 4, 5, 4),
  Hole4 = c(4, 4, 5, 5, 5, 5, 4, 5, 5, 5),
  Hole5 = c(5, 4, 4, 4, 4, 4, 4, 4, 5, 4),
  Hole6 = c(4, 3, 3, 3, 3, 3, 2, 3, 4, 3),
  Hole7 = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
  Hole8 = c(3, 2, 3, 3, 3, 3, 3, 3, 3, 3),
  Hole9 = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
  Hole10 = c(4, 4, 4, 4, 4, 4, 4, 4, 3, 4),
  Hole11 = c(4, 4, 5, 4, 4, 4, 4, 4, 6, 5),
  Hole12 = c(4, 5, 5, 5, 5, 5, 4, 5, 5, 5),
  Hole13 = c(3, 3, 3, 3, 2, 3, 3, 3, 3, 3),
  Hole14 = c(3, 2, 4, 3, 4, 4, 3, 4, 4, 4),
  Hole15 = c(4, 4, 4, 6, 4, 4, 4, 4, 6, 5),
  Hole16 = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
  Hole17 = c(3, 3, 3, 4, 5, 4, 3, 3, 4, 4),
  Hole18 = c(3, 4, 4, 4, 4, 4, 4, 4, 4, 4),
  stringsAsFactors = FALSE
)

# commented out because euclidean distance provided bad results
#euclidean_dist <- as.matrix(dist(scores[, 2:19], method = "euclidean"))
#euclidean_dist

#euclidean_similarity <- 1 / (1 + euclidean_dist)

#euclidean_similarity <- round(euclidean_similarity, 3)
#euclidean_similarity

#https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient
spearman_similarity <- cor(t(scores[, 2:19]), method = "spearman")

# Set row and column names
rownames(spearman_similarity) <- scores$Player
colnames(spearman_similarity) <- scores$Player

# Create heatmap using pheatmap
pheatmap(spearman_similarity,
         color = colorRampPalette(c("red", "white", "blue"))(100),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = TRUE,
         number_format = "%.2f",
         main = "Spearman Correlation Similarity Between Golfers",
         fontsize = 8,
         fontsize_row = 8,
         fontsize_col = 8,
         angle_col = 45)

# for further inspection
golfer1 <- "Viktor Hovland"
golfer2 <- "Scottie Scheffler"
plot(1:18, scores[scores$Player == golfer1, 2:19], type = "b", col = "blue", lwd = 2,
     ylim = range(scores[, 2:19]), xlab = "Hole", ylab = "Score", main = "Score Profiles for Two Golfers")
lines(1:18, scores[scores$Player == golfer2, 2:19], type = "b", col = "red", lwd = 2)
legend("topleft", legend = c(golfer1, golfer2), col = c("blue", "red"), lty = 1, lwd = 2, inset = c(-0.2, 1.2), xpd = TRUE)

