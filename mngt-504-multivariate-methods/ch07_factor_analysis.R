library(psych)
library(stats)

# Example 7.1
file_path <- "data/Cost_food.csv"
cost_food <- read.csv(file_path, header = TRUE)
feature_data <- cost_food[, c("CSTS", "CASF", "CLNS", "CVEG", "CFRT", "COFT")]
feature_data_scaled <- scale(feature_data)


### PRINCIPAL METHOD ###


fa_result <- principal(feature_data_scaled, nfactors = 2, rotate = "varimax")
print(fa_result$loadings, cutoff = 0) # book says factor loading of 0.5 can be highlighted

communalities <- fa_result$communality
communalities

factor_scores <- fa_result$scores
factor_scores


### MANUAL METHOD ###


# Table 6.5
cor_matrix <- cor(feature_data_scaled)
cor_matrix

# Table 7.2
pca_result <- prcomp(feature_data_scaled, scale. = TRUE)
eigenvalues <- pca_result$sdev^2
eigenvalues
eigenvectors <- pca_result$rotation
eigenvectors

# pg. 139
nfactors <- 2
loadings_unrotated <- eigenvectors[, 1:nfactors] %*% diag(sqrt(eigenvalues[1:nfactors]))
loadings_unrotated

# pg. 139 (value in parenthesis) 
communalities <- rowSums(loadings_unrotated^2)
communalities

# pg. 140
varimax_result <- varimax(loadings_unrotated, normalize = TRUE)
loadings_rotated <- varimax_result$loadings
loadings_rotated

# G matrix is the rotated loadings
G <- loadings_rotated
cor_matrix_inv <- solve(cor_matrix)
score_coefficients <- t(G) %*% cor_matrix_inv
factor_scores <- feature_data_scaled %*% t(score_coefficients)
colnames(factor_scores) <- c("Factor1", "Factor2")
factor_scores

#Factor Interpretation:
#Factor 1 (Non-produce food): High loadings on CSTS, CASF, CLNS, COFT
#Factor 2 (Produce food): High loadings on CVEG, CFRT

#####

# ml = maximum likelihood
pa_result <- fa.parallel(feature_data_scaled, fa = "both", fm = "ml", n.iter = 100)

nfactors <- pa_result$nfact
fa_result <- principal(feature_data_scaled, nfactors = nfactors, rotate = "varimax")

#Factor Loadings
loadings_matrix <- matrix(fa_result$loadings, nrow = 6)
rownames(loadings_matrix) <- c("CSTS", "CASF", "CLNS", "CVEG", "CFRT", "COFT")
colnames(loadings_matrix) <- paste0("Factor.", 1:nfactors)
result_table <- data.frame(
  round(loadings_matrix, 3)
)

for (i in 1:nfactors) {
  col <- paste0("Factor ", i)
  result_table[[col]][abs(loadings_matrix[, i]) > 0.5]
}
print(result_table)

communalities <- fa_result$communality
cat("\nCommunalities:\n")
print(round(communalities, 3))

factor_scores <- fa_result$scores
colnames(factor_scores) <- paste0("Factor.", 1:nfactors)
cat("\nFactor Scores:\n")
print(round(factor_scores, 3))





