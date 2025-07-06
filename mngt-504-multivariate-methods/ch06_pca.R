
# All functions from Appendix:  Principal Components Analysis (PCA) in R

library(stats)   
library(FactoMineR) 
library(ade4)      
library(vegan)      
library(amap)
library(ExPosition)
library(h2o)
library(PCAtools)

file_path <- "data/Sparrows.csv"
sparrows <- read.csv(file_path, header = TRUE)

feature_data <- sparrows[, c("X1", "X2", "X3", "X4", "X5")]
feature_data_scaled <- scale(feature_data, center = TRUE, scale = TRUE)

# 1. princomp (from stats)
# Similar methodology to section 6.2
pca_princomp <- princomp(feature_data, cor = TRUE) # cor = TRUE for correlation matrix
summary(pca_princomp) 
print(pca_princomp$loadings) 
princomp_scores <- predict(pca_princomp)
plot(pca_princomp) 
biplot(pca_princomp, main = "Biplot: princomp") 
plot(princomp_scores[, 1], princomp_scores[,data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC 2], 
     col = ifelse(sparrows$Survivorship == "S", "green", "red"), 
     pch = 19, 
     xlab = "PC1", 
     ylab = "PC2", 
     main = "Survivorship Plot: princomp (PC1 vs PC2)")
legend("topright", 
       legend = c("Survived", "Not Survived"), 
       col = c("green", "red"), 
       pch = 19)

# 2. prcomp (from stats)
# prcomp (SVD) is numerically more accurate
pca_prcomp <- prcomp(feature_data, scale. = TRUE) # scale. = TRUE for correlation matrix
summary(pca_prcomp) 
print(pca_prcomp$rotation) 
prcomp_scores <- predict(pca_prcomp)
plot(pca_prcomp, type = "l", main = "Scree Plot: prcomp") 
biplot(pca_prcomp, main = "Biplot: prcomp")
plot(prcomp_scores[, 1], prcomp_scores[, 2], 
     col = ifelse(sparrows$Survivorship == "S", "green", "red"), 
     pch = 19, 
     xlab = "PC1", 
     ylab = "PC2", 
     main = "Survivorship Plot: prcomp (PC1 vs PC2)")
legend("bottomright", 
       legend = c("Survived", "Not Survived"), 
       col = c("green", "red"), 
       pch = 19)

# 3. PCA (from FactoMineR)
pca_factominer <- PCA(feature_data, scale.unit = TRUE, graph = FALSE) # scale.unit = TRUE for correlation matrix
print(pca_factominer$eig) 
print(pca_factominer$var$coord) 
factominer_scores <- pca_factominer$ind$coord 
plot.PCA(pca_factominer, choix = "ind", title = "PCA Plot: FactoMineR (Individuals)") 
plot.PCA(pca_factominer, choix = "var", title = "PCA Plot: FactoMineR (Variables)") 

# 4. dudi.pca (from ade4)
pca_ade4 <- dudi.pca(feature_data, scale = TRUE, scannf = FALSE, nf = 2) # doing first two components. 
summary(pca_ade4) 
print(pca_ade4$co) 
ade4_scores <- pca_ade4$li 
s.label(pca_ade4$li, clabel = 1, boxes = FALSE)
title("PCA Plot: ade4 (Individuals)")
s.arrow(pca_ade4$co, clabel = 1, boxes = FALSE)
title("PCA Plot: ade4 (Variables)")

# 5. rda (from vegan)
pca_vegan <- rda(feature_data, scale = TRUE) # scale = TRUE for correlation matrix
summary(pca_vegan) 
print(scores(pca_vegan, display = "species")) 
vegan_scores <- scores(pca_vegan, display = "sites") 
plot(pca_vegan, type = "p", main = "PCA Plot: vegan (Individuals)") 
biplot(pca_vegan, main = "Biplot: vegan") 

# 6. acp (from amap)
pca_amap <- acp(feature_data_scaled)
print(pca_amap$eig) 
print(pca_amap$loadings) 
amap_scores <- pca_amap$scores 
plot(pca_amap$scores[, 1:2], main = "PCA Plot: amap (Individuals)", xlab = "PC1", ylab = "PC2")
arrows(x0 = 0, y0 = 0, x1 = pca_amap$loadings[, 1] * 3, y1 = pca_amap$loadings[, 2] * 3, col = "red", length = 0.1)

# 7. epPCA (ExPosition)
pca_exposition <- epPCA(feature_data_scaled, scale = FALSE, graphs = FALSE)
print(pca_exposition$ExPosition.Data$eig)
print(pca_exposition$ExPosition.Data$fj) 
exposition_scores <- pca_exposition$ExPosition.Data$fi
plot(exposition_scores[, 1], exposition_scores[, 2], 
     main = "PCA Plot: ExPosition (Individuals)", xlab = "PC1", ylab = "PC2", pch = 19)

# 8. h2o.prcomp (from h2o)
# having trouble with this one. 

# 9. pca (from PCAtools)
pca_pcatools <- pca(feature_data)
print(pca_pcatools$eig)
print(pca_pcatools$loadings)
pcatools_scores <- pca_pcatools$rotated

# Compare eigenvalues across methods
eigenvalues <- data.frame(
  princomp = pca_princomp$sdev^2,
  prcomp = pca_prcomp$sdev^2,
  factominer = pca_factominer$eig[, 1],
  ade4 = pca_ade4$eig,
  vegan = pca_vegan$CA$eig,
  # getting weird values for the three packages below. couldn't figure out.
  amap = pca_amap$eig,
  ExPosition = pca_exposition$ExPosition.Data$eig,
  PCAtools = pca_pcatools$eig
)
print("Eigenvalues Comparison:")
print(eigenvalues)

