
# All functions from Appendix: Multivariate Distance/Similarity Measures in R

library(vegan)
library(ecodist)
library(ade4)
library(labdsv)
library(biotools)
library(cultevo)
library(ape)

file_path <- "data/Sparrows.csv"
sparrows <- read.csv(file_path, header = TRUE)

# sparrow features data
feature_data <- sparrows[, c("X1", "X2", "X3", "X4", "X5")]
# Standardizing variables
feature_std <- scale(feature_data, center = TRUE, scale = TRUE)
feature_std


#############################################################################

# A.1.1 Distances between Individual Observations

## FUNCTIONS:

# dist (from vegan)
euclidean_dist <- dist(feature_std, method = "euclidean")
euclidean_dist

# vegdist (from vegan) - Euclidean distance
euclidean_vegdist <- vegdist(feature_std, method = "euclidean")
euclidean_vegdist

# decostand (from vegan) - standardization, then vegdist()
feature_decostand_std <- decostand(as.matrix(feature_data), method = "standardize")
euclidean_decostand_vegdist <- vegdist(feature_decostand_std, method = "euclidean")
euclidean_decostand_vegdist

# designdist (from vegan) - custom Euclidean distance
euclidean_designdist <- designdist(feature_std, method = "sqrt(sum((A-B)^2))")
euclidean_designdist

# distance (from ecodist) - Euclidean distance
euclidean_ecodist <- distance(feature_std, method = "euclidean")
euclidean_ecodist

#############################################################################

# A.1.2 Distances between Populations and Samples

features_and_survivorship <- data.frame(Survivorship = sparrows$Survivorship, feature_data)

# Penrose script not finished: 
# Need to locate this script -> source("Penrose.dist.R")
# Reminder: need biotools package

penrose_dist <- Penrose.dist( x = features_and_survivorship, group = features_and_survivorship$Survivorship)
cat(paste("Penrose distances between Survivorship groups:", penrose_dist, "\n"))

# Mahalanobis
group_means <- aggregate(feature_data, by = list(Survivorship = sparrows$Survivorship), mean, na.rm = TRUE)
group_means <- group_means[, -1]
group_means
boxm_result <- boxM(feature_data, sparrows$Survivorship)
boxm_result
pooled_cov <- boxm_result$pooled
pooled_cov

# boxm (from biotools) - using pooled like book
Mahalanobis_Distance <- D2.dist(group_means, cov = pooled_cov)
cat(paste("Mahalanobis distance (D2.dist):", Mahalanobis_Distance, "\n"))
# Mahalanobis distance (D2.dist): 0.23530819190254 (means are similar?)

# distance (from ecodist)
mahalanobis_ecodist <- distance(group_means, icov = pooled_cov, method = "mahalanobis")
cat(paste("Mahalanobis distance (distance):", mahalanobis_ecodist, "\n"))

#############################################################################

# A.1.3 Distances and Similarities for Presence-Absence Data

# NOTE: I AM NOT USING SPARROW DATA FOR THESE BECAUSE THERE IS NOT MULTIPLE BINARY COLUMNS
# USING TABLE 5.5 FROM TEXTBOOK - BUT ADDING A THIRD RANDOM SERIES FOR FUN

Tab5.5 <- rbind(
  Species.1 = c(0, 0, 1, 1, 1, 0, 1, 1, 1, 0),
  Species.2 = c(1, 1, 1, 1, 0, 0, 0, 0, 1, 1),
  Species.3 = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 1) # added this species for fun
)
colnames(Tab5.5) <- paste("Site", 1:10)

# Adapted version of Chapter 5 code that works for all pairs.
for (pair in list(c(1, 2), c(1, 3), c(2, 3))) { # update this pair list here for more species
  sp1 <- pair[1]
  sp2 <- pair[2]
  a <- sum(Tab5.5[sp1,] == 1 & Tab5.5[sp2,] == 1)  # Joint presences
  b <- sum(Tab5.5[sp1,] == 1 & Tab5.5[sp2,] == 0)
  c <- sum(Tab5.5[sp1,] == 0 & Tab5.5[sp2,] == 1)
  d <- sum(Tab5.5[sp1,] == 0 & Tab5.5[sp2,] == 0)  # Joint absences
  N <- ncol(Tab5.5)
  
  # from Section 5.5 / Table 5.6
  SMI <- (a + d) / N
  OI <- a / sqrt((a + b) * (a + c))
  DS <- 2 * a / (2 * a + b + c)
  Jacc <- a / (a + b + c)
  
  cat(sprintf("\nSpecies %d vs. Species %d:\n", sp1, sp2))
  cat(sprintf("  Simple Matching Index: %.2f\n", SMI))
  cat(sprintf("  Ochiai Index: %.2f\n", OI))
  cat(sprintf("  Dice-Sorensen Index: %.2f\n", DS))
  cat(sprintf("  Jaccard Index: %.2f\n", Jacc))
}

# Jaccard distance (default from stats)
jaccard_stats <- dist(Tab5.5, method = "binary")
jaccard_stats

# Jaccard distance (from vegan)
jaccard_vegdist <- vegdist(Tab5.5, method = "jaccard")
jaccard_vegdist

# Jaccard distance (from ade4) https://www.rdocumentation.org/packages/ade4/versions/1.7-23/topics/dist.binary
jaccard_ade4 <- dist.binary(Tab5.5, method = 1)  # 1 = Jaccard index (1901)
jaccard_ade4

# Jaccard distance (from labdsv) - (Steinhaus = Jaccard)
jaccard_labdsv <- dsvdis(Tab5.5, index = "steinhaus")
jaccard_labdsv

# Jaccard distance (from ecodist)
jaccard_ecodist <- distance(Tab5.5, method = "jaccard")
jaccard_ecodist

# Jaccard similarity (s = 1 - d)
jaccard_similarity <- 1 - as.matrix(jaccard_stats)
jaccard_similarity

# Simple matching coefficient (from ade4)
simple_matching_ade4 <- dist.binary(Tab5.5, method = 2)  # 2 = Simple matching coefficient of Sokal & Michener (1958)
simple_matching_similarity <- 1 - simple_matching_ade4^2
simple_matching_similarity

# Ochiai similarity (from ade4)
ochiai_ade4 <- dist.binary(Tab5.5, method = 7)  # 7 = Ochiai (1957)
ochiai_similarity <- 1 - ochiai_ade4^2
ochiai_similarity

# Dice-Sorensen similarity (from labdsv)
dice_labdsv <- dsvdis(Tab5.5, index = "sorensen")
dice_similarity <- 1 - as.matrix(dice_labdsv)
dice_similarity

# Dice-Sorensen distance (from ecoddist)
dice_ecodist <- distance(Tab5.5, method = "sorensen")
dice_similarity_ecodist <- 1 - as.matrix(dice_ecodist)
dice_similarity_ecodist

#############################################################################

# A.2 Mantel Randomization Test

diet_file <- "data/Country_diets.csv"
diet_data <- read.csv(diet_file, header = TRUE)

feature_data <- diet_data[, c("CSTS", "CASF", "CLNS", "CVEG", "CFRT", "COFT")]

group_means <- as.matrix(feature_data)

pooled_cov <- cov(feature_data, use = "complete.obs")

# Mahalanobis distance (from biotools)
mahalanobis_dist <- D2.dist(group_means, cov = pooled_cov)
mahalanobis_dist

# Had "coordinates" of each country generated from ChatGPT -> did not double check if thesea are correct
library(geosphere)
coords <- data.frame(
  Country = diet_data$Country,
  Lat = c(
    41.3275,  # Albania (Tirana)
    48.2082,  # Austria (Vienna)
    53.9045,  # Belarus (Minsk)
    50.8503,  # Belgium (Brussels)
    43.8486,  # Bosnia and Herzegovina (Sarajevo)
    42.6977,  # Bulgaria (Sofia)
    45.8150,  # Croatia (Zagreb)
    50.0755,  # Czechia (Prague)
    55.6761,  # Denmark (Copenhagen)
    59.4370,  # Estonia (Tallinn)
    60.1695,  # Finland (Helsinki)
    48.8566,  # France (Paris)
    52.5200,  # Germany (Berlin)
    37.9838,  # Greece (Athens)
    47.4979,  # Hungary (Budapest)
    64.1466,  # Iceland (Reykjavik)
    53.3498,  # Ireland (Dublin)
    41.9028,  # Italy (Rome)
    56.9496,  # Latvia (Riga)
    54.6872,  # Lithuania (Vilnius)
    49.6116,  # Luxembourg (Luxembourg)
    35.8989,  # Malta (Valletta)
    42.4413,  # Montenegro (Podgorica)
    52.3676,  # Netherlands (Amsterdam)
    41.6086,  # North Macedonia (Skopje)
    59.9139,  # Norway (Oslo)
    52.2297,  # Poland (Warsaw)
    38.7223,  # Portugal (Lisbon)
    47.1569,  # Republic of Moldova (Chisinau)
    44.4268,  # Romania (Bucharest)
    55.7558,  # Russian Federation (Moscow)
    44.7866,  # Serbia (Belgrade)
    48.1486,  # Slovakia (Bratislava)
    46.0569,  # Slovenia (Ljubljana)
    40.4168,  # Spain (Madrid)
    59.3293,  # Sweden (Stockholm)
    46.9480,  # Switzerland (Bern)
    51.5074   # United Kingdom (London)
  ),
  Lon = c(
    19.8187,   # Albania
    16.3738,   # Austria
    27.5615,   # Belarus
    4.3517,    # Belgium
    18.4131,   # Bosnia and Herzegovina
    23.3241,   # Bulgaria
    15.9819,   # Croatia
    14.4378,   # Czechia
    12.5683,   # Denmark
    24.7536,   # Estonia
    24.9384,   # Finland
    2.3522,    # France
    13.4050,   # Germany
    23.7275,   # Greece
    19.0402,   # Hungary
    -21.9426,  # Iceland
    -6.2603,   # Ireland
    12.4964,   # Italy
    24.1052,   # Latvia
    25.2797,   # Lithuania
    6.1296,    # Luxembourg
    14.5058,   # Malta
    19.2594,   # Montenegro
    4.9041,    # Netherlands
    21.9994,   # North Macedonia
    10.7522,   # Norway
    21.0122,   # Poland
    -9.1393,   # Portugal
    28.8353,   # Republic of Moldova
    26.1025,   # Romania
    37.6173,   # Russian Federation
    20.4573,   # Serbia
    17.1077,   # Slovakia
    14.5058,   # Slovenia
    -3.7038,   # Spain
    18.0686,   # Sweden
    7.4474,    # Switzerland
    -0.1278    # United Kingdom
  )
)

# this loop is to calculate the distance between two coordinates. From Online at https://www.rdocumentation.org/packages/geosphere/versions/1.5-20/topics/distHaversine
n <- nrow(coords)
geo_dist <- matrix(0, nrow = n, ncol = n, dimnames = list(coords$Country, coords$Country))
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    dist <- distHaversine(c(coords$Lon[i], coords$Lat[i]), c(coords$Lon[j], coords$Lat[j])) / 1000
    geo_dist[i, j] <- dist
    geo_dist[j, i] <- dist
  }
}

mahalanobis_dist_dist <- as.dist(mahalanobis_dist)
mahalanobis_dist_dist
geo_dist_dist <- as.dist(geo_dist)
geo_dist_dist

# Mantel (from vegan)
mantel_vegan <- vegan::mantel(mahalanobis_dist_dist, geo_dist_dist, method = "pearson", permutations = 9999)

# Mantel (from cultevo)
mantel_cultevo <- cultevo::mantel.test(mahalanobis_dist_dist, geo_dist_dist, nperm = 9999)

# Mantel (from ade4)
mantel_ade4 <- ade4::mantel.rtest(mahalanobis_dist_dist, geo_dist_dist, nrepet = 9999)

# Mantel (from biotools) -- this is the one that makes the fun density plot!
mantel_biotools <- biotools::mantelTest(mahalanobis_dist_dist, geo_dist_dist, nperm = 9999)

# Mantel (from ecodist)
mantel_ecodist <- ecodist::mantel(mahalanobis_dist_dist ~ geo_dist_dist, nperm = 999)

