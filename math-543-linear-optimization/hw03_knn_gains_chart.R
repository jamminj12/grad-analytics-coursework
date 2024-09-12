## Problem 3.7

library(ggplot2)
library(lattice)
library(gains)
library (caret)
library(readxl)
library(class)
Chapter_03_3_6_data <- read_excel("data/Chapter_03 3-6 data.xlsx", 
                                  skip = 1) # I'm skipping the row that says Original Data
# View(Chapter_03_3_6_data)


dataNorm <- Chapter_03_3_6_data
dataNorm[, -5] <- scale(Chapter_03_3_6_data[, -5])
head(dataNorm)

ind <- sample(2, nrow(dataNorm), replace=TRUE, prob=c(0.6, 0.4))
ind
trainData <- dataNorm[ind==1,]
testData <- dataNorm[ind==2,]
head(testData)

dim(trainData); dim(testData)

KnnTestPrediction <- list()
accuracy <- numeric()

for(k in 1:9){
  
  KnnTestPrediction[[k]] <- knn(trainData[,-5], testData[,-5], 
                                trainData$Graduated, k, prob=TRUE)
  
  accuracy[k] <- sum(KnnTestPrediction[[k]]==testData$Graduated)/length(testData$Graduated)*100
  
}

k_ideal <- which.max(accuracy)

print(k_ideal)# part a answer

KNN_Graduated <- knn(trainData[,-5], testData[,-5], 
                     trainData$Graduated, k = k_ideal)

KNN_Graduated_Prob <- as.numeric(KNN_Graduated == "Yes")

##### From: Gains chart method 2 kNN code_cander.docx

# Combine actual and predicted values
gain_data <- data.frame(
  actual = as.numeric(testData$Graduated == "Yes"),
  predicted = KNN_Graduated_Prob
)

# Sort the data by predicted probabilities
gain_data <- gain_data[order(-gain_data$predicted), ]

# Compute cumulative gain
gain_data$cumulative_actual <- cumsum(gain_data$actual)
gain_data$cumulative_total <- 1:nrow(gain_data)
gain_data$cumulative_percentage <- gain_data$cumulative_actual / 
  sum(gain_data$actual)
gain_data$cumulative_total_percentage <- gain_data$cumulative_total / 
  nrow(gain_data)

# Create a gain chart
ggplot(gain_data, aes(x = cumulative_total_percentage, 
                      y = cumulative_percentage)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Gain Chart",
    x = "Cumulative Percentage of Total",
    y = "Cumulative Gain"
  ) # part b answer

