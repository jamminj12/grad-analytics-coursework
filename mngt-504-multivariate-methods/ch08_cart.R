library(rpart)
library(rpart.plot)

file_path <- "data/Cost_food.csv"
cost_food <- read.csv(file_path, header = TRUE)
feature_data <- cost_food[, c("CSTS", "CASF", "CLNS", "CVEG", "CFRT", "COFT")]
group <- as.factor(cost_food$Group)
countries <- cost_food$Country

cart_model <- rpart(group ~ CSTS + CASF + CLNS + CVEG + CFRT + COFT, 
                    data = cost_food, method = "class")
print(cart_model)

rpart.plot(cart_model, 
           type = 2,          
           extra = 104,        
           fallen.leaves = TRUE,
           main = "CART for Group Classification")
