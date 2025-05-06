#Instruction: Run through the functions file before starting with this file

#importing the data set into R file
Car <- read.csv("Dataset/Car_Classification.csv", stringsAsFactors = TRUE)

#Displaying basic information about the data set structure and dimensions of data set
str(Car)
dim(Car)
summary(Car)

#Re-coding the variables for easy modelling and programming
library(dplyr)

#Running the function to change value
Car <- replace_value(Car, Doors, "5more", "5+")
Car <- replace_value(Car, Persons, "more", "5+")

#Checking if there is any blank row in data set
blank_counts <- sapply(Car, function(x) sum(x == ""))
print(blank_counts)

#Finding out all the unique values in data set and checking if there is any inconsistency in the file

for (column_name in names(Car)) {
  unique_values_list <- list()
  # Extract the column
  column <- Car[[column_name]]
  
  # Find unique values
  unique_values <- unique(column)
  
  # Store the unique values in the list, using the column name as the key
  unique_values_list[[column_name]] <- unique_values
  cat(column_name, ": ", paste(unique_values, collapse = ", "), "\n")
}

#Building a count chart for checking distribution of all variable
library(ggplot2)

categorical_cols <- c("Buying.Price", "Maintainence", "Doors", "Persons", "Lug_Space", "Safety", "Class")

# Function to run count plot on all the columns present
count_plot(Car, categorical_cols)

#Checking the relationship between Features and Class
ggplot(Car, aes(x = Buying.Price, fill = Class)) +
  geom_bar(position = "fill") +
  labs(title = "Buying Price vs Car Class", y = "Proportion", x = "Buying Price") +
  theme_minimal()

#Building a Decision Tree Model
library(tidyverse)
library(rpart)
library(Matrix)

set.seed(123)
#Building the Train and Validation Dataset
train <- sample(nrow(Car), 0.75*nrow(Car), replace = FALSE)
TrainSet <- Car[train,]
ValidSet <- Car[-train,]
summary(TrainSet)
summary(ValidSet)

#Building the Decision Tree Model
DTmodel <- rpart(Class ~ ., data = TrainSet, method = "class")
print(DTmodel)

#Plotting the Model
plot(DTmodel, uniform = TRUE, main = "Decision Tree for Car Classification")
text(DTmodel, use.n = TRUE, all = TRUE, cex = 0.7, minlength = 4)

#Making Predictions based on DT Model
DTpredictions <- predict(DTmodel, ValidSet, type = "class")

#Evaluation of Model
confusionMatrix(DTpredictions, ValidSet$Class)

#Trying Random Forest classification models to check the accuracy
library(randomForest)
rf_model <- randomForest(Class ~ ., data = TrainSet)
RFpredictions <- predict(rf_model, ValidSet)

#Evaluation of Model
confusionMatrix(RFpredictions, ValidSet$Class)

#Trying the xg-boost classification to check accuracy of model
library(xgboost)
library(caret)


dummies_XG <- dummyVars(Class ~ ., data = Car)
features_matrix <- predict(dummies_XG, newdata = Car)
str(features_matrix)

# Converting to matrix as xgboost doesn't support dataframe
x_data <- as.matrix(features_matrix)
str(x_data)
# Converting Class to numeric labels starting from 0 as xgboost doesn't take factors
y_data <- as.numeric(Car$Class) - 1
str(y_data)

x_train <- x_data[train, ]
y_train <- y_data[train]

x_test <- x_data[-train, ]
y_test <- y_data[-train]

#Training the XGboost model
xgb_model <- xgboost(
  data = x_train,
  label = y_train,
  objective = "multi:softmax",
  num_class = length(unique(y_data)),
  nrounds = 100,
  max_depth = 4,
  eta = 0.3,
  verbose = 0
)

#Making Predictions on the XGboost Model
predictions <- predict(xgb_model, x_test)

# Converting numeric prediction to factor for comparison
predicted_class <- factor(predictions, labels = levels(Car$Class))
actual_class <- factor(y_test, labels = levels(Car$Class))

# Evaluate performance of xgboost model
confusionMatrix(predicted_class, actual_class)

# KNN Model
library(class)

#Performing a loop to find out the best neighbours for KNN
k_values <- 1:45
accuracy_scores <- numeric(length(k_values))
for (i in k_values) {
  knn_pred <- knn(train = x_train, test = x_test, cl = y_train, k = i)
  accuracy_scores[i] <- mean(knn_pred == y_test)
}

# Find best k
best_k <- which.max(accuracy_scores)
cat("Best k =", best_k, "with Accuracy =", round(accuracy_scores[best_k] * 100, 2), "%\n")

# Plotting accuracy vs. k
accuracy_df <- data.frame(k = k_values, Accuracy = accuracy_scores)

ggplot(accuracy_df, aes(x = k, y = Accuracy)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  geom_vline(xintercept = best_k, linetype = "dashed", color = "forestgreen") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "KNN Accuracy vs. K",
       x = "Number of Neighbors (k)",
       y = "Accuracy") +
  theme_minimal()

#Running KNN with best accuracy of 7
knn_pred <- knn(train = x_train, test = x_test, cl = y_train, k = 7)
accuracy <- mean(knn_pred == y_test)
cat("KNN Accuracy:", round(accuracy * 100, 2), "%\n")

#Evaluating all DT models 
library(gmodels)
generate_crosstab(ValidSet$Class, DTpredictions, "Actual Default", "Predicted Default")
generate_crosstab(ValidSet$Class, RFpredictions, "Actual Default", "Predicted Default")
generate_crosstab(ValidSet$Class, predicted_class, "Actual Default", "Predicted Default")
generate_crosstab(ValidSet$Class, knn_pred, "Actual Default", "Predicted Default")

