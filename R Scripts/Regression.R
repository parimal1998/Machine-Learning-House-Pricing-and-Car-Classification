#Instruction: Run through the functions file before starting with this file.
#Including the function file to keep it clean

#importing the data set into R file
library(readxl)
Valuation <- read_excel("Dataset/Valuation_Regression.xlsx")

#Detecting the structure and summary of the data set
str(Valuation)
summary(Valuation)

#Checking if there is any blank row in data set
blank_counts <- sapply(Valuation, function(x) sum(x == ""))
print(blank_counts)

#Re-coding the variables for easy modelling and programming
library(dplyr)

#Changing the dates from decimal to date format
Valuation$`X1 transaction date` <- convert_decimal_to_year_month(Valuation$`X1 transaction date`)

#Changing the column names
colnames(Valuation) <- c("Row_No", "Date", "House_Age", "Dist_to_station", "No_of_stores", "Latitude", "Longitude", "House_price_unit")

#Checking the column names 
colnames(Valuation)

#Converting the house prices as currently it is in area format
Valuation$House_Price <- (Valuation$House_price_unit*1000) / 3.3

#Checking for some potential outliers in the data set
library(ggplot2)
ggplot(Valuation, aes(House_Price)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  theme_minimal()

#Checking the relation between Dist to station and House Price
ggplot(Valuation, aes(x = Dist_to_station, y = House_Price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

#Removing the outliers from the data set

Valuation <- Valuation %>%
  filter(House_Price >= 3000 & House_Price <= 25000)

#Checking the box plot for house price
ggplot(Valuation, aes(y = House_Price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of House Prices", y = "House Price") +
  theme_minimal()

library(corrplot)
corrplot(cor(Valuation[, sapply(Valuation, is.numeric)]), method = "color")
cor(Valuation[c("House_Age", "Dist_to_station", "No_of_stores", "Latitude", "Longitude", "House_Price")])

#Building a pair plot to check the relations between variables
pairs(Valuation[c("House_Age", "Dist_to_station", "No_of_stores", "Latitude", "Longitude", "House_Price")])

#Checking the distribution of every variable using psych
#install.packages("psych")
library(psych)
pairs.panels(Valuation[c("House_Age", "Dist_to_station", "No_of_stores", "Latitude", "Longitude", "House_Price")])

#Removing Row column from data set 
Valuation_Cleared <- Valuation
Valuation_Cleared <- Valuation_Cleared %>% select(-Date, -Row_No, -House_price_unit)

#Splitting the train and test data set
set.seed(123)
trainIndex <- sample(nrow(Valuation_Cleared), 0.75*nrow(Valuation_Cleared), replace = FALSE)
train <- Valuation_Cleared[trainIndex,]
test <- Valuation_Cleared[-trainIndex,]

#Building the regression Model
Regression_Model <- lm(House_Price ~ ., data = train)
Regression_Model
summary(Regression_Model)

#Improving the performance of Model
Valuation_Cleared$House_Age2 <- Valuation_Cleared$House_Age^2
Valuation_Cleared$Dist_to_station2 <- Valuation_Cleared$Dist_to_station^2
Valuation_Cleared$Store_gt_5 <- ifelse(Valuation_Cleared$No_of_stores >= 5, 1, 0)

#Creating new test and train set for the updated model
train_improved <- Valuation_Cleared[trainIndex,]
test_improved <- Valuation_Cleared[-trainIndex,]
Regression_Model_Improved <- lm(House_Price ~ ., data = train_improved)
summary(Regression_Model_Improved)

# Predicting Original model
pred_original <- predict(Regression_Model, newdata = test)

# Predicting Improved model
pred_improved <- predict(Regression_Model_Improved, newdata = test_improved)

#Adding actual test values
actual <- test$House_Price

# Calculating the metrics for both the model
library(Metrics)
metrics_original <- evaluate_model(actual, pred_original)
metrics_improved <- evaluate_model(actual, pred_improved)

#Printing the results
results <- rbind(Original_Model = metrics_original,
                 Improved_Model = metrics_improved)
print(results)

