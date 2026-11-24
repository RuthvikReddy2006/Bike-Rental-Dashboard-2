# Load required packages
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)

bike <- read.csv(file.choose())
# Check original category values
table(bike$season, useNA = "ifany")
table(bike$weathersit, useNA = "ifany")
table(bike$workingday, useNA = "ifany")
table(bike$holiday, useNA = "ifany")
table(bike$yr, useNA = "ifany")

# Convert categorical variables correctly
bike$season <- factor(bike$season)

bike$weathersit <- factor(bike$weathersit)

bike$workingday <- factor(bike$workingday)

bike$holiday <- factor(bike$holiday)

bike$yr <- factor(bike$yr)

# Check converted variables
str(bike)

# Select variables for modelling

# Recreate model data
model_data <- bike[, c("cnt", "temp", "hum", "windspeed",
                       "season", "weathersit", "workingday",
                       "holiday", "weekday", "mnth", "yr")]

# Remove unused factor levels
model_data <- droplevels(model_data)

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(model_data$cnt,
                                   p = 0.80, list = FALSE)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Check dimensions
dim(train_data)
dim(test_data)

# 5.2 Multiple Linear Regression

# Build the linear regression model
linear_model <- lm(cnt ~ ., data = train_data)

# Display model summary
summary(linear_model)

# Predict bike rentals using test data
linear_predictions <- predict(
  linear_model,
  newdata = test_data
)

# Compare actual and predicted values
linear_results <- data.frame(
  Actual = test_data$cnt,
  Predicted = linear_predictions
)

# Display first 10 predictions
head(linear_results, 10)

# Calculate model performance

# RMSE
linear_RMSE <- sqrt(mean(
  (linear_results$Actual -
     linear_results$Predicted)^2
))

# MAE
linear_MAE <- mean(abs(
  linear_results$Actual -
    linear_results$Predicted
))

# R-squared for test data
linear_R2 <- 1 - sum(
  (linear_results$Actual -
     linear_results$Predicted)^2
) / sum(
  (linear_results$Actual -
     mean(linear_results$Actual))^2
)

# Display results
linear_performance <- data.frame(
  Model = "Multiple Linear Regression",
  RMSE = linear_RMSE,
  MAE = linear_MAE,
  R2 = linear_R2
)

print(linear_performance)

library(ggplot2)

ggplot(linear_results,
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Actual vs Predicted Bike Rentals",
    x = "Actual Daily Rentals",
    y = "Predicted Daily Rentals"
  ) +
  theme_minimal()

# 5.3 Decision Tree Regression

# Build Decision Tree model
tree_model <- rpart(
  cnt ~ .,
  data = train_data,
  method = "anova",
  control = rpart.control(
    maxdepth = 5,
    minsplit = 20
  )
)

# Display model summary
print(tree_model)

# Display decision tree
library(rpart.plot)

rpart.plot(
  tree_model,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  main = "Decision Tree for Bike Rental Prediction"
)

# Predict using test data
tree_predictions <- predict(
  tree_model,
  newdata = test_data
)

# Compare actual and predicted values
tree_results <- data.frame(
  Actual = test_data$cnt,
  Predicted = tree_predictions
)

# Display first 10 predictions
head(tree_results, 10)

# Calculate Decision Tree performance

# RMSE
tree_RMSE <- sqrt(mean(
  (tree_results$Actual -
     tree_results$Predicted)^2
))

# MAE
tree_MAE <- mean(abs(
  tree_results$Actual -
    tree_results$Predicted
))

# R-squared
tree_R2 <- 1 - sum(
  (tree_results$Actual -
     tree_results$Predicted)^2
) / sum(
  (tree_results$Actual -
     mean(tree_results$Actual))^2
)

# Display performance
tree_performance <- data.frame(
  Model = "Decision Tree Regression",
  RMSE = tree_RMSE,
  MAE = tree_MAE,
  R2 = tree_R2
)

print(tree_performance)

ggplot(tree_results,
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Decision Tree: Actual vs Predicted Bike Rentals",
    x = "Actual Daily Rentals",
    y = "Predicted Daily Rentals"
  ) +
  theme_minimal()

# 5.4 Random Forest Regression

# Build Random Forest model
set.seed(123)

rf_model <- randomForest(
  cnt ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Display model results
print(rf_model)

# Predict using test data
rf_predictions <- predict(
  rf_model,
  newdata = test_data
)

# Compare actual and predicted values
rf_results <- data.frame(
  Actual = test_data$cnt,
  Predicted = rf_predictions
)

# Display first 10 predictions
head(rf_results, 10)

# Calculate Random Forest performance

# RMSE
rf_RMSE <- sqrt(mean(
  (rf_results$Actual -
     rf_results$Predicted)^2
))

# MAE
rf_MAE <- mean(abs(
  rf_results$Actual -
    rf_results$Predicted
))

# R-squared
rf_R2 <- 1 - sum(
  (rf_results$Actual -
     rf_results$Predicted)^2
) / sum(
  (rf_results$Actual -
     mean(rf_results$Actual))^2
)

# Display performance
rf_performance <- data.frame(
  Model = "Random Forest Regression",
  RMSE = rf_RMSE,
  MAE = rf_MAE,
  R2 = rf_R2
)

print(rf_performance)

# Variable importance graph

varImpPlot(
  rf_model,
  main = "Variable Importance - Random Forest"
)

png("random_forest_importance.png",
    width = 1000,
    height = 700,
    res = 120)

varImpPlot(
  rf_model,
  main = "Variable Importance - Random Forest"
)

dev.off()

ggplot(rf_results,
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Random Forest: Actual vs Predicted Bike Rentals",
    x = "Actual Daily Rentals",
    y = "Predicted Daily Rentals"
  ) +
  theme_minimal()

# 5.5 Compare all three models

model_comparison <- data.frame(
  Model = c(
    "Multiple Linear Regression",
    "Decision Tree Regression",
    "Random Forest Regression"
  ),
  RMSE = c(
    linear_RMSE,
    tree_RMSE,
    rf_RMSE
  ),
  MAE = c(
    linear_MAE,
    tree_MAE,
    rf_MAE
  ),
  R2 = c(
    linear_R2,
    tree_R2,
    rf_R2
  )
)

print(model_comparison)

ggplot(model_comparison,
       aes(x = Model, y = RMSE)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Comparison of RMSE Across Models",
    x = "Machine Learning Model",
    y = "RMSE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    )
  )

# 5.6 Random Forest improvement using cross-validation

library(caret)

# Set up 5-fold cross-validation
cv_control <- trainControl(
  method = "cv",
  number = 5
)

# Try different mtry values
rf_grid <- expand.grid(
  mtry = c(2, 3, 4, 5, 6)
)

# Train and tune Random Forest
set.seed(123)

rf_tuned <- train(
  cnt ~ .,
  data = train_data,
  method = "rf",
  trControl = cv_control,
  tuneGrid = rf_grid,
  ntree = 500,
  metric = "RMSE"
)

# Display cross-validation results
print(rf_tuned)

# Predict using tuned Random Forest
rf_tuned_predictions <- predict(
  rf_tuned,
  newdata = test_data
)

# Calculate test performance
tuned_RMSE <- sqrt(mean(
  (test_data$cnt - rf_tuned_predictions)^2
))

tuned_MAE <- mean(abs(
  test_data$cnt - rf_tuned_predictions
))

tuned_R2 <- 1 - sum(
  (test_data$cnt - rf_tuned_predictions)^2
) / sum(
  (test_data$cnt - mean(test_data$cnt))^2
)

# Display results
tuned_performance <- data.frame(
  Model = "Tuned Random Forest",
  RMSE = tuned_RMSE,
  MAE = tuned_MAE,
  R2 = tuned_R2
)

print(tuned_performance)

print(rf_tuned)