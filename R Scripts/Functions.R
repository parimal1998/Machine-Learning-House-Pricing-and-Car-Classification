#Function to replace single value in a column
replace_value <- function(data, column_name, old_value, new_value) {
  data <- data %>%
    mutate(
      {{ column_name }} := ifelse({{ column_name }} == old_value, new_value, {{ column_name }})
    )
  return(data)
}

count_plot <- function(df, categorical_cols) {
  # Loop through the columns and plot
  for (col in categorical_cols) {
    print(ggplot(df, aes(x = .data[[col]])) + # Using .data for dynamic column names
            geom_bar(aes(fill = .data[[col]])) +
            labs(title = paste("Distribution of", col),
                 x = col,
                 y = "Count") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_viridis_d())
  }
}

#Converting dates from decimal to normal format
convert_decimal_to_year_month <- function(decimal_date) {
  year <- floor(decimal_date)
  month_fraction <- decimal_date - year
  month <- floor(month_fraction * 12) + 1
  return(sprintf("%d-%02d", year, month))
}

#Evaluation Function for Regression
evaluate_model <- function(actual, predicted) {
  r2 <- 1 - sum((predicted - actual)^2) / sum((actual - mean(actual))^2)
  rmse_val <- rmse(actual, predicted)
  mae_val <- mae(actual, predicted)
  return(c(R2 = round(r2, 4), RMSE = round(rmse_val, 4), MAE = round(mae_val, 4)))
}

#Function for evaluation of all Classification Models
generate_crosstab <- function(actual, predicted, row_label = "Actual Default", col_label = "Predicted Default") {
  CrossTable(actual, predicted,
             prop.chisq = FALSE,
             prop.r = FALSE,
             prop.c = FALSE,
             dnn = c(row_label, col_label))
}
