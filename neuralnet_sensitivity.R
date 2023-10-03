library(neuralnet)
library(Metrics)
data <- read.csv("Timseries_data_cbleach.csv")
# Split the dataset into training (80%) and testing (20%) sets
# Step 3: Preprocess the data
set.seed(123)
sample_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[sample_index, ]
test_data <- data[-sample_index, ]

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

train_data[, c("fco2_norm", "pH_norm", "Salinity_norm", "SSTA_norm", "DO_norm", "talk_norm", "tco2_norm")] <- lapply(train_data[, c("fco2_norm", "pH_norm", "Salinity_norm", "SSTA_norm", "DO_norm", "talk_norm", "tco2_norm")], normalize)
test_data[, c("fco2_norm", "pH_norm", "Salinity_norm", "SSTA_norm", "DO_norm", "talk_norm", "tco2_norm")] <- lapply(test_data[, c("fco2_norm", "pH_norm", "Salinity_norm", "SSTA_norm", "DO_norm", "talk_norm", "tco2_norm")], normalize)

max_bleach <- max(train_data$bleach_norm)
train_data$bleach_norm <- train_data$bleach_norm / max_bleach
test_data$bleach_norm <- test_data$bleach_norm / max_bleach

# Step 4: Create and train the neural network model
nn <- neuralnet(bleach_norm ~ fco2_norm + pH_norm + Salinity_norm + SSTA_norm + DO_norm + talk_norm + tco2_norm,
                data = train_data, hidden = c(5, 5), linear.output = TRUE)

# Step 5: Make predictions and evaluate the model
test_predictions <- predict(nn, test_data)
test_predictions <- test_predictions * max_bleach

correlation <- cor(test_data$bleach_norm, test_predictions)
rsquared_value <- correlation^2

cat("R-squared:", rsquared_value, "\n")

rmse_value <- rmse(test_data$bleach_norm, test_predictions)
mae_value <- mae(test_data$bleach_norm, test_predictions)
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")


# Initialize a data frame to store sensitivity results
sensitivity_df <- data.frame(Parameter = character(0), Value = numeric(0), Predicted_bleach_norm = numeric(0))

# Define a custom function to calculate sensitivity
calculate_sensitivity <- function(parameter_name, values) {
  sensitivity_values <- numeric(length(values))
  
  for (i in 1:length(values)) {
    # Create a copy of the data frame
    test_data_copy <- test_data
    
    # Modify the specified parameter with varying values
    test_data_copy[[parameter_name]] <- values[i]
    
    # Predict bleach_norm for the modified data frame
    test_predictions <- predict(nn, test_data_copy)
    
    # Denormalize the predictions
    test_predictions <- test_predictions * max_bleach
    
    # Calculate the mean predicted bleach_norm
    sensitivity_result <- mean(test_predictions)
    
    sensitivity_values[i] <- sensitivity_result
  }
  
  sensitivity_df <<- rbind(sensitivity_df, data.frame(Parameter = parameter_name, Value = values, Predicted_bleach_norm = sensitivity_values))
}

# Perform sensitivity analysis for each parameter
parameters_to_analyze <- c("fco2_norm", "pH_norm", "Salinity_norm", "SSTA_norm", "DO_norm", "talk_norm", "tco2_norm")

for (parameter in parameters_to_analyze) {
  values <- unique(test_data[[parameter]])  # Get unique values of the parameter from the test_data
  calculate_sensitivity(parameter, values)
}

# Plot sensitivity analysis results for each parameter
library(ggplot2)

# Define a custom color palette for the lines
custom_colors <- c("red", "blue", "green", "purple", "orange", "brown", "darkolivegreen")

# Plot sensitivity analysis results for each parameter with dark colors and increased line width
ggplot(sensitivity_df, aes(x = Value, y = Predicted_bleach_norm, color = Parameter)) +
  geom_line(linewidth = 1.5) +  # Increase line width to 1.5 (you can adjust the value)
  labs(x = "Parameter Value", y = "Predicted bleach_norm", title = "Sensitivity Analysis for Parameters") +
  theme_classic() +
  scale_color_manual(values = custom_colors, name = "Parameter")
