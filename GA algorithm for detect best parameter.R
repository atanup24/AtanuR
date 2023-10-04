# Load required libraries
library(GA)  # for genetic algorithm
library(caret)  # for modeling and resampling

# Load your dataset
# Replace 'your_dataset.csv' with the actual filename and path
data <- read.csv("C:/Users/atanupanja/Desktop/SGR_combined.csv")
table(data$SGR)

# Define the fitness function
# In this function, you should specify how you want to evaluate the quality of parameter combinations
# You can use any appropriate metric (e.g., RMSE, R-squared, MAE) depending on your modeling approach (e.g., linear regression, random forest)
# The lower the fitness value, the better the parameter combination
fitness_function <- function(params) {
  # Debugging: Print column names and selected parameters
  print(names(data))
  print(params)
  
  # Subset the dataset with selected parameters
  selected_data <- data[, params]
  
  # Debugging: Print the dimensions of the selected_data
  print(dim(selected_data))
  
  # Split the data into training and testing sets while ensuring stratified sampling
  # Split the data into training and testing sets with stratified sampling
  set.seed(123)  # for reproducibility
  trainIndex <- createDataPartition(data$SGR, p = 0.7, 
                                    list = FALSE,
                                    times = 1,
                                    groups = data$SGR)  # Use 'SGR' as the strata
  training_data <- data[trainIndex, ]
  testing_data <- data[-trainIndex, ]
  
  # Fit your predictive model (e.g., linear regression)
  model <- lm(SGR ~ ., data = training_data)
  
  # Evaluate the model (use appropriate metric)
  predictions <- predict(model, newdata = testing_data)
  rmse <- sqrt(mean((testing_data$SGR - predictions)^2))
  
  # Minimize RMSE as the fitness value
  return(rmse)
}


# Define parameter space
# Each parameter is represented by a binary value (1 for inclusion, 0 for exclusion)
# You can adjust the parameter space according to your needs
# For example, if you have 5 parameters, you would have a binary string of length 5
# In this example, we include all 5 parameters in the initial population
num_parameters <- 5
parameter_space <- rep(1, num_parameters)

# Set up genetic algorithm parameters
ga_parameters <- ga(
  type = "binary",  # Binary encoding for parameter selection
  fitness = fitness_function,  # Fitness function to minimize RMSE
  nBits = num_parameters,  # Number of bits for each parameter
  maxiter = 50,  # Maximum number of generations
  run = 100,  # Number of runs
  monitor = FALSE  # Disable monitoring for simplicity
)

# Get the best parameter combination
best_params <- ga_parameters@solution
best_param_selection <- parameter_space * best_params

# Print the best parameter selection
cat("Best parameter selection:", best_param_selection, "\n")

# You can now use the best parameter combination to build your final model and make predictions
# Example:
# best_selected_data <- data[, best_param_selection == 1]
# final_model <- lm(SGR ~ ., data = best_selected_data)
# final_predictions <- predict(final_model, newdata = testing_data)
# Create a vector of parameter names corresponding to the selection
parameter_names <- c("Nitrate", "Nitrite", "Phosphate", "Ammonia", "Silicate")

# Plot the best parameter selection as a bar chart
barplot(best_param_selection, names.arg = parameter_names, 
        main = "Best Parameter Selection",
        xlab = "Parameters",
        ylab = "Selected (1) / Not Selected (0)")
# Create a vector of parameter names corresponding to the selection
parameter_names <- c("Nitrate", "Nitrite", "Phosphate", "Ammonia", "Silicate")

# Get the names of the selected parameters
selected_parameter_names <- parameter_names[best_param_selection == 1]

# Print the names of the selected parameters
cat("Best selected parameters:", selected_parameter_names, "\n")

