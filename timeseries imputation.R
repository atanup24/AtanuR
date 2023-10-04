# Load necessary libraries
library(imputeTS)

# Load your dataset (replace 'your_data.csv' with your actual data file)
data <- read.csv("PIC_SST_timeseries.csv")

# Create a new 'Date' column by combining 'Year' and 'Month'
data$Date <- as.Date(paste(data$Year, data$Month, "01", sep = "-"), format = "%Y-%m-%d")

# Sort the data by date
data <- data[order(data$Date),]

# Define the time series index
index <- zoo::zoo(1:nrow(data), order.by = data$Date)

# Loop through each variable for imputation
variables_to_impute <- c("PIC", "POC", "NPP", "Chlorophyll", "Salinity", "Nitrite", "Oxygen", "Nitrate", "Silicate", "Phosphate", "tco2", "talk", "mean_NSST", "mean_DSST")

for (var in variables_to_impute) {
  # Find NA values
  missing_values <- is.na(data[[var]])
  
  if (sum(missing_values) > 0) {
    # Impute missing values using na_kalman
    data[[var]] <- na_kalman(data[[var]])
  }
}

# Save the imputed data to a new CSV file (replace 'imputed_data.csv' with your desired filename)
write.csv(data, "imputed_data.csv", row.names = FALSE)

# Print a message when the process is complete
cat("Imputation and saving complete. Imputed data saved as 'imputed_data.csv'.\n")
