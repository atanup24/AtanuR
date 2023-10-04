# Load necessary libraries
library(tidyverse)

# Read the CSV dataset with nutrient concentrations
nutrient_data <- read.csv("imputed_PIC_SST_data.csv")

# Define the Monod-like growth model
monod_like_growth <- function(Nitrate, Nitrite, Phosphate, K1, K2, K3, Vmax) {
  return(Vmax * (Nitrate / (K1 + Nitrate)) * (Nitrite / (K2 + Nitrite)) * (Phosphate / (K3 + Phosphate)))
}

# Set the Monod constants for Vibrio (adjust as needed)
K1 <- 0.1
K2 <- 0.2
K3 <- 0.3

# Define the maximum and minimum Vmax values
max_Vmax <- 1.0  # Adjust for maximum growth
min_Vmax <- 0.1  # Adjust for minimum growth

# Apply the Monod-like growth model to the dataset for both scenarios
nutrient_data$Vibrio_Max <- with(nutrient_data, monod_like_growth(Nitrate, Nitrite, Phosphate, K1, K2, K3, max_Vmax))
nutrient_data$Vibrio_Min <- with(nutrient_data, monod_like_growth(Nitrate, Nitrite, Phosphate, K1, K2, K3, min_Vmax))

# Rescale the predicted Vibrio populations to 10,000
max_predicted <- max(nutrient_data$Vibrio_Max)
min_predicted <- min(nutrient_data$Vibrio_Min)
rescaling_factor_max <- 10000 / max_predicted
rescaling_factor_min <- 10000 / min_predicted
nutrient_data$Vibrio_Max <- nutrient_data$Vibrio_Max * rescaling_factor_max
nutrient_data$Vibrio_Min <- nutrient_data$Vibrio_Min * rescaling_factor_min

# Plot the predicted Vibrio population trend with both scenarios
ggplot(nutrient_data, aes(x = Year)) +
  geom_line(aes(y = Vibrio_Max), color = "blue", linetype = "solid") +
  geom_line(aes(y = Vibrio_Min), color = "red", linetype = "dashed") +
  labs(x = "Year", y = "Predicted Vibrio Population (Scaled to 10,000)") +
  ggtitle("Predicted Vibrio Population Trend with Maximum and Minimum Growth Scenarios")
