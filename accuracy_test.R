# Sample data of experimental measurements
experimental_data <- c(8.1, 7.9, 8.0, 7.8, 8.2)  # Replace with your experimental measurements

# Calculate the reference value as the mean of experimental measurements
reference_value <- mean(experimental_data)

# Calculate accuracy and precision metrics
accuracy <- reference_value - mean(experimental_data)
precision <- sd(experimental_data)

# Print accuracy and precision
cat("Reference Value (Mean of Experimental Measurements):", reference_value, "\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")

# Visualize the measurements
library(ggplot2)
df <- data.frame(Measurements = experimental_data)
p <- ggplot(df, aes(x = 1, y = Measurements)) +
  geom_boxplot() +
  geom_point(aes(color = "Measurements"), position = position_jitter(width = 0.1)) +
  geom_hline(yintercept = reference_value, linetype = "dashed", color = "red") +
  labs(title = "Experimental Measurements vs. Reference (Mean)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p)
