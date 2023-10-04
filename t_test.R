library(ggplot2)

# Assuming you have a data frame called 'data' with columns "GR_original" and "Growth_Rate"
# Replace 'data' with the actual name of your data frame
your_data <- read.csv("phytoplankton_data_with_growth_rate.csv")
# Combine the two columns into a single data frame for plotting
combined_data <- data.frame(
  Variable = rep(c("GR_original", "Growth_Rate"), each = nrow(your_data)),
  Value = c(your_data$GR_original, your_data$Growth_Rate)
)

# Perform a two-sample t-test
t_test_result <- t.test(your_data$GR_original, your_data$Growth_Rate)
# Check if the p-value is less than alpha (common significance level, e.g., 0.05)
alpha <- 0.05
if (t_test_result$p.value < alpha) {
  cat("Null hypothesis rejected:\n")
  cat("There is a significant difference between", t_test_result$method, "of", t_test_result$alternative, "means.\n")
} else {
  cat("Null hypothesis not rejected:\n")
  cat("There is no significant difference between", t_test_result$method, "of", t_test_result$alternative, "means.\n")
}

# Print the confidence interval
cat("Confidence Interval (95%): [", t_test_result$conf.int[1], "-", t_test_result$conf.int[2], "]\n")

# Print the t-statistic and degrees of freedom
cat("t-statistic:", t_test_result$statistic, "\n")
cat("Degrees of Freedom:", t_test_result$parameter, "\n")

# Print the t-test result
print(t_test_result)

# Create a data frame to store the confidence interval
confidence_interval_data <- data.frame(
  Variable = "Difference",
  Value = t_test_result$estimate,
  Lower_CI = t_test_result$conf.int[1],
  Upper_CI = t_test_result$conf.int[2]
)

# Create a bar plot with error bars representing the confidence interval
p<-ggplot(confidence_interval_data, aes(x = Variable, y = Value, ymin = Lower_CI, ymax = Upper_CI)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  geom_errorbar(width = 0.2, color = "black") +
  labs(x = "", y = "Difference") +
  ggtitle("Confidence Interval of the Difference") +
  theme_minimal()

ggsave("confidence_interval_plot.tiff", plot = p, device = "tiff", width = 6, height = 4, dpi = 300)

# Plot a box plot to compare the two columns
p1<-ggplot(combined_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(x = "", y = "Value") +
  ggtitle("Box Plot of GR_original and Growth_Rate") +
  theme_classic()
ggsave("Box Plot of GR_original and Growth_Rate.tiff", plot = p1, device = "tiff", width = 6, height = 4, dpi = 300)