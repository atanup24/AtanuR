# Sample data (replace this with your dataset)
data <- read.csv("E:/Atanu Kumar Panja/R work/phytoplankton_data_with_growth_rate.csv")

# Perform the Shapiro-Wilk test
shapiro_test <- shapiro.test(data$NPP)

# Set the significance level (alpha)
alpha <- 0.05

# Check if the data is normally distributed
if (shapiro_test$p.value > alpha) {
  cat("Data appears to be normally distributed (fail to reject H0)\n")
} else {
  cat("Data does not appear to be normally distributed (reject H0)\n")
}


# Perform the Shapiro-Wilk test
shapiro_test <- shapiro.test(data$PIC)

# Set the significance level (alpha)
alpha <- 0.05

# Create a bar chart to display the test results
barplot(c(statistic = shapiro_test$statistic, p_value = shapiro_test$p.value), 
        names.arg = c("Test Statistic", "p-value"),
        col = c("blue", "red"),
        main = "Shapiro-Wilk Test Result",
        ylim = c(0, max(shapiro_test$statistic, shapiro_test$p.value) + 0.1),
        ylab = "Value")

# Add a horizontal line at alpha level for significance
abline(h = alpha, col = "green", lty = 2)

# Add text labels
text(1, shapiro_test$statistic + 0.02, round(shapiro_test$statistic, 4), col = "blue", pos = 3)
text(2, shapiro_test$p.value + 0.02, round(shapiro_test$p.value, 4), col = "red", pos = 3)

# Add significance indicator
if (shapiro_test$p.value > alpha) {
  text(1.5, alpha + 0.02, "Not Significant", col = "green")
} else {
  text(1.5, alpha + 0.02, "Significant", col = "red")
}
