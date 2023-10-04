# Load required libraries
library(ggplot2)
library(dplyr)

# Import your CSV dataset
data <- read.csv("phytoplankton_data_with_growth_rate.csv")

# Create a function to plot box plots for monthly variation comparison
plot_monthly_variation_box <- function(data, variables) {
  p <- ggplot(data, aes(x = as.factor(Month), y = .data[[variables]])) +
    geom_boxplot(fill = "blue", alpha = 0.7) +
    labs(x = "Month", y = variables, title = paste("Monthly Variation of", variables)) +
    scale_x_discrete(breaks = 1:12, labels = month.name) +  # Label x-axis with month names
    theme_classic()
  
  # Save the plot as a TIFF file
  tiff_filename <- paste(variables, ".tiff", sep = "")
  tiff(tiff_filename, width = 8, height = 6, units = "in", res = 300)
  print(p)
  dev.off()
}

# Create and save box plots for each variable
variables_to_plot <- c("GR_original", "Growth_Rate","Carbon")

for (variable in variables_to_plot) {
  plot_monthly_variation_box(data, variable)
}
