# Example data (replace with your own data)
nutrient_concentration <- c(0.2, 0.4, 0.6, 0.8, 1.0)  # Nutrient concentration (e.g., g/L)
specific_growth_rate_per_hour <- c(0.02, 0.25, 0.3, 0.35, 0.5)  # Specific growth rate (per hour)

# Define the Monod equation
monod_equation <- function(params, S) {
  ??_max <- params[1]
  Ks <- params[2]
  ??_max * S / (Ks + S)
}

# Define the objective function for optimization
objective_function <- function(params) {
  predicted_growth_rates <- monod_equation(params, nutrient_concentration)
  sum((specific_growth_rate_per_hour - predicted_growth_rates)^2)
}

# Initial parameter estimates
initial_params <- c(0.2, 0.05)

# Nonlinear optimization using optim
fit <- optim(
  par = initial_params,
  fn = objective_function,
  method = "BFGS"
)

# Extract the estimated parameters (??_max and Ks)
estimated_params <- fit$par
Ks_estimate <- estimated_params[2]

# Print Ks
cat("Estimated Ks:", Ks_estimate, "\n")

# Generate a range of nutrient concentrations for plotting
S_range <- seq(0, max(nutrient_concentration), length.out = 100)

# Calculate specific growth rates using the estimated parameters
predicted_growth_rates <- monod_equation(estimated_params, S_range)

# Create a plot
plot(nutrient_concentration, specific_growth_rate_per_hour, pch = 16, col = "blue",
     xlab = "Nutrient Concentration (g/L)", ylab = "Specific Growth Rate (per hour)",
     main = "Monod Model Fit")
lines(S_range, predicted_growth_rates, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Data", "Monod Model"), col = c("blue", "red"), lty = 1, lwd = 2)
# Use the estimated Ks value (replace with your estimated Ks)
Ks_estimate <- 0.05  # Replace with your estimated Ks value

# Select a specific nutrient concentration (replace with the concentration you want)
S_concentration <- 0.3  # Replace with the concentration you're interested in

# Calculate the maximum specific growth rate (??_max) at the selected nutrient concentration
mu_max <- specific_growth_rate_per_hour[nutrient_concentration == S_concentration] /
  (1 - S_concentration / Ks_estimate)

cat("Maximum Specific Growth Rate (??_max) at [S =", S_concentration, "g/L]:", mu_max, "per hour\n")
# Example experimental data
N <- c(100, 200, 400, 800)  # Cell density or biomass concentration measurements (e.g., cells/mL or g/L)
time_intervals <- c(2, 2, 4, 6)  # Time intervals between measurements (in hours)

# Calculate ln(Nt) - ln(N0) for each time interval
ln_Nt_minus_ln_N0 <- log(N[2:length(N)]) - log(N[1:(length(N)-1)])

# Calculate specific growth rates for each time interval
growth_rates <- ln_Nt_minus_ln_N0 / time_intervals

# Calculate the average specific growth rate (??) over the entire growth experiment
average_growth_rate <- mean(growth_rates)

cat("Average Specific Growth Rate (??):", average_growth_rate, "per hour\n")
