# Load necessary libraries
library(deSolve)
library(igraph)

# Define the Lotka-Volterra predator-prey model equations
lotka_volterra <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dZ <- rZ * Z - aZP * Z * P
    dP <- rP * P * (1 - (P + C) / K) - aPZ * Z * P
    dC <- uP * P
    dI <- uZ * Z
    return(list(c(dZ, dP, dC, dI)))
  })
}

# Set model parameters
parameters <- list(
  rZ = 0.1,   # Zooplankton growth rate
  rP = 0.5,   # Phytoplankton growth rate
  aZP = 0.02, # Zooplankton predation rate on Phytoplankton
  aPZ = 0.01, # Phytoplankton predation rate on Zooplankton
  K = 1000,   # Carrying capacity for Phytoplankton
  uP = 0.1,   # Particulate Organic Carbon (POC) production rate by Phytoplankton
  uZ = 0.05   # Particulate Inorganic Carbon (PIC) production rate by Zooplankton
)

# Set initial conditions
initial_state <- c(Z = 40, P = 100, C = 20, I = 10)

# Set time points for simulation
times <- seq(0, 100, by = 1)

# Run the simulation
output <- ode(y = initial_state, times = times, func = lotka_volterra, parms = parameters)

# Create a network graph to visualize interactions
foodweb <- graph(edges = c(1 - 2, 2 - 1, 2 - 3, 1 - 4))

# Define node names
node_names <- c("Z", "P", "C", "I")

# Add nodes to the graph
foodweb <- add_vertices(foodweb, v = length(node_names), name = node_names)

# Plot the network graph
plot(foodweb, layout = layout.circle)

# Plot the time series of populations
matplot(output[, 1], output[, -1], type = "l", xlab = "Time", ylab = "Population", col = 1:4, lty = 1, lwd = 2)
legend("topright", legend = c("Zooplankton", "Phytoplankton", "POC", "PIC"), col = 1:4, lty = 1, lwd = 2)