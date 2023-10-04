# Sample data for three groups
group1 <- c(78, 85, 92, 88, 96)
group2 <- c(65, 70, 72, 63, 75)
group3 <- c(90, 88, 95, 92, 98)

# Perform one-way ANOVA
result_anova <- aov(c(group1, group2, group3) ~ rep(c("Group 1", "Group 2", "Group 3"), each = 5))

# Print ANOVA summary
summary(result_anova)

# Plot the results (box plot)
boxplot(group1, group2, group3, names = c("Group 1", "Group 2", "Group 3"), main = "Box Plot for ANOVA")

# Interpret the results
alpha <- 0.05
p_value <- summary(result_anova)[[1]]$"Pr(>F)"[1]

if (p_value < alpha) {
  cat("Reject the null hypothesis.\n")
  cat("There is a significant difference between at least two groups.\n")
} else {
  cat("Fail to reject the null hypothesis.\n")
  cat("There is no significant difference between the groups.\n")
}
