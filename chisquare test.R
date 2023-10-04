# Step 1: Create a contingency table
data <- data.frame(
  Category = c("A", "B", "A", "B", "A", "B", "A", "B"),
  Outcome = c("Yes", "Yes", "No", "Yes", "Yes", "No", "No", "No")
)

cont_table <- table(data$Category, data$Outcome)

# Step 2: Perform the chi-square test
chi_sq_result <- chisq.test(cont_table)

# Step 3: Interpret the result
print(chi_sq_result)

# Step 4: Draw a conclusion
if (chi_sq_result$p.value < 0.05) {
  cat("Conclusion: There is a significant association between Category and Outcome.\n")
} else {
  cat("Conclusion: There is no significant association between Category and Outcome.\n")
}

# Create a bar plot of the contingency table
barplot(cont_table, beside = TRUE, legend.text = TRUE, col = c("lightblue", "lightgreen"))
