# Load the required library
library(dplyr)

# Load the data from the CSV file
data <- read.csv("GLODAPv2.2022_Merged_Master_File.csv")

# Assuming 'data' is your dataframe
data <- data %>%
  mutate(
    G2nitrate = ifelse(G2nitrate == -9999, "", G2nitrate),
    G2oxygen = ifelse(G2oxygen == -9999, "", G2oxygen),
    G2silicate = ifelse(G2silicate == -9999, "", G2silicate),
    G2phosphate = ifelse(G2phosphate == -9999, "", G2phosphate),
    G2fco2 = ifelse(G2fco2 == -9999, "", G2fco2),
    G2tco2 = ifelse(G2tco2 == -9999, "", G2tco2),
    G2nitrite = ifelse(G2nitrite == -9999, "", G2nitrite),
    G2doc = ifelse(G2doc == -9999, "", G2doc),
    G2don = ifelse(G2don == -9999, "", G2don),
    G2toc = ifelse(G2toc == -9999, "", G2toc)
  )

# Filter out rows with -9999 values in any of the specified columns
filtered_data <- data %>%
  filter(G2nitrate != -9999,
         G2oxygen != -9999,
         G2silicate != -9999,
         G2phosphate != -9999,
         G2fco2 != -9999)

# Select the desired columns
selected_columns <- filtered_data %>%
  select(G2latitude, G2longitude, G2year, G2month,
         G2nitrate, G2oxygen, G2silicate, G2phosphate, G2fco2)

# Save the cleaned dataset to a CSV file
write.csv(selected_columns, "master_dataset_cleaned.csv", row.names = FALSE)

# Check the first few rows of the cleaned dataset
head(selected_columns)
