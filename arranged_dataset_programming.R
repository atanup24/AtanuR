# Load the required library
library(data.table)

# Set the file path to your large CSV file
large_csv_file <- "GLODAPv2.2022_Merged_Master_File.csv"

# Set the number of rows per chunk
chunk_size <- 690624  # Adjust this to your needs

# Read the large CSV file using data.table's fread function
large_data <- fread(large_csv_file)

# Calculate the number of chunks
num_chunks <- ceiling(nrow(large_data) / chunk_size)

# Split the data into chunks
chunks <- split(large_data, rep(1:num_chunks, each = chunk_size, length.out = nrow(large_data)))

# Save each chunk as a separate CSV file
for (i in 1:num_chunks) {
  chunk <- chunks[[i]]
  chunk_file <- paste0("chunk_", i, ".csv")
  write.csv(chunk, file = chunk_file, row.names = FALSE)
}

# Load the required library
library(dplyr)

# Filter the dataset to include data from 2003 to 2019
filtered_data <- large_data %>%
  filter(G2year >= 2003 & G2year <= 2019) %>%
  arrange(G2year)
# Save the filtered dataset to a CSV file
write.csv(filtered_data, file = "2003-2019_dataset.csv", row.names = FALSE)

# Load the required library
library(dplyr)

# Specify the columns you want to keep
selected_columns <- c(
  "G2year", "G2month", "G2lattitude", "G2longitude",
  "G2salinity", "G2nitrite", "G2oxygen", "G2nitrate",
  "G2silicate", "G2phosphate", "G2tco2", "G2talk"
)

# Load the required library
library(dplyr)

# Specify the columns you want to keep
selected_columns <- c(
  "G2year", "G2month", "G2latitude", "G2longitude",
  "G2salinity", "G2nitrite", "G2oxygen", "G2nitrate",
  "G2silicate", "G2phosphate", "G2tco2", "G2talk"
)

# Filter the dataset to include data from 2003 to 2019 and select specific columns
filtered_data <- large_data %>%
  filter(G2year >= 2003 & G2year <= 2019) %>%
  select(all_of(selected_columns))

# Save the filtered and selected data to a CSV file
write.csv(filtered_data, file = "filtered_data_2003_to_2019.csv", row.names = FALSE)

# Load the required libraries
library(dplyr)
library(readr)

# Read the CSV file
# Replace 'your_dataset.csv' with the actual file path to your dataset
dataset <- read_csv("filtered_data_2003_to_2019.csv")

# Filter data for each month and year, ignoring blank cells
monthly_averages <- dataset %>%
  filter(G2year >= 2003 & G2year <= 2019) %>%
  group_by(G2year, G2month) %>%
  summarize_if(is.numeric, ~ mean(., na.rm = TRUE))

# Save the monthly averages to a CSV file
write_csv(monthly_averages, "monthly_averages_2003_to_2019.csv")



