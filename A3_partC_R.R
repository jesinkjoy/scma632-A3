# Load necessary libraries
library(AER)  # for tobit function
library(readr)  # for reading CSV file

# Load the dataset
data <- read_csv("E:/JESIN/DOCUMENTS/scma/A2a/NSSO68.csv")

# Inspect the dataset
head(data)

# Selecting relevant columns for analysis
selected_cols <- c("MPCE_URP", "Age", "Sex", "Education", "Religion", "hhdsz")
data_selected <- data[selected_cols]

# Handling missing values if any
data_selected <- na.omit(data_selected)

# Perform Tobit regression
tobit_model <- tobit(MPCE_URP ~ Age + Sex + Education + Religion + hhdsz, data = data_selected)

# Summary of the model
summary(tobit_model)
