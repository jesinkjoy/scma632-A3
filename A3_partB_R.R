# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Read the dataset
data <- read_csv("E:/JESIN/DOCUMENTS/scma/A2a/NSSO68.csv")

# Create a binary variable for non-vegetarian status
data$non_veg <- ifelse(data$eggsno_q > 0 | data$fishprawn_q > 0 | data$goatmeat_q > 0 |
                         data$beef_q > 0 | data$pork_q > 0 | data$chicken_q > 0 | data$othrbirds_q > 0, 1, 0)

# Select relevant variables for the probit model
data_clean <- data %>%
  dplyr::select(non_veg, Age, Sex, hhdsz, Religion, Education, MPCE_URP, state, State_Region) %>%
  na.omit()

# Fit the probit regression model
probit_model <- glm(non_veg ~ Age + Sex + hhdsz + Religion + Education + MPCE_URP + state + State_Region, 
                    data = data_clean, family = binomial(link = "probit"))

# Summarize the model
summary(probit_model)

# Make predictions
data_clean$predicted_prob <- predict(probit_model, type = "response")

# Visualize the results
ggplot(data_clean, aes(x = predicted_prob, fill = as.factor(non_veg))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Predicted Probability of Being Non-Vegetarian", x = "Predicted Probability", y = "Count")
