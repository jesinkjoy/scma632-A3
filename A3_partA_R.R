# Load necessary libraries
library(dplyr)
library(caret)
library(tm)
library(e1071)
library(ggplot2)
library(pROC)
library(rpart)
library(rpart.plot)

# Load the dataset
data <- read.csv('E:/JESIN/DOCUMENTS/scma/A3/A/spam.csv', encoding = 'ISO-8859-1')

# Data preprocessing
data <- na.omit(data)  # Drop rows with missing values
data$Category <- ifelse(data$Category == 'ham', 0, 1)  # Encode target variable

# Take a sample of the data to reduce processing time
set.seed(42)
data_sample <- data %>% sample_n(1000)  # Adjust the sample size as needed

# Feature extraction
corpus <- Corpus(VectorSource(data_sample$Message))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

dtm <- DocumentTermMatrix(corpus)
X <- as.data.frame(as.matrix(dtm))
y <- data_sample$Category

# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex,]
X_test <- X[-trainIndex,]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Logistic Regression
log_reg <- glm(y_train ~ ., data = X_train, family = binomial)
y_pred_log_reg <- predict(log_reg, newdata = X_test, type = "response")
y_pred_log_reg_class <- ifelse(y_pred_log_reg > 0.5, 1, 0)

# Confusion Matrix for Logistic Regression
conf_matrix_log_reg <- table(y_test, y_pred_log_reg_class)
print("Confusion Matrix for Logistic Regression:")
print(conf_matrix_log_reg)

# ROC Curve for Logistic Regression
roc_log_reg <- roc(y_test, y_pred_log_reg)
auc_log_reg <- auc(roc_log_reg)

ggplot() + 
  geom_line(aes(x = roc_log_reg$specificities, y = roc_log_reg$sensitivities, color = 'blue')) +
  geom_abline(linetype = 'dashed', color = 'red') +
  labs(x = 'False Positive Rate', y = 'True Positive Rate', title = 'ROC Curve') +
  scale_color_manual(name = 'Model', values = 'blue', labels = paste('Logistic Regression (AUC =', round(auc_log_reg, 2), ')'))



# Print column names
print(colnames(X_train))

# Ensure column names are unique and valid
colnames(X_train) <- make.names(colnames(X_train), unique = TRUE)
colnames(X_test) <- make.names(colnames(X_test), unique = TRUE)

# Print column names again to confirm
print(colnames(X_train))

# Ensure y_train is a factor
y_train <- as.factor(y_train)

# Decision Tree Classifier
dtc <- rpart(y_train ~ ., data = X_train, method = "class")
y_pred_dtc <- predict(dtc, newdata = X_test, type = "class")

# Confusion Matrix for Decision Tree
conf_matrix_dtc <- table(y_test, y_pred_dtc)
print("Confusion Matrix for Decision Tree:")
print(conf_matrix_dtc)

# ROC Curve for Decision Tree
y_prob_dtc <- predict(dtc, newdata = X_test, type = "prob")[,2]
roc_dtc <- roc(y_test, y_prob_dtc)
auc_dtc <- auc(roc_dtc)

ggplot() + 
  geom_line(aes(x = roc_dtc$specificities, y = roc_dtc$sensitivities, color = 'blue')) +
  geom_abline(linetype = 'dashed', color = 'red') +
  labs(x = 'False Positive Rate', y = 'True Positive Rate', title = 'ROC Curve') +
  scale_color_manual(name = 'Model', values = 'blue', labels = paste('Decision Tree (AUC =', round(auc_dtc, 2), ')'))


# Compare the models
cat("Classification Report for Logistic Regression:\n")
print(confusionMatrix(factor(y_pred_log_reg_class), factor(y_test)))

cat("Classification Report for Decision Tree:\n")
print(confusionMatrix(factor(y_pred_dtc), factor(y_test)))