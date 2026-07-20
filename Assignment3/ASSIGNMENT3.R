# ==========================================
# ASSIGNMENT 3: LOGISTIC REGRESSION IN R
# ==========================================

# Step 1: Data Loading 
loan_data <- read.csv("loan_data.csv")

# Data structure (Optional)
# str(loan_data)

loan_data$loan_status <- as.factor(loan_data$loan_status)

# Step 3: Data ko Train aur Test Sets mein Split karna (80% Train, 20% Test)
set.seed(34) 
sample_size <- floor(0.80 * nrow(loan_data))
train_indices <- sample(seq_len(nrow(loan_data)), size = sample_size)

train_data <- loan_data[train_indices, ]
test_data  <- loan_data[-train_indices, ]

# Step 4: Logistic Regression Model Train Karna
# '.' ka matlab hai baaki saare columns predictors hain
model <- glm(loan_status ~ ., data = train_data, family = binomial)

# Model ki summary dekhne ke liye
summary(model)

# Step 5: Test Data Par Predictions Karna
# Yeh probabilities (0 se 1 ke darmiyan) return karega
predict_probs <- predict(model, newdata = test_data, type = "response")

# Agar probability 0.5 se zyada hai to 1 (Approved/Paid), warna 0
predict_classes <- ifelse(predict_probs > 0.5, 1, 0)
predict_classes <- as.factor(predict_classes)

# Step 6: Evaluation Metrics (Confusion Matrix & Accuracy)
# Actual values test data se leti hain
actual_classes <- test_data$loan_status

# Confusion Matrix banana
conf_matrix <- table(Predicted = predict_classes, Actual = actual_classes)
print("--- Confusion Matrix ---")
print(conf_matrix)

# Accuracy calculate karna
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))