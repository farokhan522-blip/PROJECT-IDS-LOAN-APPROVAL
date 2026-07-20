# ==============================================================================
# MASTER COMPREHENSIVE PIPELINE: DATA WRANGLING, EDA, & MACHINE LEARNING
# ==============================================================================

# Required Libraries Installation & Loading
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("modeest")) install.packages("modeest")

library(tidyverse)
library(modeest)

# ==============================================================================
# PART 1: INITIAL DATA LOADING & REPLICATED SUMMARIZATION
# ==============================================================================

# Load dataset (Replicating initial base structure)
loan <- read.csv("loan_data.csv")

# MODE FUNCTION
mode_function <- function(data){
  similar <- unique(data)
  similar[which.max(tabulate(match(data, similar)))]
}

# NUMERICAL COLUMNS EXTRACATION
numeric_columns <- loan[, c("person_age",
                            "person_income",
                            "person_emp_exp",
                            "loan_amnt",
                            "loan_int_rate",
                            "loan_percent_income",
                            "cb_person_cred_hist_length",
                            "credit_score")]

# Base Summary Outputs
cat("\n--- Base Statistical Computations ---\n")
sapply(numeric_columns, mean, na.rm = TRUE)
sapply(numeric_columns, median, na.rm = TRUE)
sapply(numeric_columns, mode_function)
sapply(numeric_columns, var, na.rm = TRUE)
sapply(numeric_columns, sd, na.rm = TRUE)

# CATEGORICAL MODE SELECTIONS
cat("\n--- Base Categorical Modes ---\n")
mode_function(loan$person_gender)
mode_function(loan$person_education)
mode_function(loan$person_home_ownership)
mode_function(loan$loan_intent)
mode_function(loan$previous_loan_defaults_on_file)
mode_function(loan$loan_status)


# ==============================================================================
# PART 2: STANDALONE OUTLIER QUANTILE BLOCKS & BOXPLOTS
# ==============================================================================
par(mfrow = c(2, 4)) # Split screen window taake saare plots crash free visible hon

# 1. person_age
x <- na.omit(numeric_columns$person_age)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR_value <- IQR(x)
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value
outliers <- x[x < lower_limit | x > upper_limit]
print("Outliers in person_age:")
print(outliers)
boxplot(x, main="Box Plot of person_age", col="orange", border="darkorange")

# 2. person_income
x <- na.omit(numeric_columns$person_income)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR_value <- IQR(x)
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value
outliers <- x[x < lower_limit | x > upper_limit]
print("Outliers in person_income:")
print(outliers)
boxplot(x, main="Box Plot of person_income", col="orange", border="darkorange")

# 3. person_emp_exp
x <- na.omit(numeric_columns$person_emp_exp)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR_value <- IQR(x)
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value
outliers <- x[x < lower_limit | x > upper_limit]
print("Outliers in person_emp_exp:")
print(outliers)
boxplot(x, main="Box Plot of person_emp_exp", col="orange", border="darkorange")

# 4. loan_amnt
x <- na.omit(numeric_columns$loan_amnt)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR_value <- IQR(x)
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value
outliers <- x[x < lower_limit | x > upper_limit]
print("Outliers in loan_amnt:")
print(outliers)
boxplot(x, main="Box Plot of loan_amnt", col="orange", border="darkorange")

# 5. loan_int_rate
x <- na.omit(numeric_columns$loan_int_rate)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR_value <- IQR(x)
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value
outliers <- x[x < lower_limit | x > upper_limit]
print("Outliers in loan_int_rate:")
print(outliers)
boxplot(x, main="Box Plot of loan_int_rate", col="orange", border="darkorange")

# 6. loan_percent_income
x <- na.omit(numeric_columns$loan_percent_income)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR_value <- IQR(x)
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value
outliers <- x[x < lower_limit | x > upper_limit]
print("Outliers in loan_percent_income:")
print(outliers)
boxplot(x, main="Box Plot of loan_percent_income", col="orange", border="darkorange")

# 7. cb_person_cred_hist_length
x <- na.omit(numeric_columns$cb_person_cred_hist_length)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR_value <- IQR(x)
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value
outliers <- x[x < lower_limit | x > upper_limit]
print("Outliers in cb_person_cred_hist_length:")
print(outliers)
boxplot(x, main="Box Plot of cb_person_cred_hist_length", col="orange", border="darkorange")

# 8. credit_score
x <- na.omit(numeric_columns$credit_score)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR_value <- IQR(x)
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value
outliers <- x[x < lower_limit | x > upper_limit]
print("Outliers in credit_score:")
print(outliers)
boxplot(x, main="Box Plot of credit_score", col="orange", border="darkorange")


# ==============================================================================
# PART 3: DATA SYMMETRY ANALYSIS LOOP
# ==============================================================================
dev.new() # Opens a separate clean plot sheet window for histograms
par(mfrow = c(2, 4)) 

numerical_cols <- c("person_age", 
                    "person_income", 
                    "person_emp_exp", 
                    "loan_amnt", 
                    "loan_int_rate", 
                    "loan_percent_income", 
                    "cb_person_cred_hist_length", 
                    "credit_score")

for (col in numerical_cols) {
  cat("\n-------------------------------------------------------\n")
  cat("Symmetry Analysis for:", col, "\n")
  cat("-------------------------------------------------------\n")
  
  col_data <- loan[[col]]
  mean_val   <- mean(col_data, na.rm = TRUE)
  median_val <- median(col_data, na.rm = TRUE)
  
  cat("Mean:   ", mean_val, "\n")
  cat("Median: ", median_val, "\n")
  
  if (abs(mean_val - median_val) < 0.1) {
    cat("Result: Symmetrical Distribution\n")
  } else if (mean_val > median_val) {
    cat("Result: Positively Skewed (Right-Skewed)\n")
  } else {
    cat("Result: Negatively Skewed (Left-Skewed)\n")
  }
  
  hist(
    col_data,
    main = paste("Hist & Density of", col),
    xlab = col,
    col = "lightgreen",
    probability = TRUE,
    las = 1
  )
  
  lines(
    density(col_data, na.rm = TRUE),
    col = "red",
    lwd = 2
  )
}
par(mfrow = c(1, 1)) # Reset mapping layouts back to standard single layout


# ==============================================================================
# PART 4: ADVANCED DATA WRANGLING PIPELINE (loan_data)
# ==============================================================================
loan_data <- read_csv('loan_data.csv', show_col_types = FALSE)

# Initial Inspections
head(loan_data)
str(loan_data)
summary(loan_data)

# Type Casting / Categorical Conversion
loan_data$person_gender <- as.factor(loan_data$person_gender)
loan_data$person_education <- as.factor(loan_data$person_education)
loan_data$person_home_ownership <- as.factor(loan_data$person_home_ownership)
loan_data$loan_intent <- as.factor(loan_data$loan_intent)
loan_data$previous_loan_defaults_on_file <- as.factor(loan_data$previous_loan_defaults_on_file)
loan_data$loan_status <- as.factor(loan_data$loan_status)

# Verification checks
levels(loan_data$person_education)
sum(is.na(loan_data))
colSums(is.na(loan_data))
names(loan_data)

# -----------------------------
# Missing Value Handling (Imputations)
# -----------------------------
loan_data$person_income[is.na(loan_data$person_income)] <- median(loan_data$person_income, na.rm = TRUE)
loan_data$person_emp_exp[is.na(loan_data$person_emp_exp)] <- median(loan_data$person_emp_exp, na.rm = TRUE)
loan_data$person_age[is.na(loan_data$person_age)] <- median(loan_data$person_age, na.rm = TRUE)
loan_data$loan_amnt[is.na(loan_data$loan_amnt)] <- median(loan_data$loan_amnt, na.rm = TRUE)
loan_data$loan_percent_income[is.na(loan_data$loan_percent_income)] <- median(loan_data$loan_percent_income, na.rm = TRUE)
loan_data$loan_int_rate[is.na(loan_data$loan_int_rate)] <- median(loan_data$loan_int_rate, na.rm = TRUE)
loan_data$credit_score[is.na(loan_data$credit_score)] <- median(loan_data$credit_score, na.rm = TRUE)
loan_data$cb_person_cred_hist_length[is.na(loan_data$cb_person_cred_hist_length)] <- median(loan_data$cb_person_cred_hist_length, na.rm = TRUE)

loan_data$person_gender[is.na(loan_data$person_gender)] <- mfv(na.omit(loan_data$person_gender))[1]
loan_data$person_education[is.na(loan_data$person_education)] <- mfv(na.omit(loan_data$person_education))[1]
loan_data$person_home_ownership[is.na(loan_data$person_home_ownership)] <- mfv(na.omit(loan_data$person_home_ownership))[1]
loan_data$loan_intent[is.na(loan_data$loan_intent)] <- mfv(na.omit(loan_data$loan_intent))[1]
loan_data$previous_loan_defaults_on_file[is.na(loan_data$previous_loan_defaults_on_file)] <- mfv(na.omit(loan_data$previous_loan_defaults_on_file))[1]

loan_data <- loan_data[!is.na(loan_data$loan_status), ]
colSums(is.na(loan_data)) # Post verification

# -----------------------------
# Data Noise & Outlier Filtering
# -----------------------------
cat("Rows before outlier removal:", nrow(loan_data), "\n")

# 1. Age Outliers
Q1_age <- quantile(loan_data$person_age, 0.25, na.rm = TRUE)
Q3_age <- quantile(loan_data$person_age, 0.75, na.rm = TRUE)
IQR_age <- Q3_age - Q1_age
loan_data <- loan_data %>% 
  filter(person_age >= (Q1_age - 1.5 * IQR_age) & person_age <= (Q3_age + 1.5 * IQR_age))

# 2. Income Outliers
Q1_inc <- quantile(loan_data$person_income, 0.25, na.rm = TRUE)
Q3_inc <- quantile(loan_data$person_income, 0.75, na.rm = TRUE)
IQR_inc <- Q3_inc - Q1_inc
loan_data <- loan_data %>% 
  filter(person_income >= (Q1_inc - 1.5 * IQR_inc) & person_income <= (Q3_inc + 1.5 * IQR_inc))

# 3. Employment Experience Outliers
Q1_exp <- quantile(loan_data$person_emp_exp, 0.25, na.rm = TRUE)
Q3_exp <- quantile(loan_data$person_emp_exp, 0.75, na.rm = TRUE)
IQR_exp <- Q3_exp - Q1_exp
loan_data <- loan_data %>% 
  filter(person_emp_exp >= (Q1_exp - 1.5 * IQR_exp) & person_emp_exp <= (Q3_exp + 1.5 * IQR_exp))

# 4. Loan Amount Outliers
Q1_amt <- quantile(loan_data$loan_amnt, 0.25, na.rm = TRUE)
Q3_amt <- quantile(loan_data$loan_amnt, 0.75, na.rm = TRUE)
IQR_amt <- Q3_amt - Q1_amt
loan_data <- loan_data %>% 
  filter(loan_amnt >= (Q1_amt - 1.5 * IQR_amt) & loan_amnt <= (Q3_amt + 1.5 * IQR_amt))

cat("Rows after outlier removal:", nrow(loan_data), "\n")

# Noise Smoothing via Binning
loan_data$credit_score_binned <- cut(loan_data$credit_score,
                                     breaks = c(300, 579, 669, 739, 850),
                                     labels = c("Poor", "Fair", "Good", "Excellent"),
                                     include.lowest = TRUE)


# ==============================================================================
# PART 5: DATA NORMALIZATION & STRUCTURE TRANSFORMATION
# ==============================================================================
print(summary(select(loan_data, person_age, person_income, person_emp_exp, loan_amnt,
                     cb_person_cred_hist_length, loan_percent_income,
                     loan_int_rate, credit_score)))

# A. Log Transformations
loan_data$person_age_ln                 <- log1p(loan_data$person_age)
loan_data$person_income_ln              <- log1p(loan_data$person_income)
loan_data$person_emp_exp_ln             <- log1p(loan_data$person_emp_exp)
loan_data$loan_amnt_ln                  <- log1p(loan_data$loan_amnt)
loan_data$cb_person_cred_hist_length_ln <- log1p(loan_data$cb_person_cred_hist_length)

# B. Min-Max Scaling
loan_data$loan_percent_income_minmax <- (loan_data$loan_percent_income - min(loan_data$loan_percent_income)) /
  (max(loan_data$loan_percent_income) - min(loan_data$loan_percent_income))

# C. Z-score Standardization
loan_data$loan_int_rate_z <- (loan_data$loan_int_rate - mean(loan_data$loan_int_rate)) / sd(loan_data$loan_int_rate)
loan_data$credit_score_z  <- (loan_data$credit_score - mean(loan_data$credit_score)) / sd(loan_data$credit_score)

# Verification Prints
print(summary(select(loan_data, person_age, person_age_ln)))
print(summary(select(loan_data, person_income, person_income_ln)))
print(summary(select(loan_data, loan_percent_income, loan_percent_income_minmax)))
print(summary(select(loan_data, loan_int_rate, loan_int_rate_z)))

# Save Consolidated Clean File
write_csv(loan_data, "loan_data_clean_normalized.csv")


# ==============================================================================
# PART 6: LOGISTIC REGRESSION PREDICTIVE MODEL
# ==============================================================================
set.seed(34) 
sample_size <- floor(0.80 * nrow(loan_data))
train_indices <- sample(seq_len(nrow(loan_data)), size = sample_size)

train_data <- loan_data[train_indices, ]
test_data  <- loan_data[-train_indices, ]

# Fit Logistic Regression using Transformed Matrix Preds
model <- glm(loan_status ~  person_home_ownership + 
               loan_intent + previous_loan_defaults_on_file + person_age_ln + 
               person_income_ln + person_emp_exp_ln + loan_amnt_ln + 
               cb_person_cred_hist_length_ln + loan_percent_income_minmax + 
               loan_int_rate_z + credit_score_z, 
             data = train_data, family = binomial)

cat("\n=== LOGISTIC REGRESSION MODEL SUMMARY ===\n")
print(summary(model))

# Response Probabilities 
predict_probs <- predict(model, newdata = test_data, type = "response")
predict_classes <- ifelse(predict_probs > 0.5, 1, 0)
predict_classes <- as.factor(predict_classes)

actual_classes <- test_data$loan_status

# ==============================================================================
# CONFUSION MATRIX & ALL PERFORMANCE METRICS (ACCURACY, PRECISION, RECALL, F1)
# ==============================================================================

# 1. Confusion Matrix banana
conf_matrix <- table(Predicted = predict_classes, Actual = actual_classes)
cat("\n--- Confusion Matrix ---\n")
print(conf_matrix)

# Matrix se True Positives, False Positives, False Negatives, aur True Negatives nikalna
# Note: Yeh assume kar raha hai ke '1' (Loan Approved/Default) aapka positive class hai
TP <- conf_matrix["1", "1"]
FP <- conf_matrix["1", "0"]
FN <- conf_matrix["0", "1"]
TN <- conf_matrix["0", "0"]

# 2. ACCURACY
# Formula: (TP + TN) / Total
accuracy <- (TP + TN) / sum(conf_matrix)

# 3. PRECISION
# Formula: TP / (TP + FP) -> Kitne predicted positives asal mein positive the
precision <- TP / (TP + FP)

# 4. RECALL (Sensitivity)
# Formula: TP / (TP + FN) -> Asal positives mein se model ne kitne capture kiye
recall <- TP / (TP + FN)

# 5. F1-SCORE
# Formula: 2 * (Precision * Recall) / (Precision + Recall)
f1_score <- 2 * (precision * recall) / (precision + recall)

# --- RESULTS PRINT KARNA ---
cat("\n=============================================\n")
cat("      MODEL EVALUATION METRICS REPORT        \n")
cat("=============================================\n")
cat("1. Accuracy: ", round(accuracy * 100, 2), "%\n", sep="")
cat("2. Precision:", round(precision * 100, 2), "%\n", sep="")
cat("3. Recall:   ", round(recall * 100, 2), "%\n", sep="")
cat("4. F1-Score: ", round(f1_score * 100, 2), "%\n", sep="")
cat("=============================================\n")