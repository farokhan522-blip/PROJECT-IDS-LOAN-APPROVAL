# Load dataset
loan <- read.csv("loan_data.csv")

# -----------------------------
# MODE FUNCTION
# -----------------------------
mode_function <- function(data){
  similar <- unique(data)
  similar[which.max(tabulate(match(data, similar)))]
}

# -----------------------------
# NUMERICAL COLUMNS
# -----------------------------
numeric_columns <- loan[, c("person_age",
                            "person_income",
                            "person_emp_exp",
                            "loan_amnt",
                            "loan_int_rate",
                            "loan_percent_income",
                            "cb_person_cred_hist_length",
                            "credit_score")]

# -----------------------------
# MEAN
# -----------------------------
sapply(numeric_columns, mean, na.rm = TRUE)

# -----------------------------
# MEDIAN
# -----------------------------
sapply(numeric_columns, median, na.rm = TRUE)

# -----------------------------
# MODE
# -----------------------------
sapply(numeric_columns, mode_function)

# -----------------------------
# VARIANCE
# -----------------------------
sapply(numeric_columns, var, na.rm = TRUE)

# -----------------------------
# STANDARD DEVIATION
# -----------------------------
sapply(numeric_columns, sd, na.rm = TRUE)

# -----------------------------
# CATEGORICAL MODE
# -----------------------------
mode_function(loan$person_gender)
mode_function(loan$person_education)
mode_function(loan$person_home_ownership)
mode_function(loan$loan_intent)
mode_function(loan$previous_loan_defaults_on_file)
mode_function(loan$loan_status)

# =========================================================
# OUTLIERS + BOXPLOTS (SEPARATE)
# =========================================================

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

#Data Symmetry
# ====================================================
# Part 4: Data Symmetry Analysis & Plots
# ====================================================

# Numerical column names
numerical_cols <- c("person_age", 
                    "person_income", 
                    "person_emp_exp", 
                    "loan_amnt", 
                    "loan_int_rate", 
                    "loan_percent_income", 
                    "cb_person_cred_hist_length", 
                    "credit_score")

# Symmetry Analysis Loop
for (col in numerical_cols) {
  
  cat("\n-------------------------------------------------------\n")
  cat("Symmetry Analysis for:", col, "\n")
  cat("-------------------------------------------------------\n")
  
  # Current column data
  col_data <- loan[[col]]
  
  # Mean and Median
  mean_val   <- mean(col_data, na.rm = TRUE)
  median_val <- median(col_data, na.rm = TRUE)
  
  # Print Mean and Median
  cat("Mean:   ", mean_val, "\n")
  cat("Median: ", median_val, "\n")
  
  # Checking Symmetry
  if (abs(mean_val - median_val) < 0.1) {
    
    cat("Result: Symmetrical Distribution\n")
    
  } else if (mean_val > median_val) {
    
    cat("Result: Positively Skewed (Right-Skewed)\n")
    
  } else {
    
    cat("Result: Negatively Skewed (Left-Skewed)\n")
  }
  
  # Histogram
  hist(
    col_data,
    main = paste("Histogram & Density of", col),
    xlab = col,
    col = "lightgreen",
    probability = TRUE,
    las = 1
  )
  
  # Density Curve
  lines(
    density(col_data, na.rm = TRUE),
    col = "red",
    lwd = 2
  )
  
  # Pause before next graph
  if (col != numerical_cols[length(numerical_cols)]) {
    readline(prompt = "Press [Enter] for next plot...")
  }
}
