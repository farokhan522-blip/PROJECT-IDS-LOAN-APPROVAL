# ==============================================================================
# DATA WRANGLING PIPELINE - LOAN DATASET
# ==============================================================================
install.packages("modeest")
# 1. Data Loading
library(tidyverse)
library(modeest)

loan_data <- read_csv('loan_data.csv', show_col_types = FALSE)

# 2. Data Inspection
head(loan_data)
str(loan_data)
summary(loan_data)

# 3. Type Casting / Categorical Conversion
loan_data$person_gender <- as.factor(loan_data$person_gender)
loan_data$person_education <- as.factor(loan_data$person_education)
loan_data$person_home_ownership <- as.factor(loan_data$person_home_ownership)
loan_data$loan_intent <- as.factor(loan_data$loan_intent)
loan_data$previous_loan_defaults_on_file <- as.factor(loan_data$previous_loan_defaults_on_file)

# Converting the target label for binary classification mapping
loan_data$loan_status <- as.factor(loan_data$loan_status)

# Verification check for structural factor levels
levels(loan_data$person_education)

view(loan_data)

# 4. Missing Values Validation (Initial Check)
sum(is.na(loan_data))
colSums(is.na(loan_data))

# 5. Column Verification
names(loan_data)


# ==============================================================================
# 6. Missing Value Handling (Moved up for logical calculation data safety)
# ==============================================================================

# -----------------------------
# Numerical Variables - Median Imputation
# -----------------------------
loan_data$person_income[is.na(loan_data$person_income)] <- median(loan_data$person_income, na.rm = TRUE)
loan_data$person_emp_exp[is.na(loan_data$person_emp_exp)] <- median(loan_data$person_emp_exp, na.rm = TRUE)
loan_data$person_age[is.na(loan_data$person_age)] <- median(loan_data$person_age, na.rm = TRUE)
loan_data$loan_amnt[is.na(loan_data$loan_amnt)] <- median(loan_data$loan_amnt, na.rm = TRUE)
loan_data$loan_percent_income[is.na(loan_data$loan_percent_income)] <- median(loan_data$loan_percent_income, na.rm = TRUE)
loan_data$loan_int_rate[is.na(loan_data$loan_int_rate)] <- median(loan_data$loan_int_rate, na.rm = TRUE)
loan_data$credit_score[is.na(loan_data$credit_score)] <- median(loan_data$credit_score, na.rm = TRUE)
loan_data$cb_person_cred_hist_length[is.na(loan_data$cb_person_cred_hist_length)] <- median(loan_data$cb_person_cred_hist_length, na.rm = TRUE)

# -----------------------------
# Categorical Variables - Mode Imputation using mfv()
# -----------------------------
loan_data$person_gender[is.na(loan_data$person_gender)] <- mfv(na.omit(loan_data$person_gender))[1]
loan_data$person_education[is.na(loan_data$person_education)] <- mfv(na.omit(loan_data$person_education))[1]
loan_data$person_home_ownership[is.na(loan_data$person_home_ownership)] <- mfv(na.omit(loan_data$person_home_ownership))[1]
loan_data$loan_intent[is.na(loan_data$loan_intent)] <- mfv(na.omit(loan_data$loan_intent))[1]
loan_data$previous_loan_defaults_on_file[is.na(loan_data$previous_loan_defaults_on_file)] <- mfv(na.omit(loan_data$previous_loan_defaults_on_file))[1]

# -----------------------------
# Target Variable - Remove Missing
# -----------------------------
loan_data <- loan_data[!is.na(loan_data$loan_status), ]

# Verification after imputation
colSums(is.na(loan_data))


# ==============================================================================
# 7. Data Noise & Outlier Handling (Question iv)
# ==============================================================================
cat("Rows before outlier removal:", nrow(loan_data), "\n")

# --- A. Outlier Removal using IQR Method ---

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

# --- B. Noise Smoothing via Binning ---
# Credit score ki continuous fluctuations ko blocks me smooth karna
loan_data$credit_score_binned <- cut(loan_data$credit_score,
                                     breaks = c(300, 579, 669, 739, 850),
                                     labels = c("Poor", "Fair", "Good", "Excellent"),
                                     include.lowest = TRUE)


# ==============================================================================
# 8. Data Normalization 
# ==============================================================================

# Summary before scaling for reference
print(summary(select(loan_data, person_age, person_income, person_emp_exp, loan_amnt,
                     cb_person_cred_hist_length, loan_percent_income,
                     loan_int_rate, credit_score)))

# A. Log Transformation -> Handle Skewness
loan_data$person_age_ln                 <- log1p(loan_data$person_age)
loan_data$person_income_ln              <- log1p(loan_data$person_income)
loan_data$person_emp_exp_ln             <- log1p(loan_data$person_emp_exp)
loan_data$loan_amnt_ln                  <- log1p(loan_data$loan_amnt)
loan_data$cb_person_cred_hist_length_ln <- log1p(loan_data$cb_person_cred_hist_length)

# B. Min-Max Scaling -> Bounds data to [0,1]
loan_data$loan_percent_income_minmax <- (loan_data$loan_percent_income - min(loan_data$loan_percent_income)) /
  (max(loan_data$loan_percent_income) - min(loan_data$loan_percent_income))

# C. Z-score Standardization -> Mean=0, SD=1
loan_data$loan_int_rate_z <- (loan_data$loan_int_rate - mean(loan_data$loan_int_rate)) / sd(loan_data$loan_int_rate)
loan_data$credit_score_z  <- (loan_data$credit_score - mean(loan_data$credit_score)) / sd(loan_data$credit_score)

# Final prints to verify structural scaling outputs
print(summary(select(loan_data, person_age, person_age_ln)))
print(summary(select(loan_data, person_income, person_income_ln)))
print(summary(select(loan_data, loan_percent_income, loan_percent_income_minmax)))
print(summary(select(loan_data, loan_int_rate, loan_int_rate_z)))

# Save final clean and processed file
write_csv(loan_data, "loan_data_clean_normalized.csv")