loan <- read.csv("loan_data.csv")

# Checking data set columns
names(loan)

# Load dataset
loan <- read.csv("loan_data.csv")

# Mode Function
mode_function <- function(data){
  similar <- unique(data)
  similar[which.max(tabulate(match(data, similar)))]
}

# Numerical Columns
numeric_columns <- loan[, c("person_age",
                            "person_income",
                            "person_emp_exp",
                            "loan_amnt",
                            "loan_int_rate",
                            "loan_percent_income",
                            "cb_person_cred_hist_length",
                            "credit_score")]

# Mean
sapply(numeric_columns, mean)

# Median
sapply(numeric_columns, median)

# Mode
sapply(numeric_columns, mode_function)

# Variance
sapply(numeric_columns, var)

# Standard Deviation
sapply(numeric_columns, sd)

# Categorical Columns Mode
mode_function(loan$person_gender)
mode_function(loan$person_education)
mode_function(loan$person_home_ownership)
mode_function(loan$loan_intent)
mode_function(loan$previous_loan_defaults_on_file)
mode_function(loan$loan_status)


# Calculate Quartiles
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)

# Calculate IQR
IQR_value <- IQR(x)

# Lower and Upper Limits
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value

# Finding Outliers
outliers <- x[x < lower_limit | x > upper_limit]

# Print Outliers
print("Outliers are:")
print(outliers)

# Number of Outliers
print(paste("Total Outliers:", length(outliers)))

# Box Plot
boxplot(
  x,
  main = "Box Plot of Loan Amount",
  ylab = "Loan Amount",
  col = "orange",
  border = "darkorange"
)
