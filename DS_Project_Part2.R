loan <- read.csv("loan_data.csv")

# Checking data set columns
names(loan)

x <- loan$loan_amnt

# mean, median & mode
mean(x)
median(x)

mode_function <- function(data){
  similar <- unique(data)
  similar[which.max(tabulate(match(data, similar)))]
}
mode_function(x)

# variance & standard Deviation

var(x)
sd(x)
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
