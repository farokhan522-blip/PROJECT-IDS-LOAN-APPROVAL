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
