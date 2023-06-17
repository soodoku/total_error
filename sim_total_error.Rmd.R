# Load the MASS package for mvrnorm function
library(MASS)

# Set seed for reproducibility
set.seed(1234)

# Number of respondents
num_respondents <- 1000

# Covariance matrix for variable1 and correlated1
cov_matrix <- matrix(c(1, 0.9, 0.9, 1), nrow = 2)

# Create an empty data frame to store the simulated data
data <- data.frame()

# Generate data for each respondent
for (i in 1:num_respondents) {
  # Random number of rows for the current respondent
  num_rows <- sample(5:1000, 1)
  
  # Generate data for the current respondent
  respondent_data <- data.frame(
    respondent_id = i,
    # Generate variable1 and correlated1 from multivariate normal distribution
    as.data.frame(mvrnorm(num_rows, mu = c(0, 0), Sigma = cov_matrix))
    # Add more variables as needed
  )
  respondent_data$V2 <- respondent_data$V2 + .1 * (sd(respondent_data$V1))
  # Generate variable2 and correlated2
  respondent_data$variable2 <- rnorm(num_rows)
  respondent_data$correlated2 <- 0.8 * respondent_data$variable2 + rnorm(num_rows, mean = 0, sd = 0.2) + .1 * (sd(respondent_data$variable2))
  
  # Append the respondent's data to the main data frame
  data <- rbind(data, respondent_data)
}

# Print the simulated data
print(data)

library(dplyr)

# Group by respondent_id and calculate means and sums
summary_data <- data %>%
  group_by(respondent_id) %>%
  summarize(
    mean_variable1 = mean(V1),
    sum_variable1 = sum(V1),
    nrows = n(),
    mean_correlated1 = mean(V2),
    sum_correlated1 = sum(V2),
    mean_variable2 = mean(variable2),
    sum_variable2 = sum(variable2),
    mean_correlated2 = mean(correlated2),
    sum_correlated2 = sum(correlated2)
  )

with(data, cor(V1, V2))
with(summary_data, cor(mean_variable1, mean_correlated1))
with(summary_data, cor(sum_variable1, sum_correlated1))

with(data, cor(variable2, correlated2))
with(summary_data, cor(mean_variable2, mean_correlated2))
with(summary_data, cor(sum_variable2, sum_correlated2))

