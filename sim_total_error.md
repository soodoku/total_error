Total Error: Using ML to Measure Total Exposure
================
Gaurav
06/17/2023

To investigate the impact of small bias in ML measures on estimates of
total number of visits, etc., we simulated some data. The code simulates
a browsing data as follows. We start by randomly choosing the number of
domains visited by a respondent. We randomly sample a number between 5
and a 1000. Second, we simulate a multivariate normal with covariance of
.9 to create two columns and take one of the columns to reflect the true
measure. To the yhat column, we add a small bias— .1 of the standard
deviation of the true measure.

``` r
library(MASS)
set.seed(1234)

num_respondents <- 1000

# Covariance matrix for var. and measure
cov_matrix <- matrix(c(1, 0.9, 0.9, 1), nrow = 2)

# Create an empty data frame to store the simulated data
fin_data <- data.frame()

# Generate data for each respondent
for (i in 1:num_respondents) {
  # Random number of rows for the current respondent
  num_rows <- sample(5:1000, 1)
  
  # Generate data for the current respondent
  data <- data.frame(
    respondent_id = i,
    # Generate variable1 and correlated1 from multivariate normal distribution
    as.data.frame(mvrnorm(num_rows, mu = c(0, 0), Sigma = cov_matrix))
  )
  names(data) <- c("respondent_id", "y", "yhat")
  data$yhat <- data$yhat + .1 * (sd(data$y))
  
  fin_data <- rbind(fin_data, data)
}

summary(fin_data)
```

    ##  respondent_id          y                  yhat        
    ##  Min.   :   1.0   Min.   :-4.532658   Min.   :-4.9175  
    ##  1st Qu.: 256.0   1st Qu.:-0.676484   1st Qu.:-0.5767  
    ##  Median : 507.0   Median :-0.001690   Median : 0.0986  
    ##  Mean   : 504.3   Mean   :-0.000216   Mean   : 0.1001  
    ##  3rd Qu.: 756.0   3rd Qu.: 0.674746   3rd Qu.: 0.7748  
    ##  Max.   :1000.0   Max.   : 5.164210   Max.   : 5.0277

Next, we create a summary data frame that creates measures at the
respondent level. We create measures that capture the mean and the
total.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:MASS':
    ## 
    ##     select

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Group by respondent_id and calculate means and sums
summary_data <- fin_data %>%
  group_by(respondent_id) %>%
  summarize(
    mean_y = mean(y),
    sum_y = sum(y),
    nrows = n(),
    mean_yhat = mean(yhat),
    sum_yhat = sum(yhat),
  )
```

Next, we check the consequences of bias on correlations.

As you can see, the original correlation (in the long form) is .9. As
expected, the correlation for the aggregated data is also unaffected.
But the correlation between the sums (which tally some version of total
visits) is dramatically lower.

``` r
with(fin_data, cor(y, yhat))
```

    ## [1] 0.9000409

``` r
with(summary_data, cor(mean_y, mean_yhat))
```

    ## [1] 0.9006167

``` r
with(summary_data, cor(sum_y, sum_yhat))
```

    ## [1] 0.5383029

Part of the reason is that net bias is negatively correlated with the
number of domains visited by the respondent.

``` r
with(summary_data, cor(sum_y - sum_yhat, nrows))
```

    ## [1] -0.9397464

The consequences for reporting raw numbers are yet clearer.

``` r
summary(summary_data$sum_y)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -89.9745 -12.8248   0.5104  -0.1106  13.5700  83.5562

``` r
summary(summary_data$sum_yhat)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -28.64   20.02   48.34   51.13   76.09  184.43

``` r
summary(fin_data$y - fin_data$yhat)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.2504 -0.4024 -0.1003 -0.1003  0.2027  1.8752

``` r
summary(summary_data$sum_y - summary_data$sum_yhat)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -126.922  -74.645  -52.051  -51.239  -25.764    2.216

The solution for raw numbers is also clear:

``` r
summary(summary_data$sum_yhat - summary_data$nrows*(.1)) # can multiply with the sd(y)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -86.43527 -11.73254   0.00754   0.03092  13.08083  88.82867
