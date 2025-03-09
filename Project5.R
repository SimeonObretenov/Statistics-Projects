# Task 1
# a) 
# pbinom - computes the cumulative probability for the Binomial distribution
# Exact Binomial Probability
n <- 40
p <- 0.2
P_X_leq_6 <- pbinom(6, size = n, prob = p)
P_X_leq_6

# b) 
# ppois - computes the cumulative probability for the Poisson distribution
# Poisson Approximation
lambda <- 8
P_Y_leq_6 <- ppois(6, lambda = lambda)
P_Y_leq_6

# Task 2
# a)
# pnorm - calculates the cumulative probability for a given value under the normal distribution
# Probability between 84 and 116
mu <- 100
sigma <- 16

P_84_116 <- pnorm(116, mean = mu, sd = sigma) - pnorm(84, mean = mu, sd = sigma)
P_84_116


# b)
# qnorm - finds the value (quantile) corresponding to a given cumulative probability
# Middle 90% cutoff values
lower_cutoff <- qnorm(0.05, mean = mu, sd = sigma)  # 5th percentile
upper_cutoff <- qnorm(0.95, mean = mu, sd = sigma)  # 95th percentile

lower_cutoff
upper_cutoff

# Task 3 
# a)
# pbinom - computes the cumulative probability for the Binomial distribution
# Binomial probability
n <- 600
p <- 0.75

P_430_450 <- pbinom(450, size = n, prob = p) - pbinom(429, size = n, prob = p)
P_430_450

# b) 
# pnorm - calculates the cumulative probability for a given value under the normal distribution
# Normal approximation without continuity correction
mu <- n * p
sigma <- sqrt(n * p * (1 - p))

P_norm_no_correction <- pnorm(450, mean = mu, sd = sigma) - pnorm(430, mean = mu, sd = sigma)
P_norm_no_correction

# Normal approximation with continuity correction
P_norm_with_correction <- pnorm(450.5, mean = mu, sd = sigma) - pnorm(429.5, mean = mu, sd = sigma)
P_norm_with_correction

# Task 4
# a) 
# Poisson probability
lambda <- 50

P_54_62 <- ppois(62, lambda = lambda) - ppois(53, lambda = lambda)
P_54_62

# b) 
# Normal approximation without continuity correction
mu <- 50
sigma <- sqrt(50)

P_norm_no_correction <- pnorm(62, mean = mu, sd = sigma) - pnorm(54, mean = mu, sd = sigma)

# Normal approximation with continuity correction
P_norm_with_correction <- pnorm(62.5, mean = mu, sd = sigma) - pnorm(53.5, mean = mu, sd = sigma)

P_norm_no_correction
P_norm_with_correction
