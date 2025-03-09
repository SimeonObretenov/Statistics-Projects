# A)

# Known parameters
alpha <- 0.05              # Significance level
z_critical <- qnorm(1 - alpha / 2) # Critical z-value for two-sided test

# Rejection region
lower_bound <- -z_critical
upper_bound <- z_critical

lower_bound
upper_bound

# B)
# Given values
sample_mean <- 5          # Sample mean
mu_0 <- 3                 # Null hypothesis mean
sigma <- 2                # Standard deviation
n <- 100                  # Sample size

# Test statistic
z_stat <- (sample_mean - mu_0) / (sigma / sqrt(n))

# p-value (two-tailed)
p_value <- 2 * (1 - pnorm(abs(z_stat)))

z_stat
p_value

# C)
# Decision
if (p_value < alpha) {
  decision <- "Reject H0"
} else {
  decision <- "Fail to reject H0"
}

decision

# D)
# Alternative mean
mu_A <- 4

# Non-central z-statistic bounds under H_A
z_lower <- (lower_bound * (sigma / sqrt(n)) + mu_0 - mu_A) / (sigma / sqrt(n))
z_upper <- (upper_bound * (sigma / sqrt(n)) + mu_0 - mu_A) / (sigma / sqrt(n))

# Power calculation (1 - beta)
power <- pnorm(z_lower) + (1 - pnorm(z_upper))

z_lower
z_upper
power

