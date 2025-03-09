# A)

# Null Hypothesis
# H(0): μ = 3.2 (The average newborn weight is unaffected by the obstetrician’s advice)

# Alternative Hypothesis
# H(A): μ < 3.2 (The obstetrician’s advice results in lower newborn weights on average.)

# B) 
# Known parameters
mu_0 <- 3.2               # Null hypothesis mean
sigma <- sqrt(0.5)        # Standard deviation
n <- 16                   # Sample size
sample_mean <- 2.8        # Observed sample mean
alpha <- 0.05             # Significance level

# Test statistic
z_stat <- (sample_mean - mu_0) / (sigma / sqrt(n))

# Critical value for one-tailed test
z_critical <- qnorm(alpha)

# Decision
if (z_stat < z_critical) {
  decision <- "Reject H0"
} else {
  decision <- "Fail to reject H0"
}

z_stat
z_critical
decision

# Test at 1% significance level
alpha_1 <- 0.01
z_critical_1 <- qnorm(alpha_1)

# Test at 10% significance level
alpha_10 <- 0.10
z_critical_10 <- qnorm(alpha_10)

# Results
list(
  "1% critical value" = z_critical_1,
  "10% critical value" = z_critical_10
)

# C) 
# Alternative mean
mu_A <- 2.5

# Non-central z-statistic
z_beta <- (z_critical * (sigma / sqrt(n)) + mu_0 - mu_A) / (sigma / sqrt(n))

# Power of the test
power <- 1 - pnorm(z_beta)

z_beta
power
