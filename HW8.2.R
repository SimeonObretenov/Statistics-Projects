# Parameters
mu_0 <- 10      # Industry standard mean
sigma <- 2      # Standard deviation
n <- 16         # Sample size
x_bar <- 11     # Sample mean
alpha <- 0.10   # Significance level

# Part (a): Two-sided test
z <- (x_bar - mu_0) / (sigma / sqrt(n))  # Test statistic
p_value <- 2 * (1 - pnorm(abs(z)))       # Two-sided p-value
z_critical <- qnorm(1 - alpha / 2)       # Critical value for two-sided test

# Print results for part (a)
cat("Part (a):\n")
cat("Test statistic (z):", z, "\n")
cat("Critical z-value (two-sided):", z_critical, "\n")
cat("P-value:", p_value, "\n")
cat("Conclusion:", ifelse(abs(z) > z_critical, "Reject H0", "Fail to reject H0"), "\n\n")

# Part (b): Right-sided test
z_critical_right <- qnorm(1 - alpha)  # Critical value for right-sided test

# Power calculation for mu = 11 and mu = 12
mu_1 <- 11
mu_2 <- 12
z_power_11 <- (mu_1 - mu_0) / (sigma / sqrt(n))
z_power_12 <- (mu_2 - mu_0) / (sigma / sqrt(n))
power_11 <- 1 - pnorm(z_critical_right - z_power_11)
power_12 <- 1 - pnorm(z_critical_right - z_power_12)

# Print results for part (b)
cat("Part (b):\n")
cat("Critical z-value (right-sided):", z_critical_right, "\n")
cat("Power for mu = 11:", power_11, "\n")
cat("Power for mu = 12:", power_12, "\n")

