# Load necessary package
if (!require('pwr')) install.packages('pwr', dependencies=TRUE)
library(pwr)

# Define parameters
mean1 <- 94.0        # Mean fasting blood glucose of people without diabetes
mean2 <- 96.5        # Mean fasting blood glucose for those with coffee intake
sd <- 10.0           # Standard deviation
effect_size <- (mean2 - mean1) / sd  # Calculate Cohen's d
power <- 0.85        # Desired power
sig_level <- 0.05    # Significance level for two-sided test

# Perform sample size calculation
result <- pwr.t.test(d = effect_size, power = power, sig.level = sig_level, type = "two.sample", alternative = "two.sided")
result$n

