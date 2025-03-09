# Task: Simulate the distribution of p-values under H0 for a one-sided two-sample t-test

# Parameters
n <- 30  # Sample size
mu1 <- 0  # Mean for population 1 under H0
mu2 <- 0  # Mean for population 2 under H0
num_simulations <- 10000  # Number of simulations

# Storage for p-values
p_values <- numeric(num_simulations)

# Simulation
set.seed(123)  # For reproducibility
for (i in 1:num_simulations) {
  # Generate two independent samples
  X <- rnorm(n, mean = mu1, sd = 1)
  Y <- rnorm(n, mean = mu2, sd = 1)
  
  # Perform a one-sided two-sample t-test
  t_test <- t.test(Y, X, alternative = "greater", var.equal = TRUE)
  
  # Store the p-value
  p_values[i] <- t_test$p.value
}

# Plot the distribution of p-values
hist(p_values, breaks = 20, probability = TRUE, 
     main = "Distribution of P-values under H0", 
     xlab = "P-value")
abline(h = 1, col = "red")  # Uniform density line

