# Task (a): Perform a hypothesis test to determine if one virus tends to produce more lesions than the other.

# Step 1: Define the hypotheses
# H0: The mean difference in the number of lesions caused by Virus 1 and Virus 2 is zero (mu_d = 0).
# H1: The mean difference in the number of lesions caused by Virus 1 and Virus 2 is not zero (mu_d ≠ 0).

# Step 2: Set the significance level
# We use alpha = 0.10 (10% level of significance).

# Step 3: Perform a paired t-test
# Since both viruses are applied to the same leaves, the data is paired, and we test for the mean difference.

# Task (b)
# Data
virus1 <- c(31, 20, 18, 17, 9, 8, 10, 7)
virus2 <- c(18, 17, 14, 11, 10, 7, 5, 6)

# Calculate the differences
differences <- virus1 - virus2

# Paired t-test
t_test_result <- t.test(virus1, virus2, paired = TRUE, alternative = "two.sided")

# Print t-test result
print(t_test_result)

# Extract confidence interval
confidence_interval <- t_test_result$conf.int
print(confidence_interval)
