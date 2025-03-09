# Given data
x <- 26   # Number of winning tickets
n <- 65   # Total tickets observed
p0 <- 0.5 # Hypothesized proportion

# Perform a two-sided proportion test
result <- prop.test(x, n, p = p0, alternative = "two.sided", conf.level = 0.95)

# Display the result
print(result)

