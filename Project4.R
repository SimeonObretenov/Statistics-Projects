#Task 2
# a)
# Define the parameters
mu <- 185  # Mean
sigma <- 20  # Standard deviation

# Generate a sequence of cholesterol levels
x <- seq(mu - 4*sigma, mu + 4*sigma, length = 100)

# Compute the PDF using the dnorm() function
pdf <- dnorm(x, mean = mu, sd = sigma)

# Plot the PDF
plot(x, pdf, type = "l", main = "Probability Density Function of Cholesterol Levels",
     xlab = "Cholesterol Level (mg/dL)", ylab = "Density", col = "blue", lwd = 2)
abline(v = mu, col = "red", lty = 2)  # Mark the mean
legend("topright", legend = c("PDF", "Mean"), col = c("blue", "red"), lty = c(1, 2))

# b)
# P(170 <= X <= 180)
prob_between_170_180 <- pnorm(180, mean = mu, sd = sigma) - pnorm(170, mean = mu, sd = sigma)
prob_between_170_180

# P(X > 200)
prob_greater_200 <- 1 - pnorm(200, mean = mu, sd = sigma)
prob_greater_200

#c)
# 25th percentile
Q1 <- qnorm(0.25, mean = mu, sd = sigma)

# 75th percentile
Q3 <- qnorm(0.75, mean = mu, sd = sigma)

# Interquartile Range (IQR)
IQR <- Q3 - Q1
IQR
