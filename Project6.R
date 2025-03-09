#Task 1
# a)
k <- 100 
x <- rnorm(sample(k:(2*k), 1), runif(1, 0, k), rexp(1, 1/k))
# sample - generates one integer from the range [k, 2 * k] (in this between 100 and 200)
# runif - generates one number between 0 and k(in this case 0 and 100)
# rexp - generates random number from an exponential distribution with rate parameter 1/k (in this case rate = 0.01)
# rnorm - generates a vector of random numbers (length determined by sample), from a normal distribution with
# Mean = runif(1, 0, k) and Standard deviation = rexp(1, 1/k).

# x is a vector of random numbers, the length of which is randomly chosen from 100 to 200. 
# The numbers are sampled from a normal distribution with:
# A mean randomly chosen between 0 and 100.
# A standard deviation randomly chosen from an exponential distribution with a rate parameter of 0.01.


# b)
# Set k
k <- 100

# Generate x
x <- rnorm(sample(k:(2*k), 1), runif(1, 0, k), rexp(1, 1/k))

# Calculate mean and standard deviation
mean_x <- mean(x)
sd_x <- sd(x)

# Plot the histogram
hist(x, breaks = 20, col = "gray", main = "Histogram of x", xlab = "Values of x")

# Add mean in red
abline(v = mean_x, col = "red", lwd = 2)

# Add standard deviation in blue
abline(v = mean_x - sd_x, col = "blue", lwd = 2, lty = 2)
abline(v = mean_x + sd_x, col = "blue", lwd = 2, lty = 2)

# Add a legend
legend("topright", legend = c("Mean", "1 SD"), col = c("red", "blue"), lwd = 2, lty = c(1, 2))

# Task 2
# b)
# Parameters
mu <- 5
sigma2 <- 4
n <- 50

# Densities
x_vals <- seq(0, 10, length.out = 1000)
density_X <- dnorm(x_vals, mean = mu, sd = sqrt(sigma2))          # Density of X
density_Xbar <- dnorm(x_vals, mean = mu, sd = sqrt(sigma2 / n))   # Density of Xbar

# Plot
plot(x_vals, density_X, type = "l", col = "blue", lwd = 2,
     main = "Densities of X and Xbar",
     xlab = "Value", ylab = "Density")
lines(x_vals, density_Xbar, col = "red", lwd = 2)
legend("topright", legend = c("X", "Xbar"), col = c("blue", "red"), lwd = 2)

# c) 
# Standard deviation of Xbar
sigma_Xbar <- sqrt(sigma2 / n)

# 95% and 99.9% neighborhoods
neighborhood_95 <- c(mu - 1.96 * sigma_Xbar, mu + 1.96 * sigma_Xbar)
neighborhood_999 <- c(mu - 3.29 * sigma_Xbar, mu + 3.29 * sigma_Xbar)

cat("95% Neighborhood: ", neighborhood_95, "\n")
cat("99.9% Neighborhood: ", neighborhood_999, "\n")

# Add to the density plot
abline(v = neighborhood_95, col = "green", lwd = 2, lty = 2)    # 95% bounds
abline(v = neighborhood_999, col = "purple", lwd = 2, lty = 2)  # 99.9% bounds
legend("topleft", legend = c("95% Neighborhood", "99.9% Neighborhood"),
       col = c("green", "purple"), lty = 2, lwd = 2)

# c) 
# Generate random sample
sample <- rnorm(50, mean = mu, sd = sqrt(sigma2))

# Empirical mean and standard deviation
xbar_empirical <- mean(sample)
s_empirical <- sd(sample)

# Histogram
hist(sample, breaks = 10, col = "lightblue", main = "Histogram of Sample",
     xlab = "Value", prob = TRUE)
abline(v = xbar_empirical, col = "red", lwd = 2, lty = 2)  # Empirical mean
legend("topright", legend = c("Empirical Mean"), col = "red", lty = 2, lwd = 2)

# Boxplot
boxplot(sample, main = "Boxplot of the Sample", col = "lightgreen",
        ylab = "Value", horizontal = TRUE)

# Task 4
# Iterates through different sample sizes (n), starting from small values like 1, 2, 3 up to large values like 2000.
# The CLT states that as the sample size increases, the sample mean approaches a normal distribution, regardless of the original distribution.
for (n in c(1, 2, 3, 5, 10, 25, 50, 100, 250, 500, 1000, 2000)) { 
  
# result: An empty vector to store the standardized sample means for the current sample size (n).
# mu and sigma: The mean and standard deviation of the exponential distribution (exp(1)).
  result <- c()
  mu <- 1
  sigma <- 1
  
# Repeats the experiment 5000 times for each sample size n.
  for (i in 1:5000) {
    
# Draws n samples from an exponential distribution with rate 1 / mu (mean = 1).
    X <- rexp(n, 1 / mu)
    
# - Standardizing the sample mean:
    #	mean(X): Sample mean.
    #	(mean(X) - mu) / (sigma * sqrt(n)): Standardizes the sample mean using the population mean (mu) and standard deviation (sigma).
    
    result[i] <- (mean(X) - mu) / (sigma * sqrt(n))
  }
  
# hist: Computes the histogram of the standardized sample means (but does not plot it yet).
# breaks: Defines bin widths for the histogram.
# ylim: Sets the y-axis range to include both the histogram density and the theoretical normal density (centered at 0).
  
  hist <- hist(result, breaks=seq(min(result)-1, max(result)+1, by=0.25), plot=FALSE)
  ylim <- range(hist$density, dnorm(0))
  
# Plots the histogram of the standardized sample means with:
# prob=TRUE: Scales the y-axis to represent probability densities.
# ylim: Ensures the y-axis accommodates both the histogram and the normal curve.
# main=paste("n=", n): Adds a dynamic title to show the current sample size.
  
  hist(result, breaks=seq(min(result)-1, max(result)+1, by=0.25), prob=TRUE, ylim=ylim, 
       main=paste("n=", n), col="lightblue")
  x <- seq(-4, 4, by=0.1)
  
# Generates the theoretical standard normal density curve (dnorm(x)) and overlays it on the histogram.
  lines(x, dnorm(x), lty=1, lwd=2, col="lightpink")
  
# Adds a 1.5-second delay to allow visualization of each histogram before the next iteration
  Sys.sleep(1.5)
}

# Comments on the Results
# 1.	For Small Sample Sizes (n = 1, 2, 3):
# The histogram of standardized sample means is skewed because the exponential distribution is highly skewed.
# The CLT does not yet apply because the sample size is too small.
# 2.	As Sample Size Increases (n = 10, 25, 50):
#	The histogram starts to resemble a normal distribution.
#	The central limit theorem begins to take effect.
# 3.	For Large Sample Sizes (n = 100, 250, 500, 1000, 2000):
#	The histogram aligns very closely with the theoretical standard normal curve.
#	This confirms the CLT: the distribution of the sample mean approaches a normal distribution as n increases, regardless of the underlying distribution (exponential in this case).


    
