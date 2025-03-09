n = 10000  # Number of coin tosses
U = runif(n, min = 0, max = 1)  # Generate n random numbers between 0 and 1
toss = U < 0.6  # A "head" occurs if the random number is less than 0.6
a = numeric(n + 1)  # Initialize cumulative count array
avg = numeric(n)  # Initialize proportion array

for (i in 2:(n + 1)) {
  a[i] = a[i - 1] + toss[i - 1]  # Update cumulative count of heads
  avg[i - 1] = a[i] / (i - 1)  # Calculate running average proportion of heads
}

plot(1:n, avg[1:n], type = "l", lwd = 5, col = "lightblue",
     ylab = "Proportions of heads", xlab = "Coin toss number",
     cex.lab = 1.5, cex.axis = 1.75)

