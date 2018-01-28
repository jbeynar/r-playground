data <- matrix(c(3,20,3,25,2,20,4,30,1,10), ncol = 2, byrow = T)
data <- cbind(data, data[,1]*data[,2], data[,1]^2)
n <- nrow(data)
colnames(data) <- c("x", "y", "xy", "x2")
sums <- colSums(data)

b <- (n * sums["xy"] - sums["x"] * sums["y"]) / 
  (n * sums["x2"] - sums["x"]^2)

a <- mean(data[,"y"]) - b * mean(data[,"x"])

# estimate y for given x
xe = 5
ye <- a + b  * xe

plot(c(data[,"x"], xe), c(data[,"y"], ye), xlab = "Earnings in k PLN", ylab = "Cinema expenses",
     main = "Cinema expenses depending on earnings")

# estimate y for multiple x
X <- 1:10
Y <- a + b * X

lines(X, Y)
