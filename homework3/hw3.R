source("common.R", echo = F)
source("homework3/get_deriv.R", echo = F)
source("homework3/sigmoid.R", echo = F)
source("homework3/predict.R", echo = F)

hw3_train <-  as.matrix(read.table("homework3/hw3_train.dat", stringsAsFactors = T))
hw3_test <-  as.matrix(read.table("homework3/hw3_test.dat", stringsAsFactors = T))

# 13-15 linear regression
N <- 1000
x <- matrix(2 * runif(2 * N) - 1, nrow = N)
noise <- as.matrix(runif(N))
noise[noise <= .1] <- -1
noise[noise > .1] <- 1
# y <- sign(v(x, 1) ^ 2 + v(x, 2) ^ 2 - .6) * noise
y <- (v(x, 1) ^ 2 + v(x, 2) ^ 2 - .6) * noise
yy <- sign(y)

X <- cbind(rep(1, N), x)
w <- solve(t(X) %*% X) %*% t(X) %*% y
yt <- sign(X %*% w)
message("Linear regress error is ", sum(yt != yy) / N)
pause()
message("Plot linear fit on data")
f <- function(x1, x2) w[1] + w[2] * x1 + w[3] * x2
plot_fit(f, x, yy)
pause()

# feature transform
X <- cbind(rep(1, N), x, v(x, 1) * v(x, 2), x ^ 2)
w <- solve(t(X) %*% X) %*% t(X) %*% y
yt <- sign(X %*% w)
message("Linear regression after feature transform error is ", sum(yt != yy) / N)
pause()
message("Plot feature transform line fit on data")
f <- function(x1, x2) w[1] + w[2] * x1 + w[3] * x2 + w[4] * x1 * x2 + w[5] * x1 ^2 + w[6] * x2 ^ 2
plot_fit(f, x, yy)
pause()

# logistic regression
message("Compute logistic train error")
alpha <- .01
T <- 2000
m <- nrow(hw3_train)
n <- ncol(hw3_train)
x <- hw3_train[, 1:n-1]
y <- v(hw3_train, n)
X <- cbind(rep(1, m), x)
w <- as.matrix(rep(0, n))
err_array <- rep(0, T)
for (i in 1:T) {
  cat("=")
  w <- w + alpha * -1 * get_deriv(w, X, y)
  yp <- predict(w, X)
  err_array[i] <- sum(yp != y) / m
}
plot(1:T, err_array, pch = 20)
pause()
message("Compute logistic test error")
x <- hw3_test[, 1:n-1]
y <- v(hw3_test, n)
m <- nrow(hw3_test)
X <- cbind(rep(1, m), x)
yp <- predict(w, X)
message("Logistic regression error is ", sum(yp != y) / m)