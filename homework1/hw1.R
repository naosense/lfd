source("common.R", echo = F)
source("homework1/pla.R", echo = F)
source("homework1/pocket.R", echo = F)
hw1_15_train <- as.matrix(read.table("homework1/hw1_15_train.dat", stringsAsFactors = T))
hw1_18_train <- as.matrix(read.table("homework1/hw1_18_train.dat", stringsAsFactors = T))
hw1_18_test <- as.matrix(read.table("homework1/hw1_18_test.dat", stringsAsFactors = T))


result <- pla(hw1_15_train[, 1:4], v(hw1_15_train, 5), 1, "order")
message(paste("Run pla with order w is", result[1], "times is", result[[2]]))
pause()

message('Run pla with random for 2000 times')
times_array <- rep(0, 2000)
times_sum <- 0
for (i in 1:2000) {
  cat("=")
  set.seed(i)
  result <- pla(hw1_15_train[, 1:4], v(hw1_15_train, 5), 1, "random")
  times_sum <- times_sum + result[[2]]
  times_array[i] <- result[[2]]
}
message("Average times of pla is ", times_sum / 2000)
hist(times_array)
pause()

result <- pocket(hw1_15_train[, 1:4], v(hw1_15_train, 5), 1, "order")
message(paste("Run pocket with order, w is", result[1], "times is", result[[2]]))
pause()

message("Run pocket with random for 2000 times")
err_array <- rep(0, 2000)
err_sum <- 0
for (i in 1:2000) {
  cat("=")
  set.seed(i)
  result <- pocket(hw1_18_train[, 1:4], v(hw1_18_train, 5), 1, "random")
  x <- hw1_18_test[, 1:4]
  X <- cbind(as.matrix(rep(1, nrow(x))), x)
  y <- v(hw1_18_test, 5)
  w <- result[[1]]
  err <- sum(sign(X %*% w) != y) / nrow(x)
  err_sum <- err_sum + err
  err_array[i] <- err
}
message("Average error of pocket is ", err_sum / 2000)
pause()

hist(err_array)
