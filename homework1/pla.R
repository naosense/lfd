pla <- function(x, y, ratio, method) {
  rs <- nrow(x)
  cs <- ncol(x)
  X <- cbind(as.matrix(rep(1, rs)), x)
  w <- as.matrix(rep(0, cs + 1))
  times <- 0
  while (any((Y <- sign(X %*% w)) != y)) {
    index <- which(Y != y)
    if (method == "order") {
      r = min(index)
    } else {
      r = sample(index, size = 1)
    }
    w <- w + ratio * y[r] * t(h(X, r))
    times <- times + 1
  }
  return(list(w, times))
}
