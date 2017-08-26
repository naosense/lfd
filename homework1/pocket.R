pocket <- function(x, y, ratio, method) {
  rs <- nrow(x)
  cs <- ncol(x)
  X <- cbind(matrix(rep(1, rs)), x)
  w <- w_min <- matrix(rep(0, cs + 1))
  err <- err_min <- 10
  for (i in 1:100) {
    if (any((Y <- sign(X %*% w)) != y)) {
      err <- sum(Y != y) / rs
      if (err_min > err) {
        w_min <- w
        err_min <- err
      }
      index <- which(Y != y)
      if (method == "order") {
        r = min(index)
      } else {
        r = sample(index, size = 1)
      }
      w <- w + ratio * y[r] * t(h(X, r))
    } else {
      break
    }
  }
  return(list(w_min, err_min))
}