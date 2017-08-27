get_deriv <- function(w, X, y, method = "random") {
  m <- nrow(X)
  g <- as.matrix(rep(0, nrow(w)))
  if (method == "random") {
    i <- sample(1:m, 1)
    g <- g + -1 * y[i] * t(h(X, i)) / c(1 + exp(y[i] * h(X, i) %*% w))
  } else {
    for (i in 1:m) {
      g <- g + -1 / m * y[i] * t(h(X, i)) / c(2 + exp(y[i] * h(X, i) %*% w))
    }
  }
  g
}