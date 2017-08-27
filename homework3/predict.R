predict <- function(w, X) {
  y <- sigmoid(X %*% w)
  y[y >= .5] <- 1
  y[y < .5] <- -1
  y
}