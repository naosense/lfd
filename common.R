pause <- function() {
  readline(prompt="Press [enter] to continue")
}

h <- function(M, n) {
  M[n, , drop = F]
}

v <- function(M, n) {
  M[, n, drop = F]
}

plot_fit <- function(f, x, y) {
  n <- ncol(x)
  if (n == 1) {
    xlim <- lim(x)
    plot(xlim, f(xlim), "l")
    points(x, y)
  } else if (n == 2) {
    x1lim <- lim(x[, 1, drop = F])
    x2lim <- lim(x[, 2, drop = F])
    z <- outer(x1lim, x2lim, f);
    contour(x1lim, x2lim, z, levels = c(0), drawlabels = F);
    yk <- unique(y);
    for (i in seq_along(yk)) {
      ind <- y == yk[i]
      points(x[ind, 1, drop = F], x[ind, 2, drop = F], pch = i);
    }
  }

}

lim <- function(x) {
  xmin <- min(x)
  xmax <- max(x)
  xinterval <- xmax - xmin
  xlim <- seq(xmin - .1 * xinterval, xmax + .1 * xinterval, length.out = 100)
}