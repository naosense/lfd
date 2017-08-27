colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
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
    plot(xlim, f(xlim), "l", lwd = 2)
    points(x, y, pch = 20, col = colors[1])
  } else if (n == 2) {
    x1lim <- lim(x[, 1, drop = F])
    x2lim <- lim(x[, 2, drop = F])
    z <- outer(x1lim, x2lim, f);
    contour(x1lim, x2lim, z, levels = c(0), drawlabels = F, lwd = 2);
    yk <- unique(y);
    for (i in seq_along(yk)) {
      ind <- y == yk[i]
      points(v(x, 1)[ind], v(x, 2)[ind], pch = 20, col = colors[i]);
    }
  }

}

lim <- function(x) {
  xmin <- min(x)
  xmax <- max(x)
  xinterval <- xmax - xmin
  xlim <- seq(xmin - .1 * xinterval, xmax + .1 * xinterval, length.out = 100)
}