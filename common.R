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

plot_fit <- function(f, x, y, fit = T, formula = T, ...) {
  lim <- function(x) {
    xmin <- min(x)
    xmax <- max(x)
    xinterval <- xmax - xmin
    xlim <- seq(xmin - .1 * xinterval, xmax + .1 * xinterval, length.out = 100)
  }

  n <- ncol(x)
  if (n == 1) {
    xlim <- lim(x)
    if (fit) {
      plot(xlim, f(xlim), "l", lwd = 2, ...)
    } else {
      plot(xlim, f(xlim), "n", ...)
    }
    points(x, y, pch = 20, col = colors[1])
  } else if (n == 2) {
    x1lim <- lim(x[, 1, drop = F])
    x2lim <- lim(x[, 2, drop = F])
    z <- outer(x1lim, x2lim, f);
    if (fit) {
      contour(x1lim, x2lim, z, levels = c(0), drawlabels = F, lwd = 2, ...);
    } else {
      plot(x1lim, x2lim, type = "n", ...)
    }
    yk <- unique(y);
    for (i in seq_along(yk)) {
      ind <- y == yk[i]
      points(v(x, 1)[ind], v(x, 2)[ind], pch = 20, col = colors[i]);
    }
  }
  if (formula) {
    er <- deparse(body(f), width.cutoff = 500)
    w_len <- length(unlist(strsplit(er, split = "\\+")))
    for (i in 1:w_len) {
      er <- gsub(paste("w\\[",i, "\\]", sep = ""), round(w[i], digits = 2), er)
    }
    mtext(parse(text = paste("hat(y)==", er)), adj = 0.2, line = -2)
  }
}