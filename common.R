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

is_empty <- function(x) {
  ifelse(is.null(dim(x)), length(x) <= 0L, prod(dim(x)) <= 0L)
}

normlize <- function(v, center = mean) {
  if (is_empty(v)) {
    return(v)
  }
  md <- center(v)
  sd <- sqrt(sum((v - md) ^ 2) / length(v))
  if (sd == 0) {
    v - md
  } else {
    (v - md) / sd
  }
}

marjority <- function(v) {
  ifelse(length(v) <= 0L, 0L, as.integer(names(which.max(table(v)))))
}

is_same <- function(x) {
  ifelse(is.null(dim(x)), length(unique(x)) == 1L, nrow(unique(x)) == 1L)
}

rsquare <- function(y, pred) {
  tss <- sum((y - mean(y)) ^ 2)
  rss <- sum((y - pred) ^ 2)
  r2 <- 1 - rss / tss
}

cross_validate <- function(data, train_fun, predict_fun, n = 10, ...) {
  N <- nrow(data)
  nsamples <- seq.int(0, nrow(data), length.out = n + 1)
  cat(nsamples, "\n")
  invisible(capture.output(pb <- txtProgressBar(0, 100, width = 80, style = 3)))
  err <- vapply(1:n, function(i) {
    train_data <- data[-((nsamples[i] + 1):nsamples[i + 1]),]
    test_data <- data[(nsamples[i] + 1):nsamples[i + 1],]
    model <- train_fun(train_data, ...)
    pred_train <- predict_fun(model, train_data)
    pred_test <- predict_fun(model, test_data)
    ein <- sum(pred_train != train_data[, ncol(train_data)]) / nrow(train_data)
    eout <- sum(pred_test != test_data[, ncol(test_data)]) / nrow(test_data)
    setTxtProgressBar(pb, i / n * 100L)
    c(ein, eout)
  }, numeric(2))
  rowMeans(err)
}
