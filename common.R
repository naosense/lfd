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

cross_validate <- function(data, train_fun, predict_fun, n = 10, type = "class", ...) {
  N <- nrow(data)
  data <- data[sample(N),]
  sections <- seq.int(0, nrow(data), length.out = n + 1)
  cat("Sections is", sections, "\n")
  err <- vapply(1:n, function(i) {
    train_data <- data[-((sections[i] + 1):sections[i + 1]),]
    test_data <- data[(sections[i] + 1):sections[i + 1],]
    model <- train_fun(train_data, type = type, ...)
    pred_train <- predict_fun(model, train_data)
    pred_test <- predict_fun(model, test_data)
    if (type == "class") {
      ein <- sum(pred_train != train_data[, ncol(train_data)]) / nrow(train_data)
      eout <- sum(pred_test != test_data[, ncol(test_data)]) / nrow(test_data)
      res <- c(ein, eout)
      cat(paste0("#", i), "Error is", res, "\n")
      res
    } else if (type == "regression") {
      r2in <- rsquare(train_data[, ncol(train_data)], pred_train)
      r2out  <- rsquare(test_data[, ncol(test_data)], pred_test)
      res <- c(r2in, r2out)
      cat(paste0("#", i), "R2 is", res, "\n")
      res
    }
  }, numeric(2))
  rowMeans(err)
}

plot_decision_boundary <- function(model, data, predict_fun, title, ...) {
  ylevel <- NULL
  if (is.factor(data[, 3])) {
    ylevel <- levels(data[, 3])
  } else {
    ylevel <- unique(data[, 3])
  }
  nlevel <- length(ylevel)
  data <- data.matrix(data)
  plot(data[, 1:2], main = title, pch = data[, 3], col = colors[data[, 3]])
  legend("topleft", ylevel, pch = 1:nlevel, col = colors[1:nlevel], bty = "n")

  rangex <- range(data[, 1])
  rangey <- range(data[, 2])

  xg <- seq.int(rangex[1], rangex[2], length.out = 100)
  yg <- seq.int(rangey[1], rangey[2], length.out = 100)
  grid <- expand.grid(xg, yg, 0)
  names(grid) <- colnames(data)
  grid[, 3] <- predict_fun(model, grid, ...)
  points(grid[, 1:2], col = colors[grid[, 3]], pch = ".")

  z <- matrix(grid[, 3], nrow = 100)
  contour(xg, yg, z, add = T, drawlabels = F, levels = 1:(nlevel - 1) + .5, lwd = 2)
}

