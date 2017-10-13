source("common.R")
cart_decision_tree <- function(data, type = "class") {
  impurify <- function(data, sp, j) {
    left <- NULL
    right <- NULL
    if (is.factor(sp) || is.character(sp)) {
      left <- h(data, data[, j] == sp)
      right <- h(data, data[, j] != sp)
    } else {
      left <- h(data, data[, j] < sp)
      right <- h(data, data[, j] >= sp)
    }
    N <- nrow(data)
    impurify <- sum(nrow(left)/N * impurify_one_group(left), nrow(right)/N *
      impurify_one_group(right))
    list(impurify = impurify, sp = sp, left = left, right = right)
  }

  impurify_one_group <- function(data) {
    if (nrow(data) == 0) {
      return(0)
    }
    y <- data[, ncol(data)]
    N <- nrow(data)
    clazz <- unique(y)
    f <- function(c) {
      p <- sum(y == c)/N
      p * (1 - p)
    }
    sum(sapply(clazz, f))
  }

  split_branch <- function(data) {
    marjority <- function(v) {
      name <- names(which.max(table(v)))
    }
    if (nrow(data) == 0) {
      return()
    }
    rows <- nrow(data)
    cols <- ncol(data)

    X <- v(data, 1:(cols - 1))
    y <- v(data, cols)

    clazz <- unique(y)
    if (nrow(clazz) == 1) {
      # only one class stop
      return(as.character(clazz[1, 1]))
    } else if (nrow(unique(X)) == 1) {
      # same x stop
      return(marjority(y))
    } else {
      min_impurify <- 10
      min_sp <- 0
      ind <- 0
      left <- NULL
      right <- NULL
      for (j in 1:(cols - 1)) {
        sps <- unique(data[, j])
        for (sp in sps) {
          res <- impurify(data, sp, j)
          if (min_impurify > res[["impurify"]]) {
          min_impurify <- res[["impurify"]]
          ind <- j
          min_sp <- sp
          left <- res[["left"]]
          right <- res[["right"]]
          }
        }
      }
      # if(nrow(left) == 0 || nrow(right) == 0) { return(marjority(y)) }
      tree <- list(name = names(data)[ind], ind = ind, sp = min_sp, left = split_branch(left),
        right = split_branch(right))
    }
    tree
  }
  split_branch(data)
}

predict_tree <- function(tree, x) {
  if (is.atomic(tree)) {
    return(tree)
  } else {
    if (is.factor(tree[["sp"]]) || is.character(tree[["sp"]])) {
      if (x[1, tree[["ind"]]] == tree[["sp"]]) {
        return(predict_tree(tree[["left"]], x))
      } else {
        return(predict_tree(tree[["right"]], x))
      }
    } else {
      if (x[1, tree[["ind"]]] < tree[["sp"]]) {
        return(predict_tree(tree[["left"]], x))
      } else {
        return(predict_tree(tree[["right"]], x))
      }
    }
  }
}

print_tree <- function(tree, prefix = "    ") {
  print_inner <- function(tree, prefix, step) {
    if (is.atomic(tree)) {
      cat(tree)
    } else {
      cat(paste(ifelse(is.null(tree[["name"]]), paste("X", tree[["ind"]], sep = ""),
        tree[["name"]]), ifelse(is.factor(tree[["sp"]]) || is.character(tree[["sp"]]),
        "=", "<"), tree[["sp"]], ":\n", sep = ""))
      cat(paste(prefix, "[L]", sep = ""))
      print_inner(tree[["left"]], paste(prefix, step, sep = ""), step)
      cat("\n")
      cat(paste(prefix, "[R]", sep = ""))
      print_inner(tree[["right"]], paste(prefix, step, sep = ""), step)
    }
  }
  print_inner(tree, prefix, prefix)
}

random_forest <- function(data, ts = 10, feature_count = as.integer(sqrt(ncol(data)))) {
  set.seed(1)
  cart_decision_tree <- function(data, type = "class") {
    impurify <- function(data, sp, j) {
      left <- NULL
      right <- NULL
      if (is.factor(sp) || is.character(sp)) {
        left <- h(data, data[, j] == sp)
        right <- h(data, data[, j] != sp)
      } else {
        left <- h(data, data[, j] < sp)
        right <- h(data, data[, j] >= sp)
      }
      N <- nrow(data)
      impurify <- sum(nrow(left)/N * impurify_one_group(left), nrow(right)/N *
        impurify_one_group(right))
      list(impurify = impurify, sp = sp, left = left, right = right)
    }

    impurify_one_group <- function(data) {
      if (nrow(data) == 0) {
        return(0)
      }
      y <- data[, ncol(data)]
      N <- nrow(data)
      clazz <- unique(y)
      f <- function(c) {
        p <- sum(y == c)/N
        p * (1 - p)
      }
      sum(sapply(clazz, f))
    }

    split_branch <- function(data) {
      marjority <- function(v) {
        name <- names(which.max(table(v)))
      }
      if (nrow(data) == 0) {
        return()
      }
      rows <- nrow(data)
      cols <- ncol(data)

      feature_selected <- sample(1:(cols - 1), feature_count, replace = T)

      X <- v(data, feature_selected)
      y <- v(data, cols)

      clazz <- unique(y)
      if (nrow(clazz) == 1) {
        # only one class stop
        return(as.character(clazz[1, 1]))
      } else if (nrow(unique(X)) == 1) {
        # same x stop
        return(marjority(y))
      } else {
        min_impurify <- 10
        min_sp <- 0
        ind <- 0
        left <- NULL
        right <- NULL
        uniq_cols <- unique(feature_selected)
        for (j in uniq_cols) {
          sps <- unique(data[, j])
          for (sp in sps) {
          res <- impurify(data, sp, j)
          if (min_impurify > res[["impurify"]]) {
            min_impurify <- res[["impurify"]]
            ind <- j
            min_sp <- sp
            left <- res[["left"]]
            right <- res[["right"]]
          }
          }
        }
        # if(nrow(left) == 0 || nrow(right) == 0) { return(marjority(y)) }
        tree <- list(name = names(data)[ind], ind = ind, sp = min_sp, left = split_branch(left),
          right = split_branch(right))
      }
      tree
    }
    split_branch(data)
  }
  predict_forest <- function(trees, data) {
    marjority <- function(v) {
      name <- names(which.max(table(v)))
    }
    vote <- function(trees, x) {
      sapply(trees, function(tree) predict_tree(tree, x))
    }
    N <- nrow(data)
    sapply(1:N, function(r) marjority(vote(trees, data[r, ])))
  }

  predict_tree_in <- function(tree, data) {
    N <- nrow(data)
    sapply(1:N, function(x) predict_tree(tree, data[x, ]))
  }

  N <- nrow(data)
  M <- ncol(data)
  test <- sample(1:N, as.integer(0.1 * N))
  train <- setdiff(1:N, test)
  y <- data[, M]
  trees <- rep(list(NULL), ts)
  oob_errors <- rep(0, ts)
  importance <- matrix(rep(0, ts * (M - 1)), nrow = ts)
  yt <- rep(0, ts)
  invisible(capture.output(pb <- txtProgressBar(min = 0, max = 100, width = 80, style = 3)))
  for (i in 1:ts) {
    row_selected <- sample(train, length(train), replace = T)
    oob <- setdiff(train, row_selected)
    trees[[i]] <- cart_decision_tree(data[row_selected, ])
    oob_errors[i] <- sum(predict_tree_in(trees[[i]], data[oob, ]) != y[oob])/length(oob)
    # oob_permute <- sample(oob, length(oob))
    importance[i, ] <- sapply(1:(M - 1), function(c) {
      data_permute <- data[oob, ]
      data_permute[, c] <- sample(data_permute[, c], length(oob))
      oob_error_permute <- sum(predict_tree_in(trees[[i]], data_permute) !=
        y[oob])/length(oob)
      abs(oob_errors[i] - oob_error_permute)
    })
    setTxtProgressBar(pb, i / ts * 100)
  }

  oob_error <- sum(oob_errors)/length(oob_errors)
  test_error <- sum(predict_forest(trees, data[test, ]) != y[test])/length(test)
  plot(1:ts, oob_errors)
  importance <- apply(importance, 2, sum)/ts
  names(importance) <- names(data)[1:(M - 1)]
  importance <- as.data.frame(importance[order(-importance)], optional = T)
  return(list(trees = trees, oob_error = oob_error, test_error = test_error, importance = importance))
}