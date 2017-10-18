source("common.R")
random_forest <- function(data, ts = 10, feature_count = as.integer(sqrt(ncol(data)))) {
  set.seed(1)
  marjority <- function(v) {
    name <- names(which.max(table(v)))
  }
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
    vote <- function(trees, x) {
      sapply(trees, function(tree) predict_tree(tree, x))
    }
    if (length(trees) <= 0) {
      return("NULL")
    }
    N <- nrow(data)
    sapply(1:N, function(i) marjority(vote(trees, h(data, i))))
  }

  predict_tree_in <- function(tree, data) {
    N <- nrow(data)
    sapply(1:N, function(i) predict_tree(tree, h(data, i)))
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

  N <- nrow(data)
  M <- ncol(data)
  test <- sample(1:N, as.integer(0.1 * N))
  train <- setdiff(1:N, test)
  y <- data[, M]
  trees <- rep(list(NULL), ts)
  # oob_errors <- rep(0, ts)
  oob_matrix <- matrix(rep(0, N * ts), nrow = N)
  importance <- rep(0, M - 1)
  proximities <- matrix(rep(0, N * N), nrow = N)
  yt <- rep(0, ts)
  invisible(capture.output(pb <- txtProgressBar(0, 100, width = 80, style = 3)))
  for (i in 1:ts) {
    row_selected <- sample(train, length(train), replace = T)
    trees[[i]] <- cart_decision_tree(data[row_selected, ])
    oob_matrix[row_selected, i] <- 1
    setTxtProgressBar(pb, i/ts * 100)
  }

  oob_error <- sum(sapply(train, function(i) {
    predict_forest(trees[oob_matrix[i, ] == 0], h(data, i)) != y[i]
  }))/length(train)

  importance <- sapply(1:(M - 1), function(j) {
    data[train, j] <- sample(data[train, j], length(train))
    oob_error_perm <- sum(sapply(train, function(i) {
      predict_forest(trees[oob_matrix[i, ] == 0], h(data, i)) != y[i]
    }))/length(train)
    abs(oob_error_perm - oob_error)
  })

  predict_matrix <- vapply(trees, function(t) predict_tree_in(t, data), FUN.VALUE = character(N))
  proximities <- vapply(1:N, function(i) colSums(predict_matrix[i,] == t(predict_matrix)), FUN.VALUE = double(N))
  test_error <- sum(predict_forest(trees, data[test, ]) != y[test])/length(test)
  names(importance) <- names(data)[1:(M - 1)]
  importance <- as.data.frame(importance[order(-importance)], optional = T)
  return(list(trees = trees, oob_error = oob_error, test_error = test_error, importance = importance, proximities = proximities, data=data))
}