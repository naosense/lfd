source("common.R")
random_forest <- function(data, ts = 10L, feature_count = as.integer(sqrt(ncol(data)))) {
  marjority <- function(v) {
    ifelse(length(v) <= 0L, y_type(1), names(which.max(table(v))))
  }
  cart_decision_tree <- function(data, type = "class") {
    impufity <- function(data, sp, j) {
      if (type_array[j]) {
        left_cond <- data[, j] == sp
      } else {
        left_cond <- data[, j] < sp
      }
      left <- h(data, left_cond)
      right <- h(data, !left_cond)
      N <- nrow(data)
      impufity <- sum(nrow(left) / N * impurity_one_group(left),
                      nrow(right) / N * impurity_one_group(right))
      list(impufity = impufity, ind = j, sp = sp, left = left, right = right)
    }

    impurity_one_group <- function(data) {
      if (nrow(data) == 0L) {
        return(0L)
      }
      y <- data[, ncol(data)]
      N <- nrow(data)
      p <- table(y) / N
      sum(p * (1 - p))
    }

    split_branch <- function(data) {
      if (nrow(data) == 0L) {
        return()
      }
      rows <- nrow(data)
      cols <- ncol(data)

      feature_selected <- sample(1:(cols - 1), feature_count, replace = T)

      X <- v(data, feature_selected)
      y <- v(data, cols)

      clazz <- unique(y)
      if (nrow(clazz) == 1L) {
        # only one class stop
        return(as.character(clazz[1, 1]))
      } else if (nrow(unique(X)) == 1L) {
        # same x stop
        return(marjority(y))
      } else {
        min_res <- list(impufity = 10L, ind = 0L, sp = 0L, left = NULL, right = NULL)
        uniq_cols <- unique(feature_selected)
        for (j in uniq_cols) {
          sps <- unique(data[, j])
          for (sp in sps) {
            res <- impufity(data, sp, j)
            if (min_res[["impufity"]] > res[["impufity"]]) {
              min_res <- res
            }
          }
        }
        tree <- list(name = names[min_res[["ind"]]],
                     ind = min_res[["ind"]],
                     sp = min_res[["sp"]],
                     left = split_branch(min_res[["left"]]),
                     right = split_branch(min_res[["right"]]))
      }
      tree
    }
    names <- colnames(data)
    split_branch(data)
  }
  predict_forest <- function(trees, data) {
    if (length(trees) <= 0L) {
      return(y_type(1))
    }
    N <- nrow(data)
    if (is.null(N) || N == 1L) {
      marjority(vapply(trees, function(t) predict_tree(t, data), y_type(1)))
    } else {
      predict_matrix <- vapply(trees, function(t) predict_tree(t, data), y_type(N))
      vapply(1:N,function(r) marjority(predict_matrix[r,]), y_type(1))
    }
  }

  predict_tree <- function(tree, data) {
    predict_tree_inner <- function(tree, x) {
      if (is.atomic(tree)) {
        return(tree)
      } else {
        if (type_array[tree[["ind"]]]) {
          if (x[tree[["ind"]]] == tree[["sp"]]) {
            return(predict_tree_inner(tree[["left"]], x))
          } else {
            return(predict_tree_inner(tree[["right"]], x))
          }
        } else {
          if (x[tree[["ind"]]] < tree[["sp"]]) {
            return(predict_tree_inner(tree[["left"]], x))
          } else {
            return(predict_tree_inner(tree[["right"]], x))
          }
        }
      }
    }
    if (is.null(nrow(data)) || nrow(data) == 1L) {
      predict_tree_inner(tree, data)
    } else {
      vapply(1:nrow(data), function(r) predict_tree_inner(tree, h(data, r)), y_type(1))
    }
  }

  N <- nrow(data)
  M <- ncol(data)
  origin_name <- colnames(data)
  type_array <- vapply(1:M, function(c) is.factor(data[1, c]) || is.character(data[1, c]), logical(1))
  y_type <- ifelse(type_array[M], character, numeric)
  data <- data.matrix(data)
  test <- sample(1:N, as.integer(0.1 * N))
  train <- setdiff(1:N, test)
  y <- data[, M]
  oob_matrix <- replicate(ts, (function() {
    a <- rep(F, N)
    a[sample(train, length(train), replace = T)] <- T
    a
  })())
  message("Trainning forest...")
  invisible(capture.output(pb <- txtProgressBar(0, 100, width = 80, style = 3)))
  trees <- lapply(1:ts, function(i) {
    tree <- cart_decision_tree(data[oob_matrix[, i], ])
    setTxtProgressBar(pb, i / ts * 100L)
    tree
  })
  message("\nComputing oob error...")
  predict_matrix <- vapply(trees, function(t) predict_tree(t, data), y_type(N))
  oob_error <- sum(vapply(train, function(r) {
    marjority(predict_matrix[r,][!oob_matrix[r, ]])
  }, y_type(1)) == y[train]) / length(train)

  message("Computing feature importance...")
  importance <- vapply(1:(M - 1), function(j) {
    data[train, j] <- sample(data[train, j], length(train))
    oob_error_perm <- sum(vapply(train, function(i) {
      predict_forest(trees[!oob_matrix[i, ]], h(data, i))
    }, FUN.VALUE = y_type(1)) == y[train]) / length(train)
    abs(oob_error_perm - oob_error)
  }, FUN.VALUE = double(1))

  message("Computing proximities...")
  proximities <- vapply(1:N, function(i, m) colSums(predict_matrix[i,] == m), FUN.VALUE = double(N), m = t(predict_matrix))
  outliers <- N / colSums(proximities ^ 2)
  uniq_y <- unique(y)
  for (i in uniq_y) {
    index <- which(y == i)
    sd <- sd(outliers[index])
    if (is.na(sd) || sd == 0L) {
      next()
    }
    outliers[index] <- (outliers[index] - mean(outliers[index])) / sd
  }
  test_error <- sum(predict_forest(trees, data[test, ]) != y[test]) / length(test)
  names(importance) <- origin_name[1:(M - 1)]
  importance <- as.data.frame(importance[order(-importance)], optional = T)
  message("Done.")
  return(list(trees = trees, oob_error = oob_error, test_error = test_error, importance = importance, proximities = proximities, data=data, outliers=outliers))
}