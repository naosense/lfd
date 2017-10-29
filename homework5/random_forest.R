source("common.R")
random_forest <- function(data, ts = 10L, feature_count = as.integer(sqrt(ncol(data)))) {
  marjority <- function(v) {
    ifelse(length(v) <= 0L, 0L, as.integer(names(which.max(table(v)))))
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
      if (is_empty(data)) {
        return(0L)
      }
      y <- data[, ncol(data)]
      N <- nrow(data)
      p <- table(y) / N
      sum(p * (1 - p))
    }

    split_branch <- function(data) {
      if (is_empty(data)) {
        return(0L)
      }
      rows <- nrow(data)
      cols <- ncol(data)

      feature_selected <- sample(1:(cols - 1), feature_count, replace = T)

      X <- v(data, feature_selected)
      y <- v(data, cols)

      clazz <- unique(y)
      if (nrow(clazz) == 1L) {
        # only one class stop
        return(as.integer(clazz[1, 1]))
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
        tree <- list(ind = min_res[["ind"]],
                     sp = min_res[["sp"]],
                     left = split_branch(min_res[["left"]]),
                     right = split_branch(min_res[["right"]]))
      }
      tree
    }
    split_branch(data)
  }
  predict_forest <- function(trees, data) {
    if (length(trees) <= 0L) {
      return(0L)
    }
    N <- nrow(data)
    if (is.null(N) || N == 1L) {
      marjority(vapply(trees, function(t) predict_tree(t, data), numeric(1)))
    } else {
      predict_matrix <- vapply(trees, function(t) predict_tree(t, data), numeric(N))
      vapply(1:N,function(r) marjority(predict_matrix[r,]), numeric(1))
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
      vapply(1:nrow(data), function(r) predict_tree_inner(tree, h(data, r)), numeric(1))
    }
  }

  N <- nrow(data)
  M <- ncol(data)
  origin_data <- data
  origin_name <- colnames(data)
  type_array <- vapply(1:M, function(c) is.factor(data[1, c]) || is.character(data[1, c]), logical(1))
  # matrix is 3-4 times faster than dataframe
  data <- data.matrix(data)
  y <- data[, M]
  oob_matrix <- replicate(ts, (function() {
    a <- rep(F, N)
    a[sample(1:N, N, replace = T)] <- T
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
  predict_matrix <- vapply(trees, function(t) predict_tree(t, data), numeric(N))
  oob_error <- sum(vapply(1:N, function(r) {
    marjority(predict_matrix[r,][!oob_matrix[r, ]])
  }, numeric(1)) != y) / N

  message("Computing feature importance...")
  importance <- vapply(1:(M - 1), function(j) {
    data[, j] <- sample(data[, j], N)
    oob_error_perm <- sum(vapply(1:N, function(i) {
      predict_forest(trees[!oob_matrix[i, ]], h(data, i))
    }, numeric(1)) != y) / N
    abs(oob_error_perm - oob_error)
  }, numeric(1))
  names(importance) <- origin_name[1:(M - 1)]
  importance <- as.data.frame(importance[order(-importance)], optional = T)

  message("Computing proximities...")
  proximities <- vapply(1:N, function(i, m) colSums(predict_matrix[i,] == m), numeric(N), m = t(predict_matrix)) / ts

  message("Computing outliers...")
  uniq_y <- unique(y)
  names(uniq_y) <- uniq_y
  index_matrix <- vapply(uniq_y, function(i) y == i, logical(N))
  outliers <- vapply(1:N, function(i) sum(index_matrix[, as.character(y[i])]) / sum(proximities[index_matrix[, as.character(y[i])], i] ^ 2), numeric(1))
  for (i in uniq_y) {
    index <- which(y == i)
    outliers[index] <- normlize(outliers[index], median)
  }
  message("Done.")
  return(list(trees = trees, oob_error = oob_error, importance = importance, proximities = proximities, data=origin_data, outliers=outliers))
}

predict_forest <- function(rf, data) {
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
      vapply(1:nrow(data), function(r) predict_tree_inner(tree, h(data, r)), numeric(1))
    }
  }

  origin_data <- rf[["data"]]
  trees <- rf[["trees"]]
  stopifnot(identical(names(origin_data), names(data)))
  if (length(trees) <= 0L) {
    return(0L)
  }
  N <- nrow(data)
  M <- ncol(data)
  ylevel <- levels(data[, M])
  type_array <- vapply(1:M, function(c) is.factor(data[1, c]) || is.character(data[1, c]), logical(1))
  data <- data.matrix(data)
  yint <- 0L
  if (is.null(N) || N == 1L) {
    yint <- marjority(vapply(trees, function(t) predict_tree(t, data), numeric(1)))
  } else {
    predict_matrix <- vapply(trees, function(t) predict_tree(t, data), numeric(N))
    yint <- vapply(1:N,function(r) marjority(predict_matrix[r,]), numeric(1))
  }
  if (type_array[M]) {
    ylevel[yint]
  } else {
    yint
  }
}