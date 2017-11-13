source("common.R")
random_forest <- function(data, type = "class", ts = 10L, feature_count = floor(sqrt(ncol(data))), node_size = 1) {
  cart_decision_tree <- function(data, type = "class") {
    impufity <- function(rows, sp, j) {
      left_index <- 0L
      if (type_array[j]) {
        left_index <- which(data[rows, j] == sp)
      } else {
        left_index <- which(data[rows, j] < sp)
      }
      left <- rows[left_index]
      right <- rows[-left_index]

      # Sepal.Length Sepal.Width Species
      # [1,]          5.6         2.5       2
      # [2,]          5.7         2.8       2
      # [3,]          5.7         2.5       3
      # [4,]          5.6         2.8       3
      impufity <- 0L
      if (is_empty(left) || is_empty(right)) {
        impufity <- Inf
      } else {
        N <- length(rows)
        impufity <- sum(length(left) / N * impurity_one_group(left),
                        length(right) / N * impurity_one_group(right))
      }

      list(impufity = impufity, ind = j, sp = sp, left = left, right = right)
    }

    impurity_one_group <- function(rows) {
      if (is_empty(rows)) {
        return(0L)
      }
      y <- data[rows, ncol(data)]
      N <- length(rows)
      if (type == "class") {
        p <- table(y) / N
        sum(p * (1 - p))
      } else if (type == "regression") {
        sum((y - mean(y)) ^ 2) / N
      } else {
        return(0L)
      }
    }

    split_branch <- function(rows) {
      # print(rows)
      if (is_empty(rows)) {
        return(0L)
      }
      # rows <- nrow(data)
      ncol <- ncol(data)

      feature_selected <- sample(ncol - 1, feature_count, replace = T)

      X <- data[rows, feature_selected, drop = F]
      y <- data[rows, ncol, drop = F]

      if (nrow(y) <= node_size || is_same(y) || is_same(X)) {
        return(ifelse(type == "class", marjority(y), mean(y)))
      } else {
        min_res <- list(impufity = Inf, ind = 0L, sp = 0L, left = NULL, right = NULL)
        uniq_cols <- unique(feature_selected)
        for (j in uniq_cols) {
          sps <- unique(data[rows, j])
          for (sp in sps) {
            res <- impufity(rows, sp, j)
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
    data <- unique(data)
    split_branch(1:nrow(data))
  }
  predict_forest <- function(trees, data) {
    if (is_empty(trees)) {
      return(ifelse(type == "class", 0L, NA))
    }
    N <- nrow(data)
    if (type == "class") {
      if (N == 1L) {
        marjority(vapply(trees, function(t) predict_tree(t, data), numeric(1)))
      } else {
        predict_matrix <- vapply(trees, function(t) predict_tree(t, data), numeric(N))
        vapply(1:N,function(r) marjority(predict_matrix[r,]), numeric(1))
      }
    } else if (type == "regression") {
      if (N == 1L) {
        mean(vapply(trees, function(t) predict_tree(t, data), numeric(1)))
      } else {
        predict_matrix <- vapply(trees, function(t) predict_tree(t, data), numeric(N))
        rowMeans(predict_matrix)
      }
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
    vapply(1:nrow(data), function(r) predict_tree_inner(tree, h(data, r)), numeric(1))
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
    a[sample(N, replace = T)] <- T
    a
  })())

  message("Trainning forest...")
  invisible(capture.output(pb <- txtProgressBar(0, 100, width = 80, style = 3)))
  trees <- lapply(1:ts, function(i) {
    tree <- cart_decision_tree(data[oob_matrix[, i], ])
    setTxtProgressBar(pb, i / ts * 100L)
    tree
  })

  if (type == "class") {
    message("\nComputing oob error...")
    predict_matrix <- vapply(trees, function(t) predict_tree(t, data), numeric(N))
    oob_error <- sum(vapply(1:N, function(r) {
      marjority(predict_matrix[r,][!oob_matrix[r, ]])
    }, numeric(1)) != y) / N

    message("Computing feature importance...")
    importance <- vapply(1:(M - 1), function(j) {
      data[, j] <- sample(data[, j])
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
    list(trees = trees, oob_error = oob_error, importance = importance, proximities = proximities, data = origin_data, outliers = outliers)
  } else if (type == "regression") {
    message("\nComputing r2")
    ypred <- vapply(1:N, function(r) {
      predict_forest(trees[!oob_matrix[r, ]], h(data, r))
    }, numeric(1))
    ypred[is.na(ypred)] <- mean(ypred, na.rm = T)
    r2 <- rsquare(y, ypred)

    message("Computing feature importance...")
    importance <- vapply(1:(M - 1), function(j) {
      data[, j] <- sample(data[, j])
      ypred <- vapply(1:N, function(i) {
        predict_forest(trees[!oob_matrix[i, ]], h(data, i))
      }, numeric(1))
      ypred[is.na(ypred)] <- mean(ypred, na.rm = T)
      r2_perm <- rsquare(y, ypred)
      abs(r2 - r2_perm)
    }, numeric(1))
    names(importance) <- origin_name[1:(M - 1)]
    importance <- as.data.frame(importance[order(-importance)], optional = T)
    message("Done.")
    list(trees = trees, r2 = r2, importance = importance, data = origin_data)
  }
}

predict_forest <- function(rf, data, type = "class", origin = F) {
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
    vapply(1:nrow(data), function(r) predict_tree_inner(tree, h(data, r)), numeric(1))
  }

  origin_data <- rf[["data"]]
  trees <- rf[["trees"]]
  stopifnot(identical(names(origin_data), names(data)))
  if (is_empty(trees)) {
    return(0L)
  }
  N <- nrow(data)
  M <- ncol(data)
  ylevel <- levels(data[, M])
  type_array <- vapply(1:M, function(c) is.factor(data[1, c]) || is.character(data[1, c]), logical(1))
  data <- data.matrix(data)
  ypred <- 0L

  if (type == "class") {
    if (N == 1L) {
      ypred <- marjority(vapply(trees, function(t) predict_tree(t, data), numeric(1)))
    } else {
      predict_matrix <- vapply(trees, function(t) predict_tree(t, data), numeric(N))
      ypred <- vapply(1:N,function(r) marjority(predict_matrix[r,]), numeric(1))
    }
    if (!origin && type_array[M]) {
      ylevel[ypred]
    } else {
      ypred
    }
  } else if (type == "regression") {
    if (N == 1L) {
      ypredm <- ean(vapply(trees, function(t) predict_tree(t, data), numeric(1)))
    } else {
      predict_matrix <- vapply(trees, function(t) predict_tree(t, data), numeric(N))
      ypred <- rowMeans(predict_matrix)
    }
    y <- data[, M]
    r2 <- rsquare(y - ypred)
    message("r2 is ", r2)
    ypred
  }
}
