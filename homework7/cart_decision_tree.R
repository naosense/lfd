source("common.R")
cart_decision_tree <- function(data, type = "class", node_size = 1) {
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
    if (is_empty(rows)) {
      return(0L)
    }

    ncol <- ncol(data)

    X <- data[rows, 1:(ncol - 1), drop = F]
    y <- data[rows, ncol, drop = F]

    if ((nrow(y) <= node_size || is_same(y) || is_same(X))) {
      return(ifelse(type == "class", marjority(y), mean(y)))
    } else {
      min_res <- list(impufity = Inf, ind = 0L, sp = 0L, left = NULL, right = NULL)

      for (j in 1:(ncol - 1)) {
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
  origin_name <- colnames(data)
  data_levels <- lapply(1:ncol(data), function(i) levels(data[, i]))
  type_array <- vapply(1:ncol(data), function(c) is.factor(data[1, c]) || is.character(data[1, c]), logical(1))
  data <- unique(data.matrix(data))
  structure(split_branch(1:nrow(data)), origin_name = origin_name, data_levels = data_levels, type_array = type_array)
}

predict_tree <- function(tree, data, origin = F) {
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

  origin_name <- attr(tree, "origin_name")
  stopifnot(identical(origin_name, names(data)))
  type_array <- attr(tree, "type_array")
  ylevel <- attr(tree, "data_levels")[[ncol(data)]]
  data <- data.matrix(data)
  yint <- 0L
  if (nrow(data) == 1L) {
    yint <- predict_tree_inner(tree, data)
  } else {
    yint <- vapply(1:nrow(data), function(r) predict_tree_inner(tree, h(data, r)), numeric(1))
  }
  if (!origin && type_array[length(type_array)]) {
    ylevel[yint]
  } else {
    yint
  }
}

print_tree <- function(tree, prefix = "    ") {
  print_inner <- function(tree, prefix, step) {
    if (is.atomic(tree)) {
      cat(ifelse(type_array[length(type_array)], ylevel[tree], tree))
    } else {
      cat(origin_name[tree[["ind"]]],
          ifelse(type_array[tree[["ind"]]],
                 paste0("=", data_levels[[tree[["ind"]]]][tree[["sp"]]]),
                 paste0("<", tree[["sp"]])),
          ":\n", sep = "")
      cat(prefix, "[L]", sep = "")
      print_inner(tree[["left"]], paste0(prefix, step), step)
      cat("\n")
      cat(prefix, "[R]", sep = "")
      print_inner(tree[["right"]], paste0(prefix, step), step)
    }
  }
  origin_name <- attr(tree, "origin_name")
  data_levels <- attr(tree, "data_levels")
  type_array <- attr(tree, "type_array")
  ylevel <- data_levels[[length(data_levels)]]
  print_inner(tree, prefix, prefix)
}