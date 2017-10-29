source("common.R")
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

    X <- v(data, 1:(cols - 1))
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

      for (j in 1:(cols - 1)) {
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
  origin_name <- colnames(data)
  data_levels <- lapply(1:ncol(data), function(i) levels(data[, i]))
  type_array <- vapply(1:ncol(data), function(c) is.factor(data[1, c]) || is.character(data[1, c]), logical(1))
  data <- data.matrix(data)
  structure(split_branch(data), origin_name = origin_name, data_levels = data_levels, type_array = type_array)
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
  if (type_array[length(type_array)]) {
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