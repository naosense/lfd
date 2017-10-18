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