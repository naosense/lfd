cols <- c("region","zone","meters","direction", "con", "floor",  "year","school","subway","tax","num","price","area")

# samples <- seq(100, 1000, by = 100)
# oob_errors <- rep(0, length(samples))

# for (i in seq_along(samples)) {
#   forest <- random_forest(houses[sample(1:29790, samples[i]), cols])
#   oob_errors[i] <- forest$oob_error
# }
# plot(samples, oob_errors, type = "b",
#      main = "OOB Error with different samples", xlab = "Sample size", ylab = "OOB Error")

# features <- 1:12
# oob_errors <- rep(0, length(features))
# for (i in seq_along(features)) {
#   forest <- random_forest(houses[sample(1:29790, 1000), cols], feature_count = features[i])
#   oob_errors[i] <- forest$oob_error
# }
#
# plot(features, oob_errors, type = "b",
#      main = "OOB Error with different feature count", xlab = "Feature count", ylab = "OOB Error")


tss <- seq(10, 100, by = 10)
oob_errors <- rep(0, length(tss))
for (i in seq_along(tss)) {
  forest <- random_forest(houses[sample(1:29790, 1000), cols], ts = tss[i], feature_count = 12)
  oob_errors[i] <- forest$oob_error
}

plot(tss, oob_errors, type = "b",
     main = "OOB Error with different tree size", xlab = "Tree size", ylab = "OOB Error")

