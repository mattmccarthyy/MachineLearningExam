##############################################################################
# K-fold CV for K = 5 and K = 10 to see stability of an estimator
##############################################################################
###############################################################################
# 5-fold CV
###############################################################################
set.seed(4060)
K <- 5
fold_id <- sample(rep(1:K, length.out = n))
mse_5 <- numeric(K)

for (k in 1:K) {
  test_idx  <- which(fold_id == k)
  train_idx <- setdiff(1:n, test_idx)
  fit <- lm(y ~ ., data = data.frame(y=y[train_idx], x[train_idx,]))
  pred <- predict(fit, newdata = data.frame(y=y[test_idx], x[test_idx,]))
  mse_5[k] <- mean((y[test_idx] - pred)^2)
}

pred_mse_5 <- mean(mse_5)

###############################################################################
# 10-fold CV
###############################################################################

set.seed(4060)
K <- 10
fold_id <- sample(rep(1:K, length.out = n))
mse_10 <- numeric(K)

for (k in 1:K) {
  test_idx  <- which(fold_id == k)
  train_idx <- setdiff(1:n, test_idx)
  fit <- lm(y ~ ., data = data.frame(y=y[train_idx], x[train_idx,]))
  pred <- predict(fit, newdata = data.frame(y=y[test_idx], x[test_idx,]))
  mse_10[k] <- mean((y[test_idx] - pred)^2)
}

pred_mse_10 <- mean(mse_10)



###############################################################################
# Boxplot to compare stability
###############################################################################
boxplot(
  list(
    "5-fold CV"  = mse_5,
    "10-fold CV" = mse_10
  ),
  ylab = "Test-set MSE",
  main = "Comparison of 5-fold vs 10-fold CV"
)



###############################################################################
# Interpretation of 5-fold vs 10-fold CV stability (exam-style answer)
###############################################################################
# The boxplots compare the distribution of test-set MSEs from 5-fold and
# 10-fold cross-validation.
#
# Centre:
# - The medians are very similar, so both procedures estimate roughly the same
#   underlying prediction error for the model.
#
# Spread / stability:
# - The 5-fold CV MSEs show a relatively narrow IQR and short whiskers, with
#   only one moderate outlier. This indicates low variability across folds,
#   i.e. a *more stable* estimator of prediction error.
# - The 10-fold CV MSEs have a wider IQR, longer whiskers and a much larger
#   outlier (~1.2). This indicates higher variability across folds, i.e. a
#   *less stable* estimator of prediction error.
#
# Reason:
# - In 5-fold CV each test set contains about 20% of the data, so each test MSE
#   is an average over many observations and therefore has lower variance.
# - In 10-fold CV each test set contains only about 10% of the data, so each
#   test MSE is more sensitive to individual observations and hence more
#   variable.
#
# Conclusion:
# - Both methods give similar average prediction error, but the 5-fold CV
#   estimator is more stable (lower variance of test MSEs) than the 10-fold CV
#   estimator.
