###############################################################################
## ST4060, 2024_2025_Q2
###############################################################################
data(rock)

# Univariate setup
x <- rock$peri # predictor
y <- rock$perm # response
n <- length(y)


###############################################################################
# a). Fit two smoothing splines (spar = 0.8 and 0.95) to full data
###############################################################################
fit_spar_08  <- smooth.spline(x, y, spar = 0.8)
fit_spar_095 <- smooth.spline(x, y, spar = 0.95)

# Fitted values on training data
yhat_08  <- predict(fit_spar_08,  x)$y
yhat_095 <- predict(fit_spar_095, x)$y

# RMSEs for both fits
rmse_08  <- sqrt(mean((y - yhat_08 )^2))
rmse_095 <- sqrt(mean((y - yhat_095)^2))
rmse_08
rmse_095
# Quote these two RMSE values in the exam for part (a).


###############################################################################
# b). Simple (not repeated, not stratified) 10-fold CV for spline (spar = 0.95)
###############################################################################
set.seed(4060) # reproducible folds

K <- 10
fold_id_spline <- sample(rep(1:K, length.out = n)) # random, unstratified folds

train_rmse_spline <- numeric(K)
test_rmse_spline  <- numeric(K)

for (k in 1:K) {
  test_idx  <- which(fold_id_spline == k)
  train_idx <- setdiff(1:n, test_idx)
  
  fit_k <- smooth.spline(x[train_idx], y[train_idx], spar = 0.95)
  
  # Training predictions / RMSE on fold k
  yhat_train <- predict(fit_k, x[train_idx])$y
  train_rmse_spline[k] <- sqrt(mean((y[train_idx] - yhat_train)^2))
  
  # Test predictions / RMSE on held-out fold k
  yhat_test <- predict(fit_k, x[test_idx])$y
  test_rmse_spline[k]  <- sqrt(mean((y[test_idx] - yhat_test )^2))
}

mean_train_rmse_spline <- mean(train_rmse_spline)
mean_test_rmse_spline  <- mean(test_rmse_spline)
mean_train_rmse_spline
mean_test_rmse_spline
# Report these mean training and test RMSEs for the spline model (spar = 0.95).


###############################################################################
# c). Simple 10-fold CV for linear regression model with intercept
###############################################################################
set.seed(4060) # new random 10-fold split

K <- 10
fold_id_lm <- sample(rep(1:K, length.out = n))

train_rmse_lm <- numeric(K)
test_rmse_lm  <- numeric(K)

for (k in 1:K) {
  test_idx  <- which(fold_id_lm == k)
  train_idx <- setdiff(1:n, test_idx)
  
  # Build explicit training data frame
  df_train <- data.frame(peri = x[train_idx],
                         perm = y[train_idx])
  
  fit_k <- lm(perm ~ peri, data = df_train)
  
  # Training RMSE on fold k
  yhat_train <- fitted(fit_k)
  train_rmse_lm[k] <- sqrt(mean((df_train$perm - yhat_train)^2))
  
  # Test RMSE on held-out fold k
  df_test  <- data.frame(peri = x[test_idx])
  yhat_test <- predict(fit_k, newdata = df_test)
  test_rmse_lm[k] <- sqrt(mean((y[test_idx] - yhat_test)^2))
}

mean_train_rmse_lm <- mean(train_rmse_lm)
mean_test_rmse_lm  <- mean(test_rmse_lm)
mean_train_rmse_lm
mean_test_rmse_lm


###############################################################################
# d). Standard deviation of training and test RMSEs (linear model CV)
###############################################################################
sd_train_rmse_lm <- sd(train_rmse_lm)
sd_test_rmse_lm  <- sd(test_rmse_lm)
sd_train_rmse_lm
sd_test_rmse_lm
# Typically sd(test RMSE) > sd(train RMSE) because each fold’s test set is
# smaller and consists of different held-out points, so test errors fluctuate
# more across folds, while training errors are averaged over larger samples and
# are therefore more stable.


###############################################################################
# e). Effect of changing from simple 10-fold CV to repeated 10-fold CV
###############################################################################
# Repeated 10-fold CV would generate several independent 10-fold partitions and
# average RMSEs across all repetitions. This reduces the variance of the CV
# estimates (training and especially test RMSE), giving a more stable and
# reliable assessment of model performance at the cost of extra computation.