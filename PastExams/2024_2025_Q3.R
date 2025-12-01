data(rock)
attach(rock)

lm.fit <- lm(perm~., data=rock)
summary(lm.fit)

###############################################################################
# a). Nonparametric bootstrap (B = 100) of full model, no scaling
###############################################################################
set.seed(4060)

B <- 100
n <- nrow(rock)
p <- length(coef(lm.fit))
boot_coef <- matrix(NA_real_, nrow = B, ncol = p)  # NA_real_ = numeric NA
colnames(boot_coef) <- names(coef(lm.fit))

for (b in seq_len(B)) {      # seq_len() safe 1:B
  idx_b  <- sample.int(n, size = n, replace = TRUE) # bootstrap indices
  rock_b <- rock[idx_b, ]
  fit_b  <- lm(perm ~ ., data = rock_b)
  boot_coef[b, ] <- coef(fit_b)
}

# Bootstrap estimates of effects of area and peri on perm (mean bootstrap betas)
boot_beta_area <- mean(boot_coef[, "area"])
boot_beta_peri <- mean(boot_coef[, "peri"])
boot_beta_area
boot_beta_peri



###############################################################################
# b). Bootstrap estimate of bias of coefficient for shape
###############################################################################
beta_shape_hat   <- coef(lm.fit)["shape"] # original estimate
beta_shape_boot  <- boot_coef[, "shape"] # all bootstrap estimates
beta_shape_mean  <- mean(beta_shape_boot)

bias_shape_boot  <- beta_shape_mean - beta_shape_hat
bias_shape_boot
# Bias = mean(bootstrap coefficients) − original coefficient estimate for shape.



###############################################################################
# c). Naive 95% bootstrap CI for the standard error of beta_shape_hat
###############################################################################
# Bootstrap SE of beta_shape_hat = sd of bootstrap coefficients:
se_shape_boot <- sd(beta_shape_boot)

# Approx. SE of this SE estimate (delta method for sd estimator):
se_of_se <- se_shape_boot / sqrt(2 * (B - 1))

# Naive normal-approximation CI for the SE:
se_shape_CI <- se_shape_boot + c(-1, 1) * 1.96 * se_of_se
se_shape_boot
se_shape_CI



###############################################################################
# d). Bootstrap assessment of overfitting via in-bag vs out-of-bag errors
###############################################################################
train_mse <- numeric(B)
oob_mse   <- numeric(B)

for (b in seq_len(B)) {
  idx_b <- sample.int(n, size = n, replace = TRUE)
  in_bag <- sort(unique(idx_b)) # indices used for fitting
  oob <- setdiff(seq_len(n), in_bag) # out-of-bag indices
  
  rock_b <- rock[idx_b, ]
  fit_b <- lm(perm ~ ., data = rock_b)
  
  # Training MSE on bootstrap sample
  train_mse[b] <- mean(residuals(fit_b)^2)
  
  # Out-of-bag MSE on observations not used for fitting
  if (length(oob) > 0) {
    pred_oob    <- predict(fit_b, newdata = rock[oob, ])
    oob_mse[b]  <- mean((rock$perm[oob] - pred_oob)^2)
  } else {
    oob_mse[b]  <- NA_real_  # no OOB points in rare case
  }
}

mean_train_mse <- mean(train_mse, na.rm = TRUE) # na.rm TRUE ignores NAs
mean_oob_mse   <- mean(oob_mse, na.rm = TRUE)
mean_train_mse
mean_oob_mse
# If mean OOB MSE >> mean training MSE, this indicates overfitting (model fits
# bootstrap samples much better than unseen OOB data).

# In this case:
# The out-of-bag MSE (~89,744) is almost twice the bootstrap training MSE (~45,934).
# This large gap indicates the model fits the resampled (in-bag) data much better 
# than it predicts unseen (OOB) observations.
# Therefore, the model shows clear evidence of overfitting.



###############################################################################
# e). Brief comparison of variances of training-error distributions
###############################################################################
# In bootstrap, each model is trained and evaluated on a resample drawn with
# replacement, so training errors are low and very similar across resamples,
# giving a relatively small variance.
# In simple 5-fold cross-validation, models are trained on different 4/5
# subsets and evaluated on held-out data, so the (test) errors vary more from
# fold to fold and their distribution typically has larger variance.