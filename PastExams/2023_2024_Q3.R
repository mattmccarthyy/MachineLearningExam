###############################################################################
## ST4060, 2023_2024_Q3
###############################################################################
library(splines) # contains bs()
library(MASS) # contains Boston

x <- Boston$nox # predictor
y <- Boston$medv# response
n <- length(y)

set.seed(4060) # as requested (affects CV / random splits later)


###############################################################################
# a). Fit a B-spline with knots at quantiles (0.15, 0.40, 0.60, 0.70, 0.85)
###############################################################################
knots_vec <- quantile(x, probs = c(0.15, 0.40, 0.60, 0.70, 0.85))

# Fit B-spline via linear model; bs() builds spline basis in the formula
dat <- data.frame(x = x, y = y)
fit_bs <- lm(y ~ bs(x, knots = knots_vec,
                    degree = 3, # cubic spline, they are smooth (cts first and second deriv.), they flex enough to capture curvature, they do not overfit as easily as higher degree splines. 
                    Boundary.knots = range(x)),
             data = dat)

round(coef(fit_bs), 4)
# 23.6771, 7.7280, -1.6411, 8.6786, -13.1903, 11.1449, -34.8569, 19.2098, -7.2492



###############################################################################
# b). Predictions from the B-spline at newx = c(0.4, 0.5, 0.6)
###############################################################################
newx <- data.frame(x = c(0.4, 0.5, 0.6))
yhat_bs_new <- predict(fit_bs, newdata = newx)
yhat_bs_new
# 27.8, 26.5 and 24



###############################################################################
# c). Fit a P-spline (smoothing spline) with LOO-CV choice of smoothing
###############################################################################
# Ordinary leave-one-out CV is requested -> use cv = TRUE
fit_ps <- smooth.spline(x, y, cv = TRUE)

# (c)(i) Penalized criterion (RSS–type) for P-spline
fit_ps$pen.crit
# Quote fit_ps$pen.crit as the P-spline penalised criterion (RSS) => # 20,523.37 in this case

# (c)(ii) Plot: data (black), B-spline (red), P-spline (blue)
x_grid <- seq(min(x), max(x), length.out = 200)

yhat_bs_grid <- predict(fit_bs, newdata = data.frame(x = x_grid))
yhat_ps_grid <- predict(fit_ps, x = x_grid)$y  # smooth.spline predict

plot(x, y,
     pch = 20, col = "black",
     xlab = "nox", ylab = "medv",
     main = "Boston: B-spline (red) vs P-spline (blue)")
lines(x_grid, yhat_bs_grid, col = "red",  lwd = 2)
lines(x_grid, yhat_ps_grid, col = "blue", lwd = 2)
legend("topright",
       legend = c("Data", "B-spline", "P-spline"),
       col    = c("black", "red", "blue"),
       pch    = c(20, NA, NA),
       lty    = c(NA, 1, 1),
       lwd    = c(NA, 2, 2),
       bty    = "n")



###############################################################################
# d). Predictions from the P-spline at newx = c(0.4, 0.5, 0.6)
###############################################################################
yhat_ps_new <- predict(fit_ps, x = newx$x)$y
yhat_ps_new
# Compare yhat_ps_new to yhat_bs_new:
# - B-spline predictions come from a fixed number/position of knots.
# - P-spline predictions use a smoothing penalty chosen by LOO-CV.
# Any differences reflect different smoothness: P-spline is typically smoother
# and may shrink extreme wiggles compared to the B-spline, giving slightly
# different fitted values at the same x.

# In this specific case
# The P-spline predictions are:
#   0.4 -> 27.78
#   0.5 -> 24.65
#   0.6 -> 21.78
#
# The B-spline predictions are:
#   0.4 -> 27.82
#   0.5 -> 26.53
#   0.6 -> 24.02
#
# The values differ because:
# - The B-spline fit uses a fixed set of knots and no smoothness penalty,
#   so it can adapt more locally to the data (more wiggly).
# - The P-spline uses a smoothing penalty chosen by LOO-CV, producing a 
#   smoother curve that shrinks rapid changes and avoids overfitting.
#
# As a result, the P-spline predictions are smoother and generally lower
# at x = 0.5 and x = 0.6 compared to the B-spline, reflecting the additional
# smoothing introduced by the penalty.


###############################################################################
# e). 5-fold cross-validation of the P-spline – prediction RMSE
###############################################################################
set.seed(4060)   # ensure reproducible fold assignment

K <- 5
fold_id <- sample(rep(1:K, length.out = n))  # random, unstratified 5-folds

cv_rmse_ps <- numeric(K)

for (k in 1:K) {
  test_idx  <- which(fold_id == k)
  train_idx <- setdiff(1:n, test_idx)
  
  # Fit P-spline on training data ONLY, using LOO-CV on that subset
  fit_k <- smooth.spline(x[train_idx], y[train_idx], cv = TRUE)
  
  # Predict on held-out test data and compute RMSE for this fold
  yhat_test <- predict(fit_k, x = x[test_idx])$y
  cv_rmse_ps[k] <- sqrt(mean((y[test_idx] - yhat_test)^2))
}

pred_rmse_ps <- mean(cv_rmse_ps)
pred_rmse_ps
# 8.06
