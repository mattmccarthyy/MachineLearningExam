###############################################################################
## ST4060, 2023_2024_Q3  –  Nonlinear regression + bootstrap
###############################################################################
# dat <- read.csv(file = "nonlinear_dataset.csv")
dat <- nonlinear_dataset

x   <- dat$x
y   <- dat$y
n   <- length(y)


###############################################################################
# a). Fit two nonlinear models with nls(), starting values a=0.05, b=0.4, c=2
###############################################################################
# Model (1):  Y_i = a X_i^2 + sin(b + c X_i) + eps_i
fit1 <- nls(
  y ~ a * x^2 + sin(b + c * x),
  start = list(a = 0.05, b = 0.4, c = 2)
)

# Model (2):  Y_i = a X_i^2 + b X_i + c + eps_i
fit2 <- nls(
  y ~ a * x^2 + b * x + c,
  start = list(a = 0.05, b = 0.4, c = 2)
)

coef(fit1) # 0.146885, -1.0709, 2.265
coef(fit2) # 0.1678, -0.2271, 0.5881

# Root Mean Squared Errors (RMSEs)
rmse1 <- sqrt(mean(resid(fit1)^2))
rmse2 <- sqrt(mean(resid(fit2)^2))
rmse1 # 1.8431
rmse2 # 1.7996
# Quote coefficient estimates and RMSEs of both models in the exam.



###############################################################################
# b). Scatterplot of (X,Y) with fitted curves from models (1) and (2)
###############################################################################
plot(x, y,
     pch = 20, col = "black",
     xlab = "x", ylab = "y",
     main = "Nonlinear models fit with nls()")

# Create a fine grid to draw smooth curves
x_grid <- seq(min(x), max(x), length.out = 200)

# Predictions from model (1) and model (2)
yhat1_grid <- predict(fit1, newdata = data.frame(x = x_grid))
yhat2_grid <- predict(fit2, newdata = data.frame(x = x_grid))

lines(x_grid, yhat1_grid, col = "blue", lwd = 2)  # model (1)
lines(x_grid, yhat2_grid, col = "red",  lwd = 2)  # model (2)

legend("topleft",
       legend = c("Data", "Model (1)", "Model (2)"),
       col    = c("black", "blue", "red"),
       pch    = c(20, NA, NA),
       lty    = c(NA, 1, 1),
       lwd    = c(NA, 2, 2),
       bty    = "n")



###############################################################################
# c). Bootstrap model (2) using B = 100 resamples (set.seed(4060) first)
###############################################################################
set.seed(4060)
B <- 100

boot_coef <- matrix(NA_real_, nrow = B, ncol = 3)
colnames(boot_coef) <- c("a", "b", "c")

for (b_idx in 1:B) {
  idx  <- sample.int(n, size = n, replace = TRUE)        # bootstrap indices
  dat_b <- data.frame(x = x[idx], y = y[idx])
  
  fit_b <- nls(
    y ~ a * x^2 + b * x + c,
    data  = dat_b,
    start = list(a = 0.05, b = 0.4, c = 2)
  )
  
  boot_coef[b_idx, ] <- coef(fit_b)
}

# Final bootstrap estimates (means of bootstrap coefficients)
boot_est <- colMeans(boot_coef)
boot_est # 0.1642, -0.1889, 0.5173
# Quote these bootstrap estimates of (a, b, c) in the exam.



###############################################################################
# d). Bootstrap estimate of the standard error of the estimator used in nls()
###############################################################################
boot_se <- apply(boot_coef, 2, sd)
boot_se # a -> 0.2297, b-> 0.2646, c-> 0.7107 
# These are the bootstrap standard error estimates for parameters a, b, c of
# nonlinear regression model (2). They replace the (assumed unavailable)
# theoretical SEs from nls().



###############################################################################
# e). 95% bootstrap confidence intervals for parameters of model (2)
###############################################################################
ci_a <- quantile(boot_coef[, "a"], probs = c(0.025, 0.975))
ci_b <- quantile(boot_coef[, "b"], probs = c(0.025, 0.975))
ci_c <- quantile(boot_coef[, "c"], probs = c(0.025, 0.975))

ci_a # 0.124, 0.2109
ci_b # -0.6863, 0.3030
ci_c # -0.6254, 1.8429
# Using the percentile method, quote these three intervals as the 95% bootstrap
# confidence intervals for the parameters a, b and c of regression model (2).
