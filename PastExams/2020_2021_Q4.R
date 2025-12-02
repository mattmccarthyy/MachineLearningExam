###############################################################################
## ST4060, 2023_2024_Q4
###############################################################################
# dat <- read.csv("insdata.csv")

dat <- insdata

age <- dat$Age      # age of policyholder
mF  <- dat$mF       # (simulated) female mortality rate
n   <- length(age)


###############################################################################
# a). First P-spline with smoothing control parameter = 0.5
###############################################################################
# Use smooth.spline as P-spline (spar = smoothing control parameter)
fit_ps1 <- smooth.spline(x = age, y = mF, spar = 0.5)

# Plot data + first P-spline
plot(age, mF,
     pch = 20, col = "black",
     xlab = "Age", ylab = "mF",
     main = "P-splines of mortality vs age")
lines(fit_ps1$x, fit_ps1$y, col = "red", lwd = 2)   # red solid curve for P-spline 1



###############################################################################
# b). Second P-spline with smoothing parameter = 0.25 (half of 0.5)
###############################################################################
fit_ps2 <- smooth.spline(x = age, y = mF, spar = 0.25)
lines(fit_ps2$x, fit_ps2$y, col = "blue", lwd = 2)  # blue solid curve for P-spline 2
legend("topleft",
       legend = c("Data", "P-spline (spar=0.5)", "P-spline (spar=0.25)"),
       col    = c("black", "red", "blue"),
       pch    = c(20, NA, NA),
       lty    = c(NA, 1, 1),
       lwd    = c(NA, 2, 2),
       bty    = "n")



###############################################################################
# c). Show that both P-splines are evaluated at the same x-points
###############################################################################
identical(fit_ps1$x, fit_ps2$x)
# The object components fit_ps1$x and fit_ps2$x contain the x-grid at which the
# smoothed curves are evaluated. The call above returns TRUE, so both P-splines
# are evaluated at the same set of age points on the x-axis.


###############################################################################
# d). Compute and compare MSEs of the two P-splines
###############################################################################
# Predictions at the observed ages
pred_ps1 <- predict(fit_ps1, x = age)$y
pred_ps2 <- predict(fit_ps2, x = age)$y

mse_ps1 <- mean((mF - pred_ps1)^2)
mse_ps2 <- mean((mF - pred_ps2)^2)
mse_ps1
mse_ps2
# In the exam:
# - Report the two MSE values.
# - Typically the less smoothed spline (spar = 0.25) has smaller training MSE,
#   because it is more flexible and tracks the data more closely.
# - The more smoothed spline (spar = 0.5) usually has larger MSE but may
#   generalise better by avoiding overfitting.

# In this case: mse_ps1 = 0.00032, mse_ps2 = 0.000062, as expected from above.



###############################################################################
# e). B-spline basis using first, second, third quartiles of age as knots
###############################################################################
library(splines)

age_knots <- quantile(age, probs = c(0.25, 0.5, 0.75))
age_knots

bs_basis <- bs(age,
               knots          = age_knots,
               degree         = 3,
               Boundary.knots = range(age))
dim(bs_basis)   # n rows x K basis functions

# Plot B-spline basis functions
ord <- order(age)
matplot(age[ord], bs_basis[ord, ],
        type = "l", lty = 1,
        xlab = "Age", ylab = "Basis value",
        main = "Cubic B-spline basis (knots at age quartiles)")
abline(v = age_knots, lty = 2, col = "grey")



###############################################################################
# f). Coordinates of a policyholder aged 60 on this B-spline basis
###############################################################################
basis_at_60 <- predict(bs_basis, 60)  # vector of basis values at age 60
basis_at_60 # 0.1713, 0.5619, 0.2659
# Quote these values (to 4 d.p.) as the coordinates of age 60 in the B-spline
# basis. To mark them on the plot from (e), add vertical line at x = 60:
abline(v = 60, col = "black", lty = 3)



###############################################################################
# g). Corresponding B-spline fit for (age, mF); output the coefficients
###############################################################################
fit_bs <- lm(mF ~ bs_basis)
coef_bs <- coef(fit_bs)
coef_bs
# 0.002, 0.007, -0.0084, 0.02019, 0.0196, 0.1455, 0.1338 
# In the exam: quote these regression coefficients as the coefficients of
# the B-spline representation for mF as a function of age.



###############################################################################
# h). Compare MSE of this B-spline with MSEs of the two P-splines
###############################################################################
pred_bs <- fitted(fit_bs)                  # predictions at observed ages
mse_bs  <- mean((mF - pred_bs)^2)
mse_bs # 0.00038, between th other 2, but closer to mse_ps2
# Compare mse_bs with mse_ps1 and mse_ps2 computed in (d):
# - If mse_bs is close to mse_ps2, the B-spline with quartile knots has
#   similar flexibility to the less smoothed P-spline.
# - Comment whether the B-spline under- or over-smooths relative to the
#   P-splines and suggest which gives the best trade-off between fit and
#   smoothness based on these MSEs and the plots.

# The B-spline MSE is 0.0003823, which lies between the two P-spline MSEs but
# is much closer to the smoother P-spline (spar = 0.5, MSE = 0.0003181). This is
# expected because the B-spline uses a fixed set of knots and no smoothing
# penalty, giving a moderately smooth fit. In contrast, the very flexible
# P-spline (spar = 0.25, MSE = 0.0000623) tracks the data much more closely,
# producing the lowest MSE. Thus, the B-spline behaves similarly to the more
# smoothed P-spline and achieves an intermediate level of fit.



###############################################################################
# i). Interpolations over age range: P-spline vs local polynomial regression
###############################################################################
age_grid <- seq(min(age), max(age), by = 1)

# P-spline interpolation (use P-spline from part (a), spar = 0.5)
interp_ps <- predict(fit_ps1, x = age_grid)$y

# Local polynomial regression (e.g. LOESS)
fit_loess <- loess(mF ~ age, span = 0.75)   # span can be adjusted
interp_loess <- predict(fit_loess, newdata = data.frame(age = age_grid))

# Plot interpolated points over observations
plot(age, mF,
     pch = 20, col = "black",
     xlab = "Age", ylab = "mF",
     main = "Interpolations: P-spline vs Local Polynomial")
points(age_grid, interp_ps,    col = "red",  pch = 20)
points(age_grid, interp_loess, col = "blue", pch = 20)
legend("topleft",
       legend = c("Data", "P-spline interp", "Local poly interp"),
       col    = c("black", "red", "blue"),
       pch    = c(20, 20, 20),
       bty    = "n")



###############################################################################
# j). Standard deviations of the interpolated samples
###############################################################################
sd_interp_ps    <- sd(interp_ps,    na.rm = TRUE)
sd_interp_loess <- sd(interp_loess, na.rm = TRUE)

sd_interp_ps # 0.0415
sd_interp_loess # 0.0412
# Quote these two standard deviations as measures of the variability of the
# interpolated mortality curves obtained from P-spline smoothing and from
# local polynomial regression, respectively.
