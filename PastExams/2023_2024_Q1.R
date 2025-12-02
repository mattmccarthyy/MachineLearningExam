###############################################################################
## ST4060, 2023_2024_Q1
###############################################################################
theta_star <- 3          # true mean
sigma      <- 1.5        # true sd
N          <- 30         # sample size per Monte Carlo run
M          <- 1000       # number of Monte Carlo resamples


###############################################################################
# a). 95% CI for Xbar using z = 1.96 and sample SD s
###############################################################################
set.seed(4060)

z_95 <- 1.96
cover_95 <- logical(M)   # TRUE if CI contains theta_star

for (m in 1:M) {
  x     <- rnorm(N, mean = theta_star, sd = sigma)
  xbar  <- mean(x)
  s_hat <- sd(x)
  
  half_width <- z_95 * s_hat / sqrt(N)
  L <- xbar - half_width
  U <- xbar + half_width
  
  cover_95[m] <- (theta_star >= L) && (theta_star <= U)
}

p_hat_95 <- mean(cover_95)
p_hat_95 # 0.934
# Quote p_hat_95 as the Monte Carlo estimate of
#   p = P(theta* ∈ C_95),
# i.e. the proportion of 95% CIs that contain the true mean theta* = 3.


###############################################################################
# b). Repeat computation with 90% CI (z = 1.645)
###############################################################################
set.seed(4060)

z_90 <- 1.645
cover_90 <- logical(M)

for (m in 1:M) {
  x     <- rnorm(N, mean = theta_star, sd = sigma)
  xbar  <- mean(x)
  s_hat <- sd(x)
  
  half_width <- z_90 * s_hat / sqrt(N)
  L <- xbar - half_width
  U <- xbar + half_width
  
  cover_90[m] <- (theta_star >= L) && (theta_star <= U)
}

p_hat_90 <- mean(cover_90)
p_hat_90 # 0.88
# Quote p_hat_90 as the Monte Carlo estimate of coverage probability
# for the 90% CI constructed with z = 1.645.


###############################################################################
# c). Comment on p_hat_95 and p_hat_90, and what they estimate
###############################################################################
# From (a) we obtained p_hat_95 ≈ 0.934. This is reasonably close to 0.95 and
# represents a Monte Carlo estimate of the true coverage probability of the
# nominal 95% CI constructed using z = 1.96, N = 30 and unknown sigma
# estimated by s.
#
# From (b) we obtained p_hat_90 ≈ 0.88, which is close to the nominal 0.90
# and similarly estimates the coverage probability of the corresponding 90% CI
# (z = 1.645). The small discrepancies from 0.95 and 0.90 are due to Monte
# Carlo randomness (finite M = 1000) and the use of the plug-in estimator s
# instead of the true σ in the CI formula.



###############################################################################
# d). Effect of increasing N from 30 to 100 (conceptual, no code needed)
###############################################################################
# Expected behaviour (1–2 sentences for the exam):
# Increasing N to 100 would make each CI narrower because the standard error
# term s / sqrt(N) decreases with N. However, since the CI is constructed in
# the same way (still 95% with z = 1.96), its true coverage probability
# remains about 95%; if anything, p_hat_95 would be closer to 0.95 because
# the sampling distribution of Xbar is more nearly normal and s is a better
# estimator of sigma when N is larger.
