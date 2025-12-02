###############################################################################
## ST4060, 2023_2024_Q2
###############################################################################
library(MASS)

x <- Animals # 28 land animals
brain <- x$brain # brain weight
body  <- x$body # body weight
n <- length(body)

set.seed(4060) # as requested
B <- 1000 # number of bootstrap resamples


###############################################################################
# Bootstrap setup: resample animals (rows) with replacement B times
###############################################################################
boot_mean_brain <- numeric(B)
boot_mean_body  <- numeric(B)
boot_mean_ratio <- numeric(B) # ratio = brain/body per animal, then mean

for (b in 1:B) {
  idx_b <- sample.int(n, size = n, replace = TRUE)  # bootstrap indices
  
  brain_b <- brain[idx_b]
  body_b  <- body[idx_b]
  
  boot_mean_brain[b] <- mean(brain_b)
  boot_mean_body[b]  <- mean(body_b)
  boot_mean_ratio[b] <- mean(brain_b / body_b)
}



###############################################################################
# a). Bootstrap estimates of mean brain and mean body weight
###############################################################################
# Bootstrap estimate = mean of bootstrap replicates for each parameter
boot_est_mean_brain <- mean(boot_mean_brain)
boot_est_mean_body <- mean(boot_mean_body)

boot_est_mean_brain # 568.15
boot_est_mean_body # 4,241.75
# Quote these two values as the bootstrap estimates of:
# - mean brain weight of all land animals,
# - mean body weight of all land animals.



###############################################################################
# b). Bootstrap estimate of mean ratio brain/body
###############################################################################
boot_est_mean_ratio <- mean(boot_mean_ratio)
boot_est_mean_ratio
# Quote this value as the bootstrap estimate of the mean ratio brain/body
# over the 28 species (ratio for each species is brain/body, then averaged).
# In this case: 6.222


###############################################################################
# c). Bootstrap estimate of bias of the sample mean body weight
###############################################################################
sample_mean_body <- mean(body)
sample_mean_body

bias_hat_body <- boot_est_mean_body - sample_mean_body
bias_hat_body # -36.69
# Bias estimate = E_boot(mean_body*) − mean_body_sample.
# Quote bias_hat_body as the bootstrap estimate of the bias of the sample
# mean body weight for these 28 species.


###############################################################################
# d). 95% naive (quantile) bootstrap CI for mean body weight
###############################################################################
ci_body_95 <- quantile(boot_mean_body, probs = c(0.025, 0.975))
ci_body_95
# Quote this (lower, upper) pair as the 95% naive bootstrap confidence
# interval for the mean body weight of these land animals.
# [419.44, 10,920.69].


###############################################################################
# e). Why is the bootstrap CI for mean body weight so wide?
###############################################################################
summary(body)
sd(body)
hist(body, breaks = 10,
     main = "Histogram of body weights (Animals data)",
     xlab = "Body weight")

# Answer for the exam (2 sentences, excluding sample size):
# The body weights are extremely skewed with a very heavy right tail:
# summary(body) shows max = 87000 while most animals are below 10000,
# and sd(body) ≈ 18060 is several times larger than the mean (≈ 4278).
# These extreme outliers / heavy tail greatly increase the variance of the
# sample mean, so the bootstrap replicates of the mean are very spread out,
# which leads to a very wide bootstrap confidence interval.
