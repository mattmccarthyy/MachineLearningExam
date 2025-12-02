###############################################################################
## Poisson–Gamma aggregate loss: Monte Carlo + analytic comparison
###############################################################################
rm(list = ls())


###############################################################################
# Input parameters (EDIT IN EXAM) 
###############################################################################
set.seed(123)        # Change to whatever he specifies
M      <- 100000     # Number of simulations, change to whatever he specifies
lambda <- 5          # Poisson parameter for claim count N
alpha  <- 2          # Gamma shape
beta   <- 0.1        # Gamma rate (mean severity = alpha / beta)


###############################################################################
# Monte Carlo simulation 
###############################################################################
# Simulate all claim counts at once
N <- rpois(M, lambda = lambda)

# Preallocate aggregate losses
S <- numeric(M)

# Simple robust loop.
for (m in seq_len(M)) {
  n_claims <- N[m]
  if (n_claims > 0) {
    S[m] <- sum(rgamma(n_claims, shape = alpha, rate = beta))
  } else {
    S[m] <- 0
  }
}


###############################################################################
# Empirical (non-parametric) stats 
###############################################################################
mean_S   <- mean(S)
sd_S     <- sd(S)
median_S <- median(S)
skew_S   <- mean((S - mean_S)^3) / (sd_S^3)   # populatin style skewness estimate
# This is completely acceptable and commonly used because:
# M is large (100 000), so the bias correction is negligible
# the purpose is descriptive, not classical inference
# the distribution is highly skewed anyway, so the choice of estimator does not affect interpretation
# If I need sample skewness specifically (doubtful), use:
skew_sample <- sum((S - mean_S)^3) / ((M - 1) * sd_S^3) * (M / (M - 2))


quant_S <- quantile(
  S,
  probs = c(0.5, 0.75, 0.9, 0.95, 0.99, 0.995),
  names = TRUE
)
VaR_995_mc <- as.numeric(quant_S["99.5%"]) # MC 99.5% VaR

## Tail Value-at-Risk / Expected Shortfall at 99.5%
TVaR_995 <- mean(S[S > VaR_995_mc]) # E[S | S > VaR_995_mc], CHECK IF HE WANTS >= or >!!!

## Exceedance probability for a given capital level K
K <- VaR_995_mc   # replace by any specified capital level in the exam, he will specify what to use. 
p_exceed <- mean(S > K)  # P(S > K) estimated from simulations, CHECK IF HE WANTS >= or >!!!


###############################################################################
# Analytic (parametric) moments
###############################################################################
# For X ~ Gamma(alpha, beta) (rate parametrisation):
# E[X] = alpha / beta
# Var(X) = alpha / beta^2
EX   <- alpha / beta
VarX <- alpha / (beta^2)

# For S = sum_{i=1}^N X_i, with N ~ Poisson(lambda):
# E[S] = lambda * E[X]
# Var(S) = lambda * (Var(X) + (E[X])^2)
ES          <- lambda * EX
VarS        <- lambda * (VarX + EX^2)
sd_S_theory <- sqrt(VarS)

## Normal-Approximation 99.5% VaR (parametric approximation)
q995_norm <- ES + qnorm(0.995) * sd_S_theory

# Monte Carlo Error for the Mean (Simulation Uncertainty)
se_mean_S <- sd_S / sqrt(M)


###############################################################################
# Summary Table of all Statistics
###############################################################################
results <- data.frame(
  Metric = c(
    "E[S] (MC)",                   # Monte Carlo estimate of expected aggregate loss
    "E[S] (analytic)",             # Closed-form theoretical mean of Poisson-Gamma aggregate loss
    "SD(S) (MC)",                  # Monte Carlo estimate of the standard deviation (volatility) of aggregate loss
    "SD(S) (analytic)",            # Closed-form theoretical standard deviation of aggregate loss 
    "Skewness (MC)",               # Empirical measure of tail-heaviness/asymmetry of the aggregate loss distribution
    "Median (MC)",                 # Empirical 50th percentile of aggregate losses 
    "VaR_99.5 (MC)",               # Monte Carlo 99.5% Value-at-Risk (high-quantile capital requirement)
    "TVaR_99.5 (MC)",              # Monte Carlo 99.5% Tail Value-at-Risk / Expected Shortfall
    "P(S > K) (MC)",               # Monte Carlo exceedance probability for capital level K
    "VaR_99.5 (Normal approx)",    # Parametric 99.5% VaR using a normal approximation to S
    "SE of E[S] (MC)"              # Monte Carlo standard error of the mean
  ),
  Value = c(
    mean_S,
    ES,
    sd_S,
    sd_S_theory,
    skew_S,
    median_S,
    VaR_995_mc,
    TVaR_995,
    p_exceed,
    q995_norm,
    se_mean_S
  )
)

print(results, digits = 4)


# Histogram with a line at 99.5% VaR.
hist(S, breaks = 50, main = "Aggregate Loss Distribution",
     xlab = "Aggregate Loss", col = "lightgrey", border = "black")
abline(v = quant_S["99.5%"], col = "red", lwd = 2, lty = 2) # Vertical line at 99.5th quantile.


###############################################################################
# INTERPRETATION GUIDE FOR ALL METRICS
###############################################################################
# Mean:
# - Mean >> Median → heavy right tail; rare large losses inflate the average.
# - Mean ≈ Median → distribution roughly symmetric.
# - Mean << Median → left-skew (rare in insurance).

# Median:
# - Median << Mean → extreme losses dominate; typical year far below the mean.
# - Median >> Mean → left-skew; unusual for aggregate losses.
# - Median ≈ Mean → low skewness.

# Standard deviation (σ):
# - σ large => high volatility; large year-to-year uncertainty.
# - σ small => stable portfolio; outcomes cluster tightly.
# - σ > mean => variability dominates expected loss.

# Skewness:
# - Skewness >> 0 => long right tail; severe losses possible.
# - Skewness ≈ 0 => distribution roughly symmetric.
# - Skewness < 0 => left-tail heavy (rare for claim totals).

# VaR 99.5%:
# - VaR >> mean => extreme-loss capital far above expected loss.
# - VaR close to TVaR => moderate tail.
# - VaR far below TVaR => very heavy tail.

# TVaR 99.5%:
# - TVaR >> VaR => extreme tail severe; losses above VaR very large.
# - TVaR slightly above VaR => tail moderately heavy.

# Exceedance probability P(S > K):
# - High P(S > K) => capital K insufficient; high insolvency risk.
# - Low P(S > K) => strong capital buffer.
# - For K = VaR_99.5, P(S > K) ≈ 0.005 => simulation consistent.

# Normal-approx VaR:
# - Normal VaR << MC VaR => normality underestimates tail risk.
# - Normal VaR ≈ MC VaR => tail light or symmetric.

# Monte Carlo SE of mean:
# - SE large => simulation mean unstable; need larger M.
# - SE small => strong precision in mean estimate.
# - SE ∝ 1/sqrt(M) => doubling M reduces SE by ~30%.

