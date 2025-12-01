###############################################################################
## ST4060, 2024_2025_Q1
## Conceptual question – answers given as comments only
###############################################################################


###############################################################################
# (a) Which of sensitivity, RMSE, and AUROC are suitable here, and why?
###############################################################################
# Sensitivity and AUROC are classification metrics and require a binary or
# categorical response, so they are NOT appropriate when Y is continuous.
# RMSE is appropriate, because it measures the average size of prediction
# errors for a continuous outcome and matches the squared–error loss used
# to fit these regression-type models.


###############################################################################
# (b) Based on the graph, which model best fits the data, and why?
###############################################################################
# Model 3 (green curve) appears to give the best fit: it follows the main
# nonlinear trend in the data without chasing every random fluctuation.
# Model 1 (blue line) is too simple and misses curvature, while model 2
# (grey) is overly wiggly and clearly tracks noise, indicating overfitting.


###############################################################################
# (c) Using the SSRs (26.0, 19.4, 23.6) and (b), which model is best, and why?
###############################################################################
# By SSR alone, model 2 (19.4) looks best, followed by model 3 (23.6) and
# then model 1 (26.0), but model 2’s small SSR comes from overfitting noise.
# Model 3 achieves a reasonably low SSR while remaining smooth and plausible,
# so it offers the best balance between goodness-of-fit and generalisation.


###############################################################################
# (d) Could model 3 come from minimising
#     sum_{i=1}^n (Y_i - beta0 - beta1 X_i)^2 + lambda (beta0^2 + beta1^2)?
###############################################################################
# This cost function is a ridge-penalised linear regression in X, which
# always produces a straight line (with shrunk coefficients) as the fitted
# model. Model 3 in the plot is clearly nonlinear in x, so it cannot have
# been obtained from this criterion using only beta0 and beta1.
# To obtain a curve like model 3, the model would need extra basis functions
# (e.g. splines or polynomial terms) in addition to this penalty.


###############################################################################
# (e) Identify B1–B4 as train/test RMSEs for models 2 and 3, and justify.
###############################################################################
# B1 and B2 have much smaller RMSEs than B3 and B4, so B1 and B2 must be
# training RMSEs, and B3 and B4 must be test RMSEs. Among the training
# boxplots, B2 has the lowest centre and smallest spread, consistent with
# the highly flexible, overfitting model 2 achieving the lowest training
# error, so B2 = model 2 train and B1 = model 3 train.
# Among the test boxplots, B3 has slightly higher and more variable RMSEs
# than B4, so B3 corresponds to model 2 test and B4 to model 3 test.
