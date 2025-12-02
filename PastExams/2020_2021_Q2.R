###############################################################################
## ST4060, 2020_2021_Q2
###############################################################################
# install.packages("ISLR")
library(ISLR)

dat <- na.omit(Hitters)
x   <- dat[, c("Years", "Hits", "Walks")]
y   <- log(dat$Salary)
n   <- nrow(x)


###############################################################################
# Helper: function to fit linear model and return test MSE on a given split
###############################################################################
test_mse_split <- function(train_idx, test_idx) {
  dat_train <- data.frame(y = y[train_idx], x[train_idx, , drop = FALSE])
  dat_test  <- data.frame(y = y[test_idx],  x[test_idx,  , drop = FALSE])
  
  fit <- lm(y ~ ., data = dat_train)
  yhat_test <- predict(fit, newdata = dat_test)
  
  mean((dat_test$y - yhat_test)^2)   # test MSE
}


###############################################################################
# a). 5-fold cross-validation: prediction MSE estimate
###############################################################################
set.seed(4060)

K <- 5
fold_id <- sample(rep(1:K, length.out = n))   # random, unstratified 5-folds

mse_5fold <- numeric(K)

for (k in 1:K) {
  test_idx  <- which(fold_id == k)
  train_idx <- setdiff(1:n, test_idx)
  mse_5fold[k] <- test_mse_split(train_idx, test_idx)
}

pred_mse_5fold <- mean(mse_5fold)
mse_5fold
pred_mse_5fold # 0.422
# Quote pred_mse_5fold as the 5-fold CV prediction MSE estimate for the
# multivariate linear regression of log-salary on Years, Hits, Walks.


###############################################################################
# b). Leave-one-out cross-validation (LOO-CV): prediction MSE estimate
###############################################################################
set.seed(4060)   # as requested, although LOO splits are deterministic

# For LOO, each test set contains one observation; its MSE is just squared error.
loo_mse <- numeric(n)

for (i in 1:n) {
  test_idx  <- i
  train_idx <- setdiff(1:n, i)
  loo_mse[i] <- test_mse_split(train_idx, test_idx)
}

pred_mse_loo <- mean(loo_mse)
head(loo_mse)
pred_mse_loo # 0.41
# Quote pred_mse_loo as the LOO-CV prediction MSE estimate for the same model.


###############################################################################
# c). Single figure with boxplots of test-set MSEs from the two frameworks
###############################################################################
boxplot(
  list(
    "5-fold CV" = mse_5fold,
    "LOO CV"    = loo_mse
  ),
  ylab = "Test-set MSE",
  main = "Comparison of 5-fold CV and LOO-CV test-set MSEs",
  ylim = c(0, 1.5)
)

###############################################################################
# d). Comment on the boxplots (for the exam; write in words)
###############################################################################
# Typical comment based on theory and expected output:
# - The medians of the two boxplots should be similar, since both aim to
#   estimate the same prediction error; thus pred_mse_5fold and pred_mse_loo
#   are usually close.
# - The LOO-CV boxplot usually shows much larger spread and more extreme
#   values because each test set is a single observation, so its MSE
#   (squared error) is highly variable.
# - The 5-fold CV boxplot is more concentrated: each test MSE is averaged
#   over about n/5 observations, giving a more stable estimate per fold.
# - Therefore, the results conform to expectations: LOO-CV has low bias but
#   high variance, while 5-fold CV trades a small bias for lower variance in
#   test-set MSEs.

# IN THIS CASE:
# The two boxplots have broadly similar centres: the median test MSE from 5-fold
# CV (~0.42) is of the same order as the median from LOO-CV, so both methods are
# estimating essentially the same prediction error. However, the LOO-CV boxplot
# shows a much larger spread and many high outliers, whereas the 5-fold CV
# MSEs are tightly clustered. This matches theory: in LOO-CV each test set
# consists of a single observation, so its squared error (and hence test-set
# MSE) is highly variable, especially for leverage points, giving high-variance
# estimates. In 5-fold CV each test MSE is averaged over about n/5 points,
# which stabilises the errors (lower variance) at the cost of a small increase
# in bias. Overall, the boxplots conform to these expectations.
