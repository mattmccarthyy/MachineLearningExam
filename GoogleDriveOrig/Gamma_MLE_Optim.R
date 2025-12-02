# MLE for Gamma (using optim)
fit_ga <- function(data) {
  nll <- function(p) -sum(dgamma(data, exp(p[1]), exp(p[2]), log = TRUE))
  opt <- optim(c(0, 0), nll, method = "BFGS")
  list(shape = exp(opt$par[1]), rate = exp(opt$par[2]))
}