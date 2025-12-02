################################################################################
## Example script: using backward, forward, and stepwise selection for GLMs
################################################################################
set.seed(123)

# Example data
n <- 200
dat <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)

## True linear predictor includes an interaction term x1:x2
eta <- 0.5 + 0.8 * dat$x1 - 0.7 * dat$x2 + 0.4 * dat$x1 * dat$x2 + 0.2 * dat$x3
lambda <- exp(eta)

dat$y <- rpois(n, lambda)


################################################################################
## 1. Base models: null vs full -----------------------------------------------
################################################################################
## Null (intercept-only) GLM: used for forward / stepwise starts
null_mod <- glm(y ~ 1, family = poisson, data = dat)

## Full GLM: includes main effects and interaction
## Note: x1 * x2 expands to x1 + x2 + x1:x2
full_mod <- glm(y ~ x1 * x2 + x3, family = poisson, data = dat)

## We will work with these in step() below.

################################################################################
## 2. BACKWARD ELIMINATION using step() ---------------------------------------
################################################################################
## step(model, direction, scope, trace, k, ...)
##
## - model: starting fitted model object (here: the full model).
## - direction = "backward": only consider removing terms.
## - scope:
##     * Optional for pure backward from full model; by default, step() will
##       only consider models nested inside the starting model.
##     * If supplied, can explicitly specify which terms are allowed to drop.
## - trace:
##     * 0  = silent (no printed output during search).
##     * 1+ = more verbose; step-by-step logs of AIC and chosen moves.
## - k:
##     * penalty term in AIC (default k = 2 => standard AIC).
##     * using k = log(n) corresponds to BIC.

backward_mod <- step(
  object    = full_mod,         # start from full model
  direction = "backward",       # only drop terms
  trace     = 0                 # no on-screen output
)

summary(backward_mod) # None are removed here because all significant, but still works.
## If you want to see each step:
## backward_mod_verbose <- step(full_mod, direction = "backward", trace = 1)

################################################################################
## 3. FORWARD SELECTION using step() ------------------------------------------
################################################################################
## For forward selection we typically start from the null model and define
## the "scope" of terms that may be added.
##
## scope defines the set of candidate terms:
## - It can be:
##     * A formula, e.g., scope = ~ x1 + x2 + x3 + x1:x2
##     * OR a list with "lower" and "upper" models:
##         scope = list(lower = ~ 1, upper = ~ x1 * x2 + x3)
##
## Here we use the lower/upper style because it’s very clear.

forward_scope <- list(
  lower = ~ 1,              # null model (intercept only)
  upper = ~ x1 * x2 + x3    # full candidate model space
)

forward_mod <- step(
  object    = null_mod,        # start from null
  scope     = forward_scope,   # which terms can be added
  direction = "forward",       # only add terms
  trace     = 0
)

summary(forward_mod)

## Again, to see the selection path:
## forward_mod_verbose <- step(null_mod, scope = forward_scope,
##                             direction = "forward", trace = 1)


################################################################################
## 4. BI-DIRECTIONAL STEPWISE SELECTION ("both") ------------------------------
################################################################################
## Bi-directional stepwise allows both adding and dropping terms.
## You must define scope carefully:
## - The starting model is usually null_mod (or a small base model).
## - scope specifies:
##   * lower = the smallest allowed model.
##   * upper = the largest allowed model.
##
## step() will:
## - try adding terms not yet in the model (within upper).
## - try removing terms already in the model (but not going below lower).

stepwise_scope <- list(
  lower = ~ 1,
  upper = ~ x1 * x2 + x3
)

stepwise_mod <- step(
  object    = null_mod,         # start small
  scope     = stepwise_scope,
  direction = "both",           # add and drop terms
  trace     = 0
)

summary(stepwise_mod)

## To see details:
## stepwise_mod_verbose <- step(null_mod, scope = stepwise_scope,
##                              direction = "both", trace = 1)


################################################################################
## 5. Notes on INTERACTIONS vs MAIN EFFECTS (hierarchy) -----------------------
################################################################################
## IMPORTANT: R’s step() does NOT automatically enforce model hierarchy.
## Example risk:
## - You allow models up to y ~ x1 * x2 + x3.
## - step() might:
##   - Drop x1 but keep x1:x2 (or likewise for x2).
##   - This creates a model with an interaction term but missing one or both
##     corresponding main effects.
##   - This is usually considered bad practice (violates hierarchical principle).
##
## To avoid this, you have to control scope and/or "keep" terms manually.

## 5.1. Example of enforcing main effects when interactions are present
##
## Strategy: define "lower" such that main effects must always be present
## when an interaction is allowed, or use "keep" to always retain certain terms.

## a) Force all main effects into the model (never drop them).
##    - Start from a model that already includes the mains.
##    - Use "keep" to prevent dropping mains, but allow dropping interactions.

base_mod <- glm(y ~ x1 + x2 + x3, family = poisson, data = dat)

## keep: function that returns TRUE for terms that must be kept.
##       Called once per term by step().
##
## Here: keep any term that is not an interaction (i.e., main effects stay).
##       interaction terms (those containing ":") can be dropped.

keep_mains <- function(model, data, extra) {
  terms_obj <- attr(terms(model), "term.labels")
  ## TRUE for terms with no ":" (main effects), FALSE for interactions
  keep_vec <- !grepl(":", terms_obj)
  return(keep_vec)
}

## Now step() can add/drop within the specified scope, but main effects
## will always be kept if they ever enter the model.

scope_hier <- list(
  lower = ~ x1 + x2 + x3,     # base model must include all mains
  upper = ~ x1 * x2 + x3      # allow x1:x2 interaction on top
)

step_hier_mod <- step(
  object    = base_mod,       # start with all mains present
  scope     = scope_hier,
  direction = "both",
  trace     = 0,
  keep      = keep_mains      # enforce that mains are not removed
)

summary(step_hier_mod)

## Result: step_hier_mod may include or exclude x1:x2, but it will not drop
## x1, x2, or x3 because keep_mains() flags them as "must keep".


################################################################################
## 6. Manual add/drop helpers: add1() and drop1() -----------------------------
################################################################################

## For small problems or exams, you may manually run one-step additions/drops
## and choose the best option yourself.

## drop1(): test dropping each term from a model
drop1(full_mod, test = "Chisq")   # GLM deviance-based LR tests

## add1(): test adding each candidate term (must specify scope)
add1(null_mod, scope = ~ x1 * x2 + x3, test = "Chisq")

## In practice:
## - You would repeatedly call drop1() or add1() and refit the chosen model.
## - step() automates exactly this, using AIC or tests internally.
