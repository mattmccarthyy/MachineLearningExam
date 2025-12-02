# 122358476 -- Matthew McCarthy -- R Script
rm(list = ls())
# setwd("C:/Users/matth/Desktop/Machine Learning Assignment")

###############################################################################
# Question 1 Code
###############################################################################
# a).
set.seed(123)

M <- 1000; N <- 100; a <- 3; b <- 2

samples <- matrix(rgamma(M * N, a, b), ncol = M, nrow = N)

sample_means = colMeans(samples)

# Monte Carlo estimate of the expected value of the sample mean
MC_E_mean <- mean(sample_means) 
MC_E_mean


# b).
# In report.


# c).
sd_error <- sd(sample_means);
sd_error





###############################################################################
# Question 2 Code
###############################################################################
rm(list = ls())
set.seed(123)

data(trees)
col_primary <- "#8d17f1" # purple

# a).
N <- 100; n <- nrow(trees); slope_boot = numeric(N)

for (i in 1:N) {
  boot_indices <- sample(1:n, size = n, replace = TRUE)
  tree_boot <- trees[boot_indices, ]
  boot_lm <- lm(Height~Girth, data = tree_boot)
  slope_boot[i] = coef(boot_lm)["Girth"]
}

par(mfrow = c(1,1))
boxplot(slope_boot, main = "Bootstrap Distribution of Slope",
        ylab = "Bootstrap Slope",
        col = col_primary, ylim = c(0.3, 1.8))


# b).
boot_mean_slope <- mean(slope_boot)
boot_mean_slope


# c).
boot_se_slope <- sd(slope_boot)
boot_se_slope


# d).
ci95 <- quantile(slope_boot, probs = c(0.025, 0.975))
ci95



###############################################################################
# Question 3 Code
###############################################################################
rm(list = ls())
set.seed(123)
library(dplyr)
library(tidyr)
library(ggplot2)
library(splines)


# Will be plotting a large amount, using colours I like that give good contrast.
col_primary <- "#8d17f1" # purple
col_secondary <- "black" # bit more self-explanatory

eu_url <- "https://raw.githubusercontent.com/mattmccarthyy/ST4060-Machine-Learning-Assignment/refs/heads/main/data/eudirectlapse.csv"
eudirectlapse <- read.csv(eu_url)


# a).
##################################################################
# Checking Overall Structure
##################################################################
glimpse(eudirectlapse) # Created a data dictionary, being placed in Overleaf.
summary(eudirectlapse)
colSums(is.na(eudirectlapse)) # No NA's present.



##################################################################
# Examining Outcome Distribution (Overall Lapse Rate)
##################################################################
tab_lapse <- table(eudirectlapse$lapse)
tab_lapse
prop.table(table(eudirectlapse$lapse))
# In total there are 23,060 policies, of which ~12.81% lapsed.

# Visualisation
# Proportions of 0/1
p_lapse <- prop.table(tab_lapse)

par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i",
    mar   = c(5.5, 5.5, 3, 1),
    tcl   = -0.25,
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    mgp   = c(3.5, 0.7, 0))

barplot(p_lapse,
        names.arg = c("Persisted", "Lapsed"),         
        col  = c("grey", col_primary),
        ylab = "Proportion of policies",
        xlab = "Policy outcome",
        bty  = "l")

# horizontal line at overall lapse rate
abline(h = p_lapse["1"], lty = 2, col = col_secondary, lwd = 2)



##################################################################
# Examining Effect of Categorical Variables
##################################################################
#################################
# Converting Lapse to a Factor
#################################
eudirectlapse$lapse = factor(eudirectlapse$lapse, levels = c(0, 1),labels = c("Persisted", "Lapsed"))


#################################
# Converting Chr Cols to Factors
#################################
char_cols <- sapply(eudirectlapse, is.character) # converted all character predictors to factors
for (j in which(char_cols)) {
  eudirectlapse[[j]] <- factor(eudirectlapse[[j]])
} # This misses number of contracts, dealing with that separately.


####################################
# Looking at region in more detail:
####################################
# Contingency table and row-wise proportions
tab_region <- table(eudirectlapse$vehicl_region, eudirectlapse$lapse)
tab_region
prop_region <- prop.table(tab_region, 1) # proportions within region

# lapse rate in each region
lapse_rate <- prop_region[, "Lapsed"]

# index of regions from highest to lowest lapse rate
ord <- order(lapse_rate, decreasing = TRUE)

# reorder table and proportions
tab_region_sorted  <- tab_region[ord, ]
prop_region_sorted <- prop_region[ord, ]
overall_lapse <- mean(eudirectlapse$lapse == "Lapsed")

par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i", # Pulling axes into plot for more space.
    mar   = c(5.5, 5.5, 3, 1),
    tcl = -0.25, # making ticks shorter
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

bp <- barplot(
  t(prop_region_sorted),
  ylim   = c(0, 1),
  col    = c("grey", col_primary),
  ylab   = "Proportion Within Region",
  xlab   = "Vehicle Region",
  las    = 2,
  names.arg = rownames(prop_region_sorted) # labels in sorted order
)

# Region aggregation
# PLEASE NOTE: Mainly for Q3 (b), hence majority being discussed there in the report.
# Starting from factor, to character
eudirectlapse$vehicl_region <- as.character(eudirectlapse$vehicl_region)

# Merge 6 and 7 into "6/7"
eudirectlapse$vehicl_region[
  eudirectlapse$vehicl_region %in% c("Reg6", "Reg7")
] <- "6/7"

# Merge 8 and 10 into "8/10"
eudirectlapse$vehicl_region[
  eudirectlapse$vehicl_region %in% c("Reg8", "Reg10")
] <- "8/10"

# Merge 9, 11 and 14 into "9/11/14"
eudirectlapse$vehicl_region[
  eudirectlapse$vehicl_region %in% c("Reg9", "Reg11", "Reg14")
] <- "9/11/14"

# Back to factor
eudirectlapse$vehicl_region <- factor(eudirectlapse$vehicl_region)
str(eudirectlapse$vehicl_region)



# THIS IS FOR PART (b). Doing here while fresh in my head.
# Re-running our plotting for further analysis:
tab_region <- table(eudirectlapse$vehicl_region, eudirectlapse$lapse)
tab_region
prop_region <- prop.table(tab_region, 1) # proportions within region

# lapse rate in each region
lapse_rate <- prop_region[, "Lapsed"]

# index of regions from highest to lowest lapse rate
ord <- order(lapse_rate, decreasing = TRUE)

# reorder table and proportions
tab_region_sorted  <- tab_region[ord, ]
prop_region_sorted <- prop_region[ord, ]
overall_lapse <- mean(eudirectlapse$lapse == "Lapsed")

par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i", # Pulling axes into plot for more space.
    mar   = c(5.5, 5.5, 3, 1),
    tcl = -0.25, # making ticks shorter
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

bp <- barplot(
  t(prop_region_sorted),
  ylim   = c(0, 1),
  col    = c("grey", col_primary),
  ylab   = "Proportion Within Region",
  xlab   = "Vehicle Region",
  las    = 2,
  names.arg = rownames(prop_region_sorted) # labels in sorted order
)

# Can get rid of two more
eudirectlapse$vehicl_region <- as.character(eudirectlapse$vehicl_region)

# Merge 3 and 6/7 into "3/6/7"
eudirectlapse$vehicl_region[
  eudirectlapse$vehicl_region %in% c("Reg3", "6/7")
] <- "Reg3/6/7"

# Merge 4 and 8/10 into "4/8/10"
eudirectlapse$vehicl_region[
  eudirectlapse$vehicl_region %in% c("Reg4", "8/10")
] <- "Reg4/8/10"

# Back to factor
eudirectlapse$vehicl_region <- factor(eudirectlapse$vehicl_region)
str(eudirectlapse$vehicl_region) # Only 8 levels.

# Plotting once more.
tab_region <- table(eudirectlapse$vehicl_region, eudirectlapse$lapse)
tab_region
prop_region <- prop.table(tab_region, 1) # proportions within region

# lapse rate in each region
lapse_rate <- prop_region[, "Lapsed"]

# index of regions from highest to lowest lapse rate
ord <- order(lapse_rate, decreasing = TRUE)

# reorder table and proportions
tab_region_sorted  <- tab_region[ord, ]
prop_region_sorted <- prop_region[ord, ]
overall_lapse <- mean(eudirectlapse$lapse == "Lapsed")

par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i", # Pulling axes into plot for more space.
    mar   = c(5.5, 5.5, 3, 1),
    tcl = -0.25, # making ticks shorter
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

bp <- barplot(
  t(prop_region_sorted),
  ylim   = c(0, 1),
  col    = c("grey", col_primary),
  ylab   = "Proportion Within Region",
  las    = 2,
  names.arg = rownames(prop_region_sorted) # labels in sorted order
)



#################################
# Converting No. of Policies to Factors
#################################
unique(eudirectlapse$policy_nbcontract)
table(eudirectlapse$lapse, eudirectlapse$policy_nbcontract)
# Higher policy number (categories of 6+ policies) have tiny samples, these would lead to unstable estimates.
# Relationship is also clearly not linear (look at change from 2 to 3 and then from 10 to 11.)
# Decided to create a factor with 4 levels.
eudirectlapse$policy_nbcontract <- factor(
  case_when(
    eudirectlapse$policy_nbcontract == 1 ~ "1",
    eudirectlapse$policy_nbcontract == 2 ~ "2",
    eudirectlapse$policy_nbcontract %in% 3:4 ~ "3-4",
    eudirectlapse$policy_nbcontract >= 5 ~ "5+"
  ),
  levels = c("1", "2", "3-4", "5+")
) 



##############################################
# Examining effect of Gender
##############################################
tab_gender <- table(eudirectlapse$polholder_gender, eudirectlapse$lapse)
tab_gender
prop.table(tab_gender, margin = 1) # Men do lapse more than women. 

chisq.test(tab_gender)
# H0: lapse is independent of gender (same proportion for males and females)
# H1: at least one gender has a different lapse proportion
# p = 0.003 => there is evidence that lapse rate differs by gender.
## Lapse rates are about 12.0% for females and 13.3% for males (difference of ~1.3%), so
## gender is statistically associated with lapse, though the effect size is small.

par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i", # Pulling axes into plot for more space.
    mar   = c(5.5, 5.5, 3, 1),
    tcl = -0.25, # making ticks shorter
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0)) # title farther from ticks, tick labels unchanged

barplot(t(prop.table(tab_gender, margin = 1)),
        beside = FALSE,
        legend.text = c("Persisted", "Lapsed"),
        args.legend = list(x = "topright",
                           inset = 0.02,
                           bty = "o",
                           bg = "white",
                           box.lwd = 1,
                           cex = 0.9),
        xlab = "Policyholder Gender",
        ylab = "Proportion Within Gender",
        col = c("grey", col_primary)) # Main omitted on purpose. Will outline in caption.
abline(h = prop.table(tab_gender, margin = 1)[2,1], lty = 2, lwd = 3, col = col_secondary) 



##############################################
# Examining effect of Gender  (clean plot)
##############################################
tab_gender   <- table(eudirectlapse$polholder_gender, eudirectlapse$lapse)
prop_gender  <- prop.table(tab_gender, margin = 1)
lapse_gender <- prop_gender[, "Lapsed"]
overall_lapse <- mean(eudirectlapse$lapse == "Lapsed")

par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i",
    mar   = c(5.5, 5.5, 3, 1),
    tcl   = -0.25,
    cex.main = 1.4,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    mgp   = c(3.5, 0.7, 0))

bp <- barplot(lapse_gender,
              ylim  = c(0, max(lapse_gender) * 1.25),
              col   = col_primary,
              border = "black",
              names.arg = c("Female", "Male"),
              ylab  = "Lapse probability",
              xlab  = "Policyholder Gender",
              bty   = "l")

abline(h = overall_lapse, lty = 2, lwd = 2, col = col_secondary)

# add % labels above bars
text(x = bp,
     y = lapse_gender + 0.005,
     labels = sprintf("%.1f%%", 100 * lapse_gender),
     cex = 1.0)



#################################
# Converting Policy Age
#################################
table(eudirectlapse$lapse, eudirectlapse$policy_age)
unique(eudirectlapse$prem_policy_age)
# Policies older than 9 years are incredibly sparse, combining would provide stability.
# Furthermore, lower year policies is seen have a higher lapse risk from the data.
# A factor makes the most sense here.

eudirectlapse$policy_age <- cut(
  eudirectlapse$policy_age,
  breaks = c(-Inf, 2, 5, 8, Inf),
  labels = c("0-2", "3-5", "6-8", "9+"),
  include.lowest = TRUE
)

str(eudirectlapse)



##############################################
# Examining effect of Car Use
##############################################
tab_caruse <- table(eudirectlapse$lapse, eudirectlapse$policy_caruse)
tab_caruse # Commercial has little data, would cause issues in a chisq test. (Want counts >= 5)
## Commercial clearly has a significant effect (~20% lapse rate), but the sample size is tiny.

eudirectlapse$policy_caruse = as.character(eudirectlapse$policy_caruse)
eudirectlapse$policy_caruse[eudirectlapse$policy_caruse == "commercial"] = "unknown"
eudirectlapse$policy_caruse = as.factor(eudirectlapse$policy_caruse)
## FOR OVERLEAF: this was done to ensure expected counts >= 5 per cell for chi-square validity and to avoid unstable logistic-regression coefficients.


tab_caruse_new <- table(eudirectlapse$lapse, eudirectlapse$policy_caruse)
tab_caruse_new
prop.table(tab_caruse_new) # Small difference between private/freelance and unknown. 
chisq.test(tab_caruse_new) # Effect is statistically significant, may not be meaningful.

# lapse rates per car-use category
lapse_rates <- prop.table(tab_caruse_new, margin = 2)["Lapsed", ]
lapse_rates # 8% vs. ~13.5% lapse rates, clearly a large difference.
# We have such a large amount of data this will most likely be used as a stable predictor.

par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i",
    mar   = c(5.5, 5.5, 3, 1),
    tcl   = -0.25,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    mgp  = c(3.5, 0.7, 0))

barplot(lapse_rates,
        ylim = c(0, max(lapse_rates) * 1.2),
        ylab = "Lapse probability",
        xlab = "Car use",
        col  = col_primary,
        border = "black",
        las = 1,
        bty = "l")



# Adding extra to final report (plot sucks).




##############################################
# Examining effect of Different Drivers
##############################################
unique(eudirectlapse$polholder_diffdriver) # 7 levels to work with.

tab_diffdriver <- table(eudirectlapse$lapse, eudirectlapse$polholder_diffdriver)
tab_diffdriver # Commercial, Learner 17 and Unknown have very little observations.
## FOR OVERLEAF: this was done to ensure expected counts >= 5 per cell for chi-square validity and to avoid unstable logistic-regression coefficients.

eudirectlapse$polholder_diffdriver = as.character(eudirectlapse$polholder_diffdriver)
rip <- c("unknown", "commercial", "learner 17")
for (var in rip) {
  eudirectlapse$polholder_diffdriver[eudirectlapse$polholder_diffdriver == var] = "other/rare/unknown"
}
eudirectlapse$polholder_diffdriver = as.factor(eudirectlapse$polholder_diffdriver)

unique(eudirectlapse$polholder_diffdriver) # 3 levels removed
tab_diffdriver_new <- table(eudirectlapse$lapse, eudirectlapse$polholder_diffdriver)
tab_diffdriver_new
# The slope is now estimable and reasonably stable.
# Its standard error will still be larger than for big groups like "same", but as long as this is flagged "it's all legal".
## "Rare categories were merged to create an 'other/rare/unknown' group (94 policies) to allow more reliable estimation."



##############################################
# Examining effect of Vehicle Garage
##############################################
tab_garage <- table(eudirectlapse$vehicl_garage, eudirectlapse$lapse)
tab_garage
prop_garage <- prop.table(tab_garage, margin = 1)

# lapse rate in each region
lapse_rate <- prop_garage[, "Lapsed"]

# index of regions from highest to lowest lapse rate
ord <- order(lapse_rate, decreasing = TRUE)

# reorder table and proportions
tab_garage_sorted  <- tab_garage[ord, ]
prop_garage_sorted <- prop_garage[ord, ]
overall_lapse <- mean(eudirectlapse$lapse == "Lapsed")

par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i", # Pulling axes into plot for more space.
    mar   = c(5.5, 5.5, 3, 1),
    tcl = -0.25, # making ticks shorter
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

bp <- barplot(
  t(prop_garage_sorted),
  ylim   = c(0, 1),
  col    = c("grey", col_primary),
  ylab   = "Proportion Within Garage Type",
  xlab   = "",
  las    = 2,
  names.arg = rownames(prop_garage_sorted) # labels in sorted order
)

prop_garage_sorted
# Incredibly similiar risk profiles in "other", "parking deck", "private garage" and "street".
# Also similiar in "unknown" and "carport".
# Aggregating these levels:
group1 <- c("other", "parking deck", "private garage", "street")
group2 <- c("unknown", "carport")
eudirectlapse$vehicl_garage <- as.character(eudirectlapse$vehicl_garage)
for (var in group1) {
  eudirectlapse$vehicl_garage[eudirectlapse$vehicl_garage == var] = "garage group 1"
}
for (var in group2) {
  eudirectlapse$vehicl_garage[eudirectlapse$vehicl_garage == var] = "garage group 2"
}
unique(eudirectlapse$vehicl_garage)
eudirectlapse$vehicl_garage <- factor(eudirectlapse$vehicl_garage)





#################################################################################################################################
# PART (b)
##############################################
# Train/Test Split (prep for part (c))
##############################################
prop_train <- 0.7

idx1 <- which(eudirectlapse$lapse == "Lapsed") # lapsed policies  
idx0 <- which(eudirectlapse$lapse == "Persisted") # persisted policies

train1 <- sample(idx1, size = floor(prop_train * length(idx1)))
train0 <- sample(idx0, size = floor(prop_train * length(idx0)))

train_idx <- c(train1, train0)

train <- eudirectlapse[train_idx, ]
test  <- eudirectlapse[-train_idx, ]

# Ensuring lapse proportion remains consistent
prop.table(table(eudirectlapse$lapse))
prop.table(table(train$lapse))
prop.table(table(test$lapse))


##############################################
# Initial Model Fitting
##############################################
mod_full <- glm(lapse ~ ., data = train, family = binomial(link = logit)) # Including every predictor.
summary(mod_full)

set.seed(123)
# Performing backward selection for a "candidate model". 
mod_initial <- step(mod_full, direction = "both", trace = 0)
summary(mod_initial)# This is just a reasonable, interpretable baseline model.



##############################################
# Testing if vehicl_garage should remain in the model
##############################################
mod_full1  <- glm(lapse ~ . + vehicl_garage, family = binomial, data = train) # Using only the training data, not the entire dataset.

mod_nogar <- update(mod_full1, . ~ . - vehicl_garage)

anova(mod_nogar, mod_full1, test = "Chisq")
# garage should definitely stay in the model



##############################################
# Collinearity
##############################################
# Before testing interactions, I want to address any collinearity/multicollinearity concerns.
# I anticiapte problems with premium variables (e.g. prem_final, prem_last, prem_market, prem_pure)
# and vehicle age variables (e.g. vehicl_age, vehicl_agepurchase).
# Criterion: if VIF > 5 I will consider removal, and if VIF > 10, these are definitely problematic.
# install.packages("car")
library(car)
vif(mod_initial)

# Drop prem_pure
mod_initial <- update(mod_initial, . ~ . - prem_pure)
summary(mod_initial)
vif(mod_initial)

# VIF prem_final still > 5, and prem_market has increased to 6.037. 
# prem_market is less interpretable for market strategy, so removing that first.
mod_initial <- update(mod_initial, . ~ . - prem_market)
summary(mod_initial)
vif(mod_initial)

max(vif(mod_initial)[, "GVIF^(1/(2*Df))"]) < 5
# All collinearity concerns have been addressed, highest adjusted VIF value is less than 5.

# Final check
alias(mod_initial)
# Full column rank, no exact linear dependencies, no alisaing, all coefs identifiable. 



##############################################
# Interaction Testing
##############################################
# Adding statistically significant interactions to the model. We will then examine practicality and practical significance.
# Policy holder age and vehicle age
mod_test <- update(mod_initial, . ~ . + polholder_age:vehicl_age)
anova(mod_initial, mod_test, test = "Chisq") # Not statistically significant

# Final premium and policy caruse
mod_test <- update(mod_initial, . ~ . + prem_final:policy_caruse)
anova(mod_initial, mod_test, test = "Chisq") # Not statistically significant

# Final premium and premium frequency
mod_test <- update(mod_initial, . ~ . + prem_final:prem_freqperyear)
anova(mod_initial, mod_test, test = "Chisq") # Not statistically significant

# Age and Gender
mod_test <- update(mod_initial, . ~ . + polholder_age:polholder_gender)
anova(mod_initial, mod_test, test = "Chisq") # Not statistically significant

# Claims history and Vehicle age
mod_test <- update(mod_initial, . ~ . + polholder_BMCevol:vehicl_age)
anova(mod_initial, mod_test, test = "Chisq") # Not statistically significant

# Policy age and Vehicle age
mod_test <- update(mod_initial, . ~ . + policy_age:vehicl_age)
anova(mod_initial, mod_test, test = "Chisq") # Not statistically significant

# Premium and Gender (price sensitivity by gender)
mod_test <- update(mod_initial, . ~ . + prem_final:polholder_gender)
anova(mod_initial, mod_test, test = "Chisq") # Not statistically significant

# No logical interactions tested above are statistically significant.
# This is actually good for a few reasons:
# Main effects capture most of the variation - stepwise selection already selected the strongest predictors
# Interactions are more subtle - we would likely need a larger sample size to detect, could do bagging but likely excessive here.
# Adding interactions without strong evidence would hurt generalisation
# Business interpretability - main effects models are clearer for strategy
# No spurious interactions makes this simpler, this is excellent for better business communication.
# Proceeding without re-checking GVIF or alisaing, no interactions needed. 



##############################################
# The Age Effect
##############################################
# Current final model (linear age)
mod_age_linear <- mod_initial

# Same model but with polholder_age modelled by a spline
mod_age_spline <- update(
  mod_initial,
  . ~ . - polholder_age + ns(polholder_age, df = 3)
)

anova(mod_age_linear, mod_age_spline, test = "Chisq")

AIC(mod_age_linear, mod_age_spline)
BIC(mod_age_linear, mod_age_spline)
# Again, no change, so won't re-check VIF or aliasing. 



##############################################
# Re-test Collinearity
##############################################
vif(mod_initial)
alias(mod_initial)


##############################################
# Final Model
##############################################
# mod_initial seems perfectly sufficient as is
mod_final <- mod_initial
summary(mod_final)
# odds ratios:
exp(coef(mod_final))

# Confidence intervals:
exp(cbind(OR = coef(mod_final), confint(mod_final)))




#################################################################################################################################
# PART (c)
##################################################################
# Testing classification model
##################################################################
##############################################
# Predicted probabilities (train & test)
##############################################
pred_probs_test  <- predict(mod_initial, newdata = test,  type = "response")
pred_probs_train <- predict(mod_initial, newdata = train, type = "response")



##############################################
# ROC curves and AUC
##############################################
library(pROC)

roc_test  <- roc(test$lapse,  pred_probs_test)
roc_train <- roc(train$lapse, pred_probs_train)

plot(roc_test, main = "ROC Curve - Test Set")

auc_values <- data.frame(
  Sample = c("Train", "Test"),
  AUC    = c(auc(roc_train), auc(roc_test))
)
auc_values



##############################################
# Optimal threshold (Youden index)
##############################################
coords_result <- coords(roc_train, x = "best", best.method = "youden")
optimal_threshold <- coords_result["threshold"]
optimal_threshold  # inspect chosen cut-off
# Used this here in the script as I had 0% sensitivity when threshold was 0.5.



##############################################
# Classification at optimal threshold
##############################################
# install.packages("caret")
library(caret)

pred_probs_test <- predict(mod_initial, newdata = test, type = "response")

# Making sure the threshold is a simple numeric
optimal_threshold <- as.numeric(coords_result["threshold"])



# True labels as a factor with explicit levels
test_lapse <- factor(test$lapse, levels = c("Persisted", "Lapsed"))



# Predicted classes at the optimal threshold
pred_class_test_opt <- factor(
  ifelse(pred_probs_test > optimal_threshold, "Lapsed", "Persisted"),
  levels = c("Persisted", "Lapsed")
)



# Making sure these are the same length, had issues here before. 
length(pred_class_test_opt)
length(test_lapse)



# If there are NAs in lapse, drop them:
keep <- !is.na(test_lapse)
conf_mat_test_opt <- confusionMatrix(
  data      = pred_class_test_opt[keep],
  reference = test_lapse[keep],
  positive  = "Lapsed"
)
conf_mat_test_opt



# Train set
pred_class_train_opt <- factor(
  ifelse(pred_probs_train > optimal_threshold, "Lapsed", "Persisted"),
  levels = c("Persisted", "Lapsed")
)
conf_mat_train_opt <- confusionMatrix(pred_class_train_opt, train$lapse,
                                      positive = "Lapsed")
conf_mat_train_opt
conf_mat_train_opt$table



# Compare key metrics (train vs test)
perf_comparison <- data.frame(
  Sample            = c("Train", "Test"),
  Sensitivity       = c(conf_mat_train_opt$byClass["Sensitivity"],
                        conf_mat_test_opt$byClass["Sensitivity"]),
  Specificity       = c(conf_mat_train_opt$byClass["Specificity"],
                        conf_mat_test_opt$byClass["Specificity"]),
  BalancedAccuracy  = c(conf_mat_train_opt$byClass["Balanced Accuracy"],
                        conf_mat_test_opt$byClass["Balanced Accuracy"])
)
perf_comparison





###############################################################################
# Question 4 Code
###############################################################################
rm(list = ls())

url1 <- "https://raw.githubusercontent.com/mattmccarthyy/ST4060-Machine-Learning-Assignment/refs/heads/main/data/motor_claims_100k_numeric.csv"
motor_claims_100k_numeric <- read.csv(url1)
col_primary <- "#8d17f1"

#######################################################
# a).
colSums(is.na(motor_claims_100k_numeric)) # No NA's.

claims <- motor_claims_100k_numeric
head(claims) # No ID to get rid of

claims_X <- subset(claims, select = -gross_amount)

# PCA (on correlation matrix)
pca_fit <- prcomp(claims_X, 
                  center = TRUE, # subtract variable means
                  scale. = TRUE) # divide by sd's (different units, must standardise)

# Basic output
summary(pca_fit) # eigenvalues and variance explained
pca_fit$rotation[, 1:3] # loadings for first 3 PCs

# Scree Plot
par(cex.main = 3.5,
    cex.lab = 2,
    cex.axis = 2)
plot(pca_fit, type = "l", main = "Scree Plot",
     col = col_primary, pch = 16)

# Biplot of the first two PCs
biplot(pca_fit, choices = c(1,2), cex = 0.6) # Removed this from report, analysis was too long.





###############################################################################
# Q4(b): OLS, Ridge, Lasso, Elastic Net for claim severity
###############################################################################
# install.packages("glmnet")
library(glmnet)
claims <- motor_claims_100k_numeric

y <- claims$gross_amount
X <- as.matrix(subset(claims, select = -gross_amount)) # all numeric predictors


###############################
# 1). Ordinary Least Squares   
###############################
ols_fit <- lm(gross_amount ~ ., data = claims)
summary(ols_fit)
coef(ols_fit)

# 10-fold CV MSE for OLS (for fair comparison)
set.seed(123)
K <- 10
folds <- sample(rep(1:K, length.out = nrow(claims)))
ols_mse <- numeric(K)

for (k in 1:K) {
  train <- folds != k
  test  <- folds == k
  fit_k <- lm(gross_amount ~ ., data = claims[train, ])
  preds <- predict(fit_k, newdata = claims[test, ])
  ols_mse[k] <- mean((y[test] - preds)^2)
}
ols_cv_mse <- mean(ols_mse); ols_cv_mse


###############################
# 2). Ridge regression    
###############################
set.seed(123)
ridge_cv <- cv.glmnet(X, y, alpha = 0, nfolds = 10) # glmnet standardises X by default
ridge_lambda <- ridge_cv$lambda.min
ridge_lambda
ridge_cv_mse <- min(ridge_cv$cvm); ridge_cv_mse

ridge_fit <- glmnet(X, y, alpha = 0, lambda = ridge_lambda)
coef(ridge_fit)


###############################
# 3). Lasso regression (L1)
###############################
set.seed(123)
lasso_cv <- cv.glmnet(X, y, alpha = 1, nfolds = 10)
lasso_lambda <- lasso_cv$lambda.min
lasso_lambda
lasso_cv_mse <- min(lasso_cv$cvm); lasso_cv_mse

lasso_fit <- glmnet(X, y, alpha = 1, lambda = lasso_lambda)
coef(lasso_fit)


###############################
# 4. Elastic Net (L1 + L2)
###############################
set.seed(123)
alpha_grid <- seq(0.1, 0.9, by = 0.1)
alpha_mse  <- numeric(length(alpha_grid))

for (i in seq_along(alpha_grid)) {
  cv_i <- cv.glmnet(X, y, alpha = alpha_grid[i], nfolds = 10)
  alpha_mse[i] <- min(cv_i$cvm)
}
best_alpha <- alpha_grid[which.min(alpha_mse)]
best_alpha

set.seed(123)
en_cv <- cv.glmnet(X, y, alpha = best_alpha, nfolds = 10)
en_lambda <- en_cv$lambda.min
en_lambda
en_cv_mse <- min(en_cv$cvm); en_cv_mse

en_fit <- glmnet(X, y, alpha = best_alpha, lambda = en_lambda)
coef(en_fit)


###############################
# Short Summary     
###############################
data.frame(
  Model       = c("OLS", "Ridge", "Lasso", "Elastic Net"),
  Alpha       = c(NA, 0, 1, best_alpha),
  Lambda      = c(NA, ridge_lambda, lasso_lambda, en_lambda),
  CV_MSE      = c(ols_cv_mse, ridge_cv_mse, lasso_cv_mse, en_cv_mse)
)

# Coefficients for a small set of key predictors, to help describe shrinkage in the report
coef(ols_fit)[c("engine_cc", "vehicle_value", "age", "ncd_level")]
coef(ridge_fit)[c("engine_cc", "vehicle_value", "age", "ncd_level"), ]
coef(lasso_fit)[c("engine_cc", "vehicle_value", "age", "ncd_level"), ]
coef(en_fit)[c("engine_cc", "vehicle_value", "age", "ncd_level"), ]

ols_cv_mse
ridge_lambda; ridge_cv_mse
lasso_lambda; lasso_cv_mse
best_alpha;   en_lambda; en_cv_mse



###############################################################################
# Q4(c) Regression tree for claim severity using rpart
###############################################################################
# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot) # for nicer tree plots


###############################
# 1). Data Preparation
###############################
# Using the same variables as in parts (a) and (b)
claims <- motor_claims_100k_numeric

tree_dat <- claims[, c("gross_amount", "cal_year", "engine_cc", "vehicle_value", "age","ncd_level")]

# Response vs. predictors
form <- gross_amount ~ cal_year + engine_cc + vehicle_value + age + ncd_level


###############################
# 2). Tune minsplit/maxdepth using K-fold CV      
###############################
set.seed(123)
K <- 5
n <- nrow(tree_dat)
folds <- sample(rep(1:K, length.out = n))

# Grid of hyperparameters 
minsplit_vals <- c(100, 500, 1000, 2000)
maxdepth_vals <- c(2, 3, 4, 5, 6)

grid <- expand.grid(
  minsplit = minsplit_vals,
  maxdepth = maxdepth_vals
)

cv_mse <- function(minsplit, maxdepth) {
  mse_k <- numeric(K)
  for (k in 1:K) {
    train_idx <- folds != k
    test_idx  <- folds == k
    
    fit_k <- rpart(
      form,
      data = tree_dat[train_idx, ],
      method = "anova",
      control = rpart.control(
        minsplit = minsplit,
        maxdepth = maxdepth,
        cp = 0.0 # grow as much as allowed; depth/split control size
      )
    )
    
    pred_k <- predict(fit_k, newdata = tree_dat[test_idx, ])
    mse_k[k] <- mean((tree_dat$gross_amount[test_idx] - pred_k)^2, na.rm = TRUE)
  }
  mean(mse_k)
}

# Run grid search
grid$cv_mse <- mapply(cv_mse, grid$minsplit, grid$maxdepth)

# Inspect results and choose best combination
grid[order(grid$cv_mse), ][1:5, ] # top 5 settings
best <- grid[which.min(grid$cv_mse), ]
best # best$minsplit and best$maxdepth are what I'll report in the write-up


###############################
# 3). Fitting Final Tree
###############################
best_tree <- rpart(
  form,
  data   = tree_dat,
  method = "anova",
  control = rpart.control(
    minsplit = best$minsplit,
    maxdepth = best$maxdepth,
    cp       = 0.0,
    xval = 10 # rpart's own internal CV for the cp table
  )
)

summary(best_tree) # detailed node summary
printcp(best_tree) # cp table and x-error
best_tree$variable.importance


###############################
# 4). Visualising The Tree
###############################
rpart.plot(
  best_tree,
  type = 2, # split labels above nodes
  extra = 101, # fitted value + n obs in nodes
  under = TRUE,
  fallen.leaves = TRUE,
  roundint = FALSE
)

# Plotting for report
rpart.plot(
  best_tree,
  type = 2,
  extra = 101,
  under = TRUE,
  fallen.leaves = TRUE,
  roundint = FALSE,
  main = "Regression Tree for Claim Severity"
)

# Overall RMSE of tuned tree (on full data)
pred_full <- predict(best_tree, newdata = tree_dat)
tree_rmse <- sqrt(mean((tree_dat$gross_amount - pred_full)^2))
tree_rmse
best$cv_mse


