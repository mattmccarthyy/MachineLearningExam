rm(list = ls())
dat <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds")) 


# =============================================================================
# Module 3: Age Analysis in Gamma GLM (Base R)
# =============================================================================
library(splines) # Required for ns()

# 1. Feature Engineering: Create 10-yr bands.
dat$age_band <- cut(dat$age, breaks = seq(10, 100, by = 10))

# 2. Fit Models
# Model A: Linear (Rigid)
mod_linear <- glm(gross_amount ~ age, family = Gamma(link="log"), data = dat)

# Model B: Factor (Clunky jumps)
mod_factor <- glm(gross_amount ~ age_band, family = Gamma(link="log"), data = dat)

# Model C: Spline (Smooth)
# df = 4 puts 3 knots at quantiles, allowing flexibility for "U-shape" risk
mod_spline <- glm(gross_amount ~ ns(age, df = 6), family = Gamma(link="log"), data = dat)

# 3. Statistical Comparison
# AIC Comparison (Lower is Better)
print(AIC(mod_linear, mod_factor, mod_spline))

# ANOVA (Chisq Test)
# Test if the complex Spline model is significantly better than the Linear model
anova(mod_linear, mod_spline, test = "Chisq")



# =============================================================================
# Visualisation: Severity vs Age (Base R)
# =============================================================================

# Step 1: Calculate Empirical Means and Medians by Age
emp_stats <- aggregate(gross_amount ~ age, data = dat, FUN = function(x) c(mn = mean(x), md = median(x)))
# The output of aggregate with multiple functions is a matrix column, simplify it:
emp_data <- data.frame(age = emp_stats$age, 
                       mean = emp_stats$gross_amount[, "mn"], 
                       median = emp_stats$gross_amount[, "md"])

# Step 2: Create Predictions for the Spline Model
# Create a sequence of ages to plot a smooth line
new_data <- data.frame(age = seq(min(dat$age), max(dat$age), length.out = 100))
new_data$pred <- predict(mod_spline, newdata = new_data, type = "response")

# Step 3: Plotting
# A. Setup the plot window (The Cloud)
# ylim: Constrain to 0-10,000 to ignore the €100k outlier but keep the average visible.
plot(dat$age, dat$gross_amount, 
     pch = 16, cex = 0.6, col = rgb(0.5, 0.5, 0.5, 0.2), 
     ylim = c(0, 25000), 
     xlab = "Driver Age", ylab = "Severity (€)",
     main = "Age Effect: Raw Data vs Gamma Spline")

# B. Overlay Empirical Lines
# Red Dashed = Empirical Mean (Data Truth)
lines(emp_data$age, emp_data$mean, col = "red", lty = 2, lwd = 2)
# Green Dotted = Empirical Median (Skewness Indicator)
lines(emp_data$age, emp_data$median, col = "darkgreen", lty = 3, lwd = 2)

# C. Overlay Model Prediction
# Blue Solid = GLM Prediction
lines(new_data$age, new_data$pred, col = "blue", lwd = 3)

# D. Legend
legend("topright", 
       legend = c("GLM (Spline)", "Empirical Mean", "Empirical Median"),
       col = c("blue", "red", "darkgreen"), 
       lty = c(1, 2, 3), lwd = c(3, 2, 2), bty = "n")

