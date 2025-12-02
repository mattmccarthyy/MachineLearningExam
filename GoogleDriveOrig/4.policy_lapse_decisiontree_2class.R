# Load libraries
library(rpart)
library(rpart.plot)
library(caret) # Useful for clean data splitting
library(tidyverse)
library(pROC)
set.seed(123)

# ============================================================
# 1.0. LOAD IN DATA
# ============================================================
dataset <- read.csv(url("https://raw.githubusercontent.com/mattmccarthyy/ST4060-Machine-Learning-Assignment/refs/heads/main/data/eudirectlapse.csv")) 
dataset$lapse = factor(dataset$lapse, levels = c(0, 1), labels= c("Persisted", "Lapsed"))
pri = c(0.5, 0.5) # This is for parms = list(prior = XX) in rpart(), adjust as needed. Forcing R to care about 12.8% that lapse.


# ============================================================
# 1.1. SETUP & CONFIGURATION
# ============================================================
# Define inputs here
target_var <- "lapse" 

# Standard predictors for Lapse risk
predictors <- c(
  "polholder_age", 
  "polholder_diffdriver", 
  "polholder_gender", 
  "policy_caruse",
  "policy_nbcontract",
  "prem_final",
  "vehicl_age",
  "vehicl_garage",
  "vehicl_region"
)

# Tuning Configuration
split_ratio <- 0.70  
k_folds <- 5 

# Define the Grid
hyper_grid <- expand.grid(
  minsplit = c(20, 50, 100), # Smaller splits often work better for lapse events ###SET
  maxdepth = c(3, 4, 5, 6),
  cp = c(0.01,    # Strict (Small, simple tree)
         0.001,   # Moderate (Standard)
         0.0001)  # Loose (Complex, deep tree), use 0.001 for the interpretations at the end to line up, but we should vary this and find an optimal one.
)


# ============================================================
# 1.2. DATA SPLITTING (70/30)
# ============================================================
# Create the formula dynamically
form <- as.formula(paste(target_var, "~", paste(predictors, collapse = " + "))) 

# Stratified Split (Ensures we have enough "Lapsed" policies in both sets)
train_index <- createDataPartition(dataset[[target_var]], p = split_ratio, list = FALSE)

train_data <- dataset[train_index, ]
test_data  <- dataset[-train_index, ]

# ============================================================
# 1.3. K-FOLD CROSS-VALIDATION FUNCTION (CLASSIFICATION)
# ============================================================
run_class_cv <- function(data, k, grid, formula) {
  
  n <- nrow(data)
  folds <- sample(rep(1:k, length.out = n))
  
  grid$avg_error <- NA
  
  print(paste("Starting Classification CV with K =", k))
  
  for(i in 1:nrow(grid)) {
    curr_min   <- grid$minsplit[i]
    curr_depth <- grid$maxdepth[i]
    curr_cp    <- grid$cp[i]
    
    fold_errors <- numeric(k)
    
    for(j in 1:k) {
      val_idx    <- which(folds == j)
      cv_train   <- data[-val_idx, ]
      cv_val     <- data[val_idx, ]
      
      # Fit model using method="class"
      m <- rpart(
        formula,
        parms = list(prior = pri),
        data = cv_train,
        method = "class", 
        control = rpart.control(minsplit = curr_min, maxdepth = curr_depth, cp = curr_cp)
      )
      
      # Predict Class Labels
      preds <- predict(m, newdata = cv_val, type = "class")
      
      # Calculate Misclassification Rate
      fold_errors[j] <- mean(preds != cv_val[[as.character(formula)[2]]])
    }
    grid$avg_error[i] <- mean(fold_errors)
  }
  return(grid)
}


# ============================================================
# 1.4. EXECUTION & SELECTION
# ============================================================
tuned_results <- run_class_cv(train_data, k_folds, hyper_grid, form)

# Find Best Parameters (Lowest Error Rate)
best_params <- tuned_results[which.min(tuned_results$avg_error), ]

print("Best Parameters Found via CV:")
print(best_params)


# ============================================================
# 1.5. FIT A BASE MODEL WE CAN OPTIMISE
# ============================================================
base_model <- rpart(
  form,
  data = train_data,
  method = "class",
  parms = list(prior = pri),
  control = rpart.control(
    minsplit = best_params$minsplit,
    maxdepth = best_params$maxdepth,
    cp = best_params$cp
  )  
)


# ============================================================
# 1.6. YOUDEN'S INDEX FOR OPTIMAL THRESHOLD
# ============================================================
probs <- predict(base_model, newdata = test_data, type = "prob")[, "Lapsed"] # Get probs on test set (Risk score 0 to 1)

roc_obj <- roc(test_data[[target_var]], probs, levels = c("Persisted", "Lapsed")) # Create ROC curve

optimal_coords <- coords(roc_obj, "best", best.method = "youden")
best_threshold <- optimal_coords$threshold[1]

print(round(best_threshold, 4))


# ============================================================
# 1.7. FINAL EVALUATION AT OPTIMAL THRESHOLD
# ============================================================
# Apply optimal cutoff to generate classes 
final_preds <- ifelse(probs >= best_threshold, "Lapsed", "Persisted")
final_preds <- factor(final_preds, levels = levels(test_data[[target_var]])) # Automatically copies the correct level order from test data

# Generate Confusion Matrix
conf_matrix <- confusionMatrix(final_preds, test_data[[target_var]], positive = "Lapsed")
print(conf_matrix$overall['Accuracy'])
print(conf_matrix$byClass['Sensitivity'])
print(conf_matrix$byClass['Specificity'])
conf_matrix # Go down to 1.8 to see sample interpretation


# ============================================================
# 1.7. VISUALISATION, ROC CURVE AND VARIABLE IMPORTANCE
# ============================================================
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

# ROC Curve plot # GO DOWN TO 1.9 FOR INTERPRETATION
acc_text <- paste0("ROC Curve (Acc: ", round(conf_matrix$overall['Accuracy'] * 100, 1), "%)")
plot(roc_obj, 
     print.thres = "best", # Show the optimal Youden cutoff on graph
     print.auc = TRUE, # Show Area Under Curve
     print.thres.adj = c(1.1, 0.5), # Nudge text to avoid line overlap
     main = acc_text, 
     col = "blue",
     legacy.axes = TRUE, # Standard 0 to 1 axes
     cex.main = 0.9) # Slightly smaller title font
grid(col = "lightgray", lty = "dotted")

# Variable importance plot
imp <- base_model$variable.importance

if (!is.null(imp)) {
  imp_df <- data.frame(Var = names(imp), Importance = imp)
  
  # Calculate Relative Percentage
  imp_df$Rel <- (imp_df$Importance / sum(imp_df$Importance)) * 100
  
  # Sort Descending (Negative sign works for numeric sorting)
  imp_df <- imp_df[order(-imp_df$Rel), ]
  
  # Bar Plot
  barplot(imp_df$Rel, # SEE 1.10 FOR INTERORETATION
          names.arg = imp_df$Var, 
          las = 2,               # Rotate labels 90 degrees (Vertical)
          col = "darkred", 
          main = "Drivers of Lapse Risk", 
          ylab = "Relative Importance (%)",
          cex.names = 0.7,       # Shrink variable names to fit
          cex.main = 0.9)
  
  # Add a border around the plot
  box()
}



# ============================================================
# 1.8. INTERPRETATION: CONFUSION MATRIX & STATISTICS
# ============================================================
# 1. ACCURACY (0.605)
#    - What it means: The % of total guesses that were correct.
#    - Verdict: LOW (~60%). This is lower than the "No Information Rate" (87%),
#      but that is expected. We intentionally sacrificed overall accuracy to 
#      stop ignoring the minority class (Lapsers).
#
# 2. KAPPA (0.065)
#    - What it means: How much better is this model than random guessing?
#      (0 = Random, 1 = Perfect).
#    - Verdict: VERY LOW. This indicates the relationship between your predictors
#      and the target variable is weak. It is a "hard" problem.
#
# 3. SENSITIVITY (0.502) -> THE "CATCH RATE"
#    - What it means: Out of all actual Lapsers, how many did we find?
#    - Verdict: MODERATE (50%). We are catching 1 out of every 2 people who leave.
#      For a difficult behavior like Lapsing, this is a usable result.
#
# 4. SPECIFICITY (0.620) -> THE "SAFE RATE"
#    - What it means: Out of all Loyal customers, how many did we correctly ignore?
#    - Verdict: MODERATE (62%). Conversely, this means our "False Alarm Rate"
#      is 38% (1 - 0.62). We will accidentally flag 38% of loyal customers as risks. THIS IS FINE IF WE ARE SENDING EMAILS (CHEAP), BAD IF WE ARE OFFERING DISCOUNTS (VERY NOT CHEAP). THINK OF BUSINESS IMPACTS.
# ============================================================


# ============================================================
# 1.9. INTERPRETATION: ROC CURVE & AUC
# ============================================================
# 1. AUC SCORE (0.584)
#    - Scale: 0.50 (Coin Flip) to 1.00 (Perfect).
#    - Verdict: WEAK. An AUC of 0.58 suggests the model has very limited 
#      discriminatory power. It is better than random, but not by much.
#      The overlap between "Lapsers" and "Persisters" in the data is huge.
#
# 2. THE CURVE SHAPE
#    - The curve is close to the diagonal line. This visually confirms that 
#      no single threshold gives us both high sensitivity and high specificity.
#      We have to choose one or the other (The Trade-off).
# ============================================================


# ============================================================
# 1.10. INTERPRETATION: VARIABLE IMPORTANCE
# ============================================================
# 1. TOP DRIVERS (Region & Car Use)
#    - Verdict: External factors (Where they live, How they use the car) are
#      stronger predictors of lapsing than the policyholder themselves.
#      'vehicl_region' is the dominant splitter.
#
# 2. THE "SHADOW" VARIABLES (Diff Driver, NB Contract)
#    - Verdict: These bars are tiny or non-existent. 
#      This implies that having a different driver or holding multiple contracts
#      has almost zero impact on whether someone lapses in this specific dataset.
# ============================================================

