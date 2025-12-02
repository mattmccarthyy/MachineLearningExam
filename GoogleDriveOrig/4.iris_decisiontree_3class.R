rm(list = ls())

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
data(iris)
dataset <- iris
# Iris is already a factor, but ensuring labels are explicit helps
dataset$Species = factor(dataset$Species, levels = c("setosa", "versicolor", "virginica"))
pri = c(1/3, 1/3, 1/3) # This is for parms = list(prior = XX). Iris is balanced (50 each), so equal priors.


# ============================================================
# 1.1. SETUP & CONFIGURATION
# ============================================================
# Define inputs here
target_var <- "Species" 

# Standard predictors for Iris Species
predictors <- c(
  "Sepal.Length", 
  "Sepal.Width", 
  "Petal.Length", 
  "Petal.Width"
)

# Tuning Configuration
split_ratio <- 0.70  
k_folds <- 5 

# Define the Grid
hyper_grid <- expand.grid(
  minsplit = c(5, 10, 20), # Smaller splits needed as Iris dataset is small (150 rows)
  maxdepth = c(2, 3, 4),
  cp = c(0.01,    # Strict (Small, simple tree)
         0.001,   # Moderate (Standard)
         0.0001)  # Loose (Complex, deep tree)
)


# ============================================================
# 1.2. DATA SPLITTING (70/30)
# ============================================================
# Create the formula dynamically
form <- as.formula(paste(target_var, "~", paste(predictors, collapse = " + "))) 

# Stratified Split (Ensures we have enough of each Species in both sets)
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
# 1.6. MULTICLASS AUC (REPLACES YOUDEN THRESHOLD)
# ============================================================
# Note: Youden's index is for binary (2-class) only.
# For 3 classes, we calculate Multiclass AUC to check performance strength.
probs <- predict(base_model, newdata = test_data, type = "prob") # Get probs matrix

# Calculate Multiclass AUC
roc_multi <- multiclass.roc(test_data[[target_var]], probs)
print(paste("Multiclass AUC:", round(auc(roc_multi), 4)))


# ============================================================
# 1.7. FINAL EVALUATION (STANDARD CLASS PREDICTION)
# ============================================================
# Predict highest probability class
final_preds <- predict(base_model, newdata = test_data, type = "class")

# Generate Confusion Matrix
conf_matrix <- confusionMatrix(final_preds, test_data[[target_var]])
print(conf_matrix$overall['Accuracy'])
conf_matrix # Go down to 1.8 to see sample interpretation


# ============================================================
# 1.7. VISUALISATION AND VARIABLE IMPORTANCE
# ============================================================
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

# Plot Tree Structure (Replacing ROC Plot as 3-Class ROC is complex to plot)
rpart.plot(base_model, 
           main = "Iris Decision Tree",
           box.palette = "auto",
           shadow.col = "gray",
           nn = TRUE) # Display Node Numbers

# Variable importance plot
imp <- base_model$variable.importance

if (!is.null(imp)) {
  imp_df <- data.frame(Var = names(imp), Importance = imp)
  
  # Calculate Relative Percentage
  imp_df$Rel <- (imp_df$Importance / sum(imp_df$Importance)) * 100
  
  # Sort Descending (Negative sign works for numeric sorting)
  imp_df <- imp_df[order(-imp_df$Rel), ]
  
  # Bar Plot
  barplot(imp_df$Rel, # SEE 1.10 FOR INTERPRETATION
          names.arg = imp_df$Var, 
          las = 2,                # Rotate labels 90 degrees (Vertical)
          col = "darkgreen",      # Changed color for Iris theme
          main = "Drivers of Species", 
          ylab = "Relative Importance (%)",
          cex.names = 0.7,        # Shrink variable names to fit
          cex.main = 0.9)
  
  # Add a border around the plot
  box()
}



# ============================================================
# 1.8. INTERPRETATION: CONFUSION MATRIX & STATISTICS
# ============================================================
# 1. ACCURACY (~ 0.96 or 96%)
#    - Result: The model is correct roughly 96% of the time. 
#      (Based on the tree image: Setosa is 100% pure, Versicolor is 94% pure, Virginica is 97% pure).
#    - General Rule: 
#      > 90% = Excellent (Easy problem or great predictors).
#      70-90% = Good (Typical business/real-world noise).
#      < 60% = Weak (Requires better data or feature engineering).
#
# 2. KAPPA (~ 0.94)
#    - Result: Extremely high agreement.
#    - General Rule: Kappa removes the "luck" factor.
#      If Accuracy is High (90%) but Kappa is Low (0.20), it means your model 
#      is just guessing the majority class. If both are high, the model is truly learning.
#
# 3. CLASS-SPECIFIC PERFORMANCE (Sensitivity/Specificity)
#    - Setosa: 1.00 (Perfect). The tree split "Petal.Length < 2.6" caught 100% of them.
#    - Versicolor vs Virginica: The confusion matrix will likely show 1 or 2 errors here.
#      This corresponds to the 6% impurity seen in the middle node of your tree diagram.
# ============================================================


# ============================================================
# 1.9. INTERPRETATION: AUC (MULTICLASS)
# ============================================================
# 1. AUC SCORE (0.9556)
#    - Result: 0.9556 is an outstanding score.
#    - What it means: If you pick a random "Setosa" and a random "Virginica", 
#      the model has a 95.6% chance of correctly ranking the Setosa as more likely to be Setosa.
#
# 2. GENERAL RULE FOR AUC
#    - 0.50 = Random Guessing (Coin Flip).
#    - 0.60 - 0.70 = Poor/Fair (Common in complex human behavior data).
#    - 0.70 - 0.80 = Good.
#    - 0.80 - 0.90 = Very Good.
#    - > 0.90 = Excellent (Predictors are very distinct).
#
# 3. MULTICLASS NOTE
#    - Since we have 3 species, this score is the *average* of all pairwise comparisons
#      (e.g., Setosa vs Versicolor, Versicolor vs Virginica, etc.).
# ============================================================


# ============================================================
# 1.10. INTERPRETATION: VARIABLE IMPORTANCE
# ============================================================
# 1. THE RANKING (From your Bar Plot)
#    - 1st: Petal.Width  (~34%)
#    - 2nd: Petal.Length (~32%)
#    - 3rd: Sepal.Length (~22%)
#    - 4th: Sepal.Width  (~12%)
#
# 2. DOMINANT PREDICTORS (The "Petals")
#    - Result: Petal dimensions combined account for ~66% of the model's power.
#    - General Rule: The top variable is your "Primary Splitter." It is usually
#      the variable at the very top of the tree (Root Node).
#
# 3. SECONDARY PREDICTORS (The "Sepals")
#    - Result: Sepal Length is useful (~22%), but Sepal Width is weak.
#    - General Rule: If a variable has < 5% importance, you can often remove it 
#      from future models to simplify data collection without hurting accuracy.
# ============================================================