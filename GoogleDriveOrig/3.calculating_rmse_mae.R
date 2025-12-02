# How to calculate RMSE and MAE and comment

# Assuming your model is named 'gam_model' and data is 'df'
preds <- predict(gam_model, type = "response")
actuals <- df$target_variable

# Calculate metrics
rmse <- sqrt(mean((preds - actuals)^2))
mae <- mean(abs(preds - actuals))

print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))