library(foreach)
library(doParallel)

# Start cluster
stopImplicitCluster();
registerDoParallel(cores = detectCores());

### Define grid
c_values <- 10^seq(from = 1, to = 3, by = 0.5);
eps_values <- 10^seq(from = -2, to = 0, by = 0.5);
gamma_values <- 10^seq(from = -5, to = -3, by = 0.5);

### Compute grid search
grid_results <-  foreach (c = c_values, .combine = rbind)%:%
  foreach (eps = eps_values, .combine = rbind)%:%
  foreach (gamma = gamma_values, .packages = c("e1071", "data.table"), .combine = rbind)%dopar%{
    print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma));
    
    # train SVM model with a particular set of hyperparamets
    model <- svm(total ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7, 
                 data = train, 
                 kernel="radial",
                 cost = c, epsilon = eps, gamma = gamma);
    
    # Get model predictions
    predictions_train <- predict(model, newdata = train);
    predictions_val <- predict(model, newdata = val);
    
    # Get errors
    errors_train <- predictions_train - train$total;
    errors_val <- predictions_val - val$total;
    
    # Compute Metrics
    mae_train <- round(mean(abs(errors_train)), 5)
    mae_val <- round(mean(abs(errors_val)), 5)
    
    # Build comparison table
    data.table(c = c, eps = eps, gamma = gamma, 
               mae_train = mae_train,
               mae_val = mae_val);
  }

# Order results by increasing mse and mae
grid_results <- grid_results[order(mae_val, mae_train)];

# Check results
best <- grid_results[1];

### Train final model
# train SVM model with best found set of hyperparamets
model <- svm(total ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7, 
             data = rbind(train, val), 
             kernel="radial",
             cost = best$c, epsilon = best$eps, gamma = best$gamma);

# Get model predictions
predictions_test <- predict(model, newdata = test);

# Get errors
errors_test <- predictions_test - test$total;

# Compute Metrics
mae_test <- round(mean(abs(errors_test)), 5)

## Summary
print(sprintf("MAE_test = %s", mae_test));
