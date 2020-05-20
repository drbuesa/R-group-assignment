######################### ONEHOT ENCODING #################################

onehot <- function(x, remove_last = TRUE){
  if (remove_last){
    values <- unique(x)[-length(unique(x))];
  }  else {
    values<-unique(x);
  } 
  
  
  ret<- matrix(0,nrow = length(x),ncol=length(values))
  
  for (i in 1:length(values)){
    ret[, i] <- as.numeric(x==values[i]);
  }
  
  
  colnames(ret)<-as.character(values);
  return(ret);
}



######################### SELECT IMPORTANT VARIABLES ########################

select_important<-function(y, n_vars, dat){
  varimp <- filterVarImp(x = dat, y=y, nonpara=TRUE);
  varimp <- data.table(variable=rownames(varimp),imp=varimp[, 1]);
  varimp <- varimp[order(-imp)];
  selected <- varimp$variable[1:n_vars];
  return(selected);
}

######################### GRID SEARCH CLUSTERS SVM ########################

grid_search <- function(cluster, predictors,
                        c_values = 10^seq(from = 1, to = 3, by = 0.5),
                        eps_values = 10^seq(from = -2, to = 0, by = 0.5), 
                        gamma_values = 10^seq(from = -5, to = -3, by = 0.5)){
  
  # Select the base_station as the most correlated one in the cluster
  total_cor_stations <- colSums(cor(train[, ..cluster]))
  base_station <- names(which(total_cor_stations == max(total_cor_stations), arr.ind = TRUE))
  
  ### Compute grid search
  grid_results <-  foreach (c = c_values, .combine = rbind)%:%
    foreach (eps = eps_values, .combine = rbind)%:%
    foreach (gamma = gamma_values, .packages = c("e1071", "data.table"), .export = c("base_station", "predictors"), .combine = rbind)%dopar%{
      print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma));
      
      # train SVM model with a particular set of hyperparamets
      model <- svm(y = train[, ..base_station], x = train[, ..predictors], 
                   data = train, 
                   kernel="radial",
                   cost = c, epsilon = eps, gamma = gamma);
      
      # Get model predictions
      predictions_train <- predict(model, newdata = train[, ..predictors]);
      predictions_val <- predict(model, newdata = val[, ..predictors]);
      
      # Get errors
      errors_train <- predictions_train - train[, ..base_station];
      errors_val <- predictions_val - val[, ..base_station];
      
      # Compute Metrics
      mae_train <- round(mean(as.matrix(abs(errors_train))), 5);
      mae_val <- round(mean(as.matrix(abs(errors_val))), 5);
      
      # Build comparison table
      data.table(c = c, eps = eps, gamma = gamma, 
                 mae_train = mae_train,
                 mae_val = mae_val);
    }
  
  # Order results by increasing mae
  grid_results <- grid_results[order(mae_val, mae_train)];
  
  # Check results
  best <- grid_results[1];
  return(best);
}