library(e1071);
library(foreach);
library(doParallel);
library(randomForest);

# Start cluster
stopImplicitCluster();
registerDoParallel(cores = detectCores());


#Compute total energy production by cluster

total_cluster_train <- list()
for (cluster in 1:length(clusters)){
  total_cluster_train[[cluster]] <- rowSums(train[, clusters[[cluster]], with = F])
}

#Train a model to predict total energy production using its predictors and hyperparameters
model_total_cluster_svm <-  foreach(cluster = 1:length(clusters), 
                                    .packages = c("e1071", "data.table"))%dopar%{
                                
    #predictors_cluster = predictors[[cluster]];

    svm(y = total_cluster_train[[cluster]], x = train[, predictors[[cluster]], with = F], 
        data = train, 
        kernel="radial",
        cost = hyperparameters[[cluster]]$c, 
        epsilon = hyperparameters[[cluster]]$eps,
        gamma = hyperparameters[[cluster]]$gamma);
}


#Train a simpler model for each station using total production per cluster as unique predictor


model_cluster_lm <-  foreach(cluster = 1:length(clusters), .combine = c)%:%
  foreach(station = clusters[[cluster]], .packages = c("data.table"))%dopar%{
    dt <- cbind(train[, ..station], total_cluster_train[[cluster]]);
    formula <- paste0(station, "~" , colnames(dt)[2]);
    lm(formula, data = dt);    
}

# Name each model with the station name
names(model_cluster_lm) <- unlist(clusters); 

#Predict total energy by cluster using SVM model 

predictions_total_test <-  foreach(cluster = 1:length(clusters), .packages = c("e1071", "data.table"))%dopar%{
  
  predict(model_total_cluster_svm[[cluster]], newdata = test[, predictors[[cluster]], with = F]);
}

#Predict production in station using predicted TOTAL by cluster 
predictions_test <-  foreach(cluster = 1:length(clusters), .combine = cbind)%:%
  foreach(station = clusters[[cluster]], 
          .packages = c("data.table"), .combine = cbind)%dopar%{
            
            dt <- cbind(test[, ..station], predictions_total_test[[cluster]]);
            model <- model_cluster_lm[[station]];
            # Predict the station using only total per cluster as predictor
            predict(model, newdata = dt);
            
          }

# Name predictions and alphabetically order them 
colnames(predictions_test) <- unlist(clusters);
predictions_test <- predictions_test[, sort(colnames(predictions_test))];
identical(stationsNames, colnames(predictions_test));


errors_test <- predictions_test - test[, ..stationsNames];

#Compute Metric (MAE)

mae_test <- round(mean(as.matrix(abs(errors_test))), 5); #2456445