######################### 1) LOAD DATA INTO MEMORY #################################

library(data.table);
library(apcluster);
library(caret);
library(foreach);
library(doParallel);

#Load datasets 


folder_path <- "./files/";

data_original <- readRDS(file.path(folder_path, "solar_dataset.RData"));
stations <- fread(file.path(folder_path, "station_info.csv"));

dim(data_original)
dim(stations)

colnames(data)[1] ### Date 
stationsNames <- colnames(data_original)[2:99] ### Station names
colnames(data)[100:456] ### Principal components

#Remove NA rows to be predicted

which(data$Date == '20071231'); ### Last row with information 5113
data <- data_original[1:5113,] 

# Set seed to get reproducible results

set.seed(14); 

#Add row indices for training data (70%)
train_index <- sample(1:nrow(data), 0.7*nrow(data));  

# row indices for validation data (15%)
val_index <- sample(setdiff(1:nrow(data), train_index), 0.15*nrow(data));  

# row indices for test data (15%)
test_index <- setdiff(1:nrow(data), c(train_index, val_index));

# split data
train <- data[train_index]; 
val <- data[val_index]; 
test  <- data[test_index];

######################### 2) DEFINITION OF FUNCTIONS ##########################################

# Select important variables 

select_important<-function(y, n_vars, dat){
  varimp <- filterVarImp(x = dat, y=y, nonpara=TRUE);
  varimp <- data.table(variable=rownames(varimp),imp=varimp[, 1]);
  varimp <- varimp[order(-imp)];
  selected <- varimp$variable[1:n_vars];
  return(selected);
}

# Grid Search Clusters SVM 

grid_search <- function(cluster, predictors, base_station,
                        c_values = 10^seq(from = 1, to = 3, by = 0.5),
                        eps_values = 10^seq(from = -2, to = 0, by = 0.5), 
                        gamma_values = 10^seq(from = -5, to = -3, by = 0.5)){

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

######################### 3) CLUSTERING OF STATIONS #####################################

# Clusters using energy production in the Stations

data_scaled <- scale(data[, ..stationsNames])
data_transpose <- as.data.frame(t(as.matrix(data_scaled)))

sim_matrix <- negDistMat(data_transpose, r=2)
dim(sim_matrix)
ap_result <- apcluster(sim_matrix)

#Extract the clusters to a list
clusters <- list()
for (i in 1:length(ap_result@clusters)){
  clusters[[i]] <- names(ap_result@clusters[[i]])
}

#Extract each exemplar as base_station to grid search

base_stations <- as.list(names(ap_result@exemplars))

############################### 4) TRAIN THE  MODEL #################################

# Start cluster
stopImplicitCluster();
registerDoParallel(cores = detectCores());

# Most important predictors per cluster

predictors <- foreach(cluster = clusters, .packages = c("caret", "data.table"))%dopar%{
   pc <- sapply(train[, ..cluster], select_important, n_vars = 20, dat = train[, PC1:PC110]);
   pc <- sort(table(pc), decreasing = TRUE);
   names(pc)[1:length(pc) -1]; #Discart last predictor to avoid overfit
}
 
# Hyperparameters optimization per cluster

hyperparameters <- foreach(cluster = 1:length(clusters), 
                           .packages = c("e1071", "data.table", "foreach", "doParallel"))%dopar%{
  grid_search(clusters[[cluster]], predictors[[cluster]], base_stations[[cluster]]);
}

model_cluster_svm <-  foreach(cluster = 1:length(clusters), .combine = c)%:%
  foreach(station = clusters[[cluster]], .packages = c("e1071", "data.table"))%dopar%{
    
    predictors_cluster = predictors[[cluster]];

  #Train the model with the predictors and hyperparamenters of the cluster
      svm(y = train[, ..station], x = train[, ..predictors_cluster], 
      data = train, 
      kernel="radial",
      cost = hyperparameters[[cluster]]$c, 
      epsilon = hyperparameters[[cluster]]$eps,
      gamma = hyperparameters[[cluster]]$gamma);
}

# Name each model with the station name
names(model_cluster_svm) <- unlist(clusters); 

#Get model predictions for multiple models (ST approach)

predictions_test <-  foreach(cluster = 1:length(clusters), .combine = cbind)%:%
  foreach(station = clusters[[cluster]], 
          .packages = c("e1071", "data.table"), .combine = cbind)%dopar%{
    
    predictors_cluster = predictors[[cluster]];
    
    # Predict the station using predictors and hyperparamenters of the cluster
    predict(model_cluster_svm[[station]], newdata = test[, ..predictors_cluster]);
    
          }

# Name predictions and alphabetically order them 
colnames(predictions_test) <- unlist(clusters);
predictions_test <- predictions_test[, sort(colnames(predictions_test))];
identical(stationsNames, colnames(predictions_test));


errors_test <- predictions_test - test[, ..stationsNames];

#Compute Metric (MAE)

mae_test <- round(mean(as.matrix(abs(errors_test))), 5); #2424213


########################## 5) KAGGLE PREDICTIONS  #######################################

model <-  foreach(cluster = 1:length(clusters), .combine = c)%:%
  foreach(station = clusters[[cluster]], .packages = c("e1071", "data.table"))%dopar%{
    
    predictors_cluster = predictors[[cluster]];
    
    #Train the model with the predictors and hyperparamenters of the cluster
    svm(y = train[, ..station], x = train[, ..predictors_cluster], 
        data = rbind(train, val, test), 
        kernel="radial",
        cost = hyperparameters[[cluster]]$c, 
        epsilon = hyperparameters[[cluster]]$eps,
        gamma = hyperparameters[[cluster]]$gamma);
  }

# Name each model with the station name
names(model) <- unlist(clusters); 


#Build the summision
data_predict <- data_original[5114:6909,]
dim(data_predict)

#Get model predictions for multiple models (ST approach)

predictions_kaggle <-  foreach(cluster = 1:length(clusters), .combine = cbind)%:%
  foreach(station = clusters[[cluster]], 
          .packages = c("e1071", "data.table"), .combine = cbind)%dopar%{
            
            predictors_cluster = predictors[[cluster]];
            
            # Predict the station using predictors and hyperparamenters of the cluster
            predict(model[[station]], newdata = data_predict[, ..predictors_cluster]);
            
          }

# Name predictions and alphabetically order them 
colnames(predictions_kaggle) <- unlist(clusters);
predictions_kaggle <- predictions_kaggle[, sort(colnames(predictions_kaggle))];

submission <- data_predict[, Date:WYNO]
as.data.table(predictions_kaggle) -> submission[, 2:99]

write.csv(submission, file = "./files/submission_cluster_svm.csv", row.names = FALSE)

