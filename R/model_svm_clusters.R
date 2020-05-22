######################### 0) LOAD DATA AND FUNCTIONS INTO MEMORY #################################

library(data.table);
library(caret);
library(ggplot2);
library(foreach);
library(doParallel);

folder_path <- "/Users/drodriguez45/Documents/GitHub/R-group-assignment/";

#Load functions

source(file.path(folder_path, "./R/auxiliary_functions.R"));

#Load datasets 

data_original <- readRDS(file.path(folder_path, "./files/solar_dataset.RData"));
stations <- fread(file.path(folder_path, "./files/station_info.csv"));
additional_vars_clean <- readRDS(file.path(folder_path, "./files/additional_variables_clean.RData"));

dim(data_original)
dim(stations)

colnames(data)[1] ### Date 
stationsNames <- colnames(data_original)[2:99] ### Station names
colnames(data)[100:456] ### Principal components

######################### 1) DATA PREPARATION #################################

#Add month of year as a new dummy variable (onehot encoding)

month <- as.numeric(sapply(data_original$Date, substr, 5,6))
onehot_month <- onehot(month, remove_last = F);
months_of_year <- c("jan", "feb", "mar", "apr", "may", "jun",
                    "jul", "ago", "sep", "oct", "nov", "dec");
colnames(onehot_month) <- months_of_year

#Keep PC1 to PC110 an scale the data, include additional variables 

data_prepared <- cbind(data_original[, Date:WYNO], 
                       onehot_month,
                       scale(data_original[, PC1:PC110]), additional_vars_clean);

#Check last row with information 5113
which(data_prepared$Date == '20071231'); 


#Remove NA rows to be predicted
data <- data_prepared[1:5113,];


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



######################### 2) CLUSTERING OF STATIONS #####################################

# Clusters using energy production in the Stations

normSolar <- scale(data[, ..stationsNames])
normSolar_transpose <- as.data.frame(t(as.matrix(normSolar)))

# Perform clutering using K between 1 and 20
totalwss <- c()
for (k in 1:20){
  result <- kmeans(normSolar_transpose, k)  
  totalwss <- c(totalwss, result$tot.withinss)
}

#Plot to decide the optimum number of clusters 
plot(totalwss[1:15], type = "line")

# Applying the elbow rule
numcl <- 8

#Perform final clustering with the optimum number k

cl_ene <- kmeans(normSolar_transpose, numcl);

ggplot(stations, aes(y = nlat, x = elon)) + 
  geom_point(color = factor(cl_ene$cluster), size = 3) +
  geom_text(label = stations$stid, size = 4);

#Extract the clusters results to a list
clusters <- list()
for (i in 1:numcl){
  clusters[[i]] <- names(cl_ene$cluster)[cl_ene$cluster == i]
}

############################### 3) TRAIN THE  MODEL #################################

# Start cluster
stopImplicitCluster();
registerDoParallel(cores = detectCores());

# Most important predictors per cluster

predictors <- foreach(cluster = clusters, .packages = c("caret", "data.table"))%dopar%{
   pc <- sapply(train[, ..cluster], select_important, n_vars = 20, dat = train[, PC1:PC110]);
   pc <- sort(table(pc), decreasing = TRUE);
   names(pc)[1:length(pc) -1]; #Discart last predictor to avoid overfit
}
 
#Add months as additional predictors
predictors <- sapply(predictors, append, months_of_year);

#Add additional predictors from additional variables data preparation phase (EDA)
predictors <- sapply(predictors, append, predictors_add);

# Hyperparameters optimization per cluster

hyperparameters <- foreach(cluster = 1:length(clusters), 
                           .packages = c("e1071", "data.table", "foreach", "doParallel"))%dopar%{
  grid_search(clusters[[cluster]], predictors[[cluster]]);
}

#Train the model with the predictors and hyperparamenters of the cluster
model_cluster_svm <-  foreach(cluster = 1:length(clusters), .combine = c)%:%
  foreach(station = clusters[[cluster]], .packages = c("e1071", "data.table"))%dopar%{
    
    predictors_cluster = predictors[[cluster]];

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

mae_test <- round(mean(as.matrix(abs(errors_test))), 5); #2358701


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
data_predict <- data_prepared[5114:6909,]
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

write.csv(submission, file = "./files/submission_cluster_svm_20200521.csv", row.names = FALSE)

