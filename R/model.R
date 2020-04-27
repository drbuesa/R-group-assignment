library(data.table);


#Load datasets 


folder_path <- "./files/";

data_original <- readRDS(file.path(folder_path, "solar_dataset.RData"));
stations <- fread(file.path(folder_path, "station_info.csv"));

dim(data_original)
dim(stations)

colnames(data)[1] ### Date 

stationsNames <- colnames(data_original)[2:99] ### Station names

colnames(data)[100:456] ### Principal components

which(data$Date == '20071231'); ### Last row with information 5113

set.seed(14); 

#Remove nulls
data <- data_original[1:5113,] 

#Add total sum of energy production in stations (only for total_energy model) ***NOT USE***
data <- cbind(data, total = rowSums(data[,2:99]))

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

######################### APPROACH 1 - CLUSTERING OF STATIONS #################################

head(stations)

#### Clusters using geographical info in Stations ### *** NOT USED ***

normStations <- scale(stations[,2:4])

#Perform a number of clusters K = 98 / 2
totalwss <- c()
for (k in 1:(nrow(normStations)/2)){
  result <- kmeans(normStations, k)  
  totalwss <- c(totalwss, result$tot.withinss)
}

#Decide the right number of clusters 
plot(totalwss[1:10], type = "line")


#### Clusters using energy production in the Stations ### 

normSolar <- scale(data[,2:99])
normSolar_transpose <- as.data.frame(t(as.matrix(normSolar)))

#Perform a number of clusters K = 20
totalwss <- c()
for (k in 1:20){
  result <- kmeans(normSolar_transpose, k)  
  totalwss <- c(totalwss, result$tot.withinss)
}

#Plot to decide the optimum number of clusters 
plot(totalwss[1:15], type = "line")
numcl <- 8

#Perform final clustering with the optimum number k
cl_geo <- kmeans(normStations, 4)
cl_ene <- kmeans(normSolar_transpose, numcl)

#Extract the clusters results to a list
clusters <- list()
for (i in 1:numcl){
  clusters[[i]] <- names(cl_ene$cluster)[cl_ene$cluster == i]
}


#Important variables analysis per cluster

library(caret);

select_important<-function(y, n_vars, dat){
  varimp <- filterVarImp(x = dat, y=y, nonpara=TRUE);
  varimp <- data.table(variable=rownames(varimp),imp=varimp[, 1]);
  varimp <- varimp[order(-imp)];
  selected <- varimp$variable[1:n_vars];
  return(selected);
}

select <- unlist(clusters[3])
pc <- sapply(train[, ..select], select_important, n_vars = 5, dat = train[,100:109])
sort(table(pc), decreasing = TRUE)



############################### APPROACH 2 - TOTAL PRODUCTION #################################

# 1) Predict total energy using model_total_energy

# 2) Calculate weights matrix using train and val 

weights <- rbind(train, val)

weights <- weights[,2:99] / weights$total
mean_weights <- as.data.table(sapply(weights, mean))
mean_weights <- transpose(mean_weights)
colnames(mean_weights) <- stationsNames

#Split the prediction for total energy among the stations based on their weights

predictions_test_w <- data.table()

for (i in 1:length(predictions_test)){
  predictions_test_w <- rbind(predictions_test_w, predictions_test[i] * mean_weights)
}

#Get errors
errors_train <- predictions_train - train[,2:99];
errors_test <- predictions_test_w - test[,2:99];
#Compute Metric (MAE)

mae_train <- round(mean(as.matrix(abs(errors_train))), 5);
mae_test <- round(mean(as.matrix(abs(errors_test))), 5);

################# APPROACH 3 - MULTIPLE SINGLE TARGET MODELS #################################

#Train 98 different models, pararellization and model optimization can be performed later 

library(randomForest);

model <- sapply(train[, 2:3], randomForest, x = train[,100:106], data =train);

model <- list()
for (station in stationsNames){
  model[[station]] <- randomForest(y = train[[station]], x = train[,100:106], 
                                   data = train)
}

#Get model predictions for multiple models (ST approach)
predictions_train <- predict(model[[1]], newdata = train); #Only one model 
predictions_train <- sapply(model, predict, newdata = train); #All models at the same time
predictions_test <- sapply(model, predict, newdata = test);

#Get errors
errors_train <- predictions_train - train[,2:99];
errors_test <- predictions_test - test[,2:99];
#Compute Metric (MAE)

mae_train <- round(mean(as.matrix(abs(errors_train))), 5); #1248521
mae_test <- round(mean(as.matrix(abs(errors_test))), 5); #2775327
