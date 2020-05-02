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

#Train 98 different models, pararellization can be performed later 

library(randomForest);

model <- list()
for (station in stationsNames){
  model[[station]] <- randomForest(y = train[, ..station], x = train[,PC1:PC7], 
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


############################# IMPROVE ACCURACY OF APPROACH 3 ############################


# Find the most correlated stations

correlations <- cor(data[, ..stationsNames])
sort(correlations["KENT",]) #Chose KENT because it is easy to find closest stations in the map

#Train a single model for KENT 
predictors <- c(paste0("PC", seq(1,7)), "BOIS", "HOOK", "GOOD") #With more than 3 the model overfits

model_kent <- randomForest(y = train[, KENT], x = train[, ..predictors], 
                           data = train)

predictions_kent <- predict(model_kent, newdata = test)
errors_kent <- predictions_kent - test$KENT
mae_kent <- round(mean(abs(errors_kent)), 5); #1169406
mae_kent

# Importance, geographical distance and correlation are the same 
select_important(y = train$KENT, n_vars = 5, dat = train[, ..stationsNames][,-"KENT"])

#SVM 

library(e1071)
model_kent <- svm(KENT ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + BOIS + GOOD + HOOK, 
             data = train, 
             kernel="radial",
             cost = 100, epsilon = 0.1, gamma = 0.0001); #1090740 with Grid Search **BEST

predictions_kent <- predict(model_kent, newdata = test)
errors_kent <- predictions_kent - test$KENT
mae_kent <- round(mean(abs(errors_kent)), 5); 
mae_kent

#xgboost 
library(xgboost)

dtrain <- xgb.DMatrix(as.matrix(train[, ..predictors]), label = train[,KENT])
dtest <- xgb.DMatrix(as.matrix(test[, ..predictors]), label = test[,KENT])

w <- list(train = dtrain, test = dtest); #Watchlist

xgb_kent1 <- xgb.train(data = dtrain, booster = 'gbtree', nrounds = 500, max_depth = 3,
                       eval_metric = 'mae', eta = 0.05, watchlist = w,
                       early_stopping_rounds = 30)
#test-mae:1114932.000000 Can perform a Grid Search for nrounds, max_depth and eta

predictions_kent <- predict(xgb_kent1, newdata = dtest)
errors_kent <- predictions_kent - test$KENT
mae_kent <- round(mean(abs(errors_kent)), 5); #1132313 
mae_kent


# Grip search for best model parameters (eta = 0.05, nrounds = 500, max_depth = 3)

library(caret)

dval <- xgb.DMatrix(as.matrix(val[, ..predictors]), label = val[,KENT])

xgb_grid <- expand.grid(nrounds = c(500, 800), max_depth = c(3, 6, 8), 
                        eta = c(0.05, 0.1, 0.12, 0.135, 0.15 ),
                        gamma = 0,
                        colsample_bytree = 1,
                        min_child_weight =1,
                        subsample = 0.3)

tr_ctrl <- trainControl(method = 'repeatedcv', search = 'grid', number = 10, repeats = 5)

xgb_train <- train(x = dval, y = val[ ,KENT], eval_metric = 'mae', trControl = tr_ctrl,
                   maximize = F, method = 'xgbTree', tuneGrid = xgb_grid[1:7,])


################# KAGGLE PREDICTIONS  - USE THE BEST MODEL #################################

#Train 98 different models

library(e1071);
library(foreach)
library(doParallel)

# Start cluster
stopImplicitCluster();
registerDoParallel(cores = detectCores());

model <- foreach(station = stationsNames, .packages = c("e1071", "data.table"))%dopar%{
        svm(y = train[, ..station], x = train[, PC1:PC7], 
        data = rbind(train, val), 
        kernel="radial",
        cost = 100, epsilon = 0.1, gamma = 0.0001);
}

#Get model predictions for multiple models (ST approach)
#When not using formula svm expects newdata to have excatly the SAME PREDICTORS
predictions_test <- sapply(model, predict, newdata = test[, PC1:PC7]); 

#Get errors
errors_test <- predictions_test - test[, ..stationsNames];

#Compute Metric (MAE)

mae_test <- round(mean(as.matrix(abs(errors_test))), 5); #2665900

#Build the summision
predictions <- data_original[5114:6909,]
dim(predictions)

predictions_kaggle <- as.data.table(sapply(model, predict, newdata = predictions[, PC1:PC7]))

submission <- predictions[, Date:WYNO]
predictions_kaggle -> submission[, 2:99]

write.csv(submission, file = "./files/submission.csv", row.names = FALSE)



###ADDISON 
# ACME Create a Random Forest model with default parameters

model2 <- randomForest(y = train[, ACME], x = train[,100:456], importance = TRUE)
model2

imp <- importance(model2)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]

varImpPlot(model2)

predictors <- c("PC1","PC2","PC5","PC7","PC3","PC10","PC15","PC11","PC12","PC8","PC22","PC26","PC19",
                "PC14","PC44")


# Fine tuning parameters of Random Forest model


model_acme <- randomForest(y = train[, ACME], x = train[, ..predictors], 
                           data = train, ntree = 500, mtry = 11, importance = TRUE)

predictions_acme <- predict(model_acme, newdata = test)
errors_acme <- predictions_acme - test$ACME
mae_acme <- round(mean(abs(errors_acme)), 5); #2482608
mae_acme