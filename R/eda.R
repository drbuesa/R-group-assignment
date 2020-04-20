#packages load up
install.packages("data.table");
library(data.table);

install.packages("dplyr");
library(dplyr);

install.packages("ggplot2");
library(ggplot2);

install.packages("Amelia");
library(Amelia);

install.packages("mice");
library(mice);

install.packages("outliers");
library(outliers);

install.packages("caret");
library(caret);

install.packages("leaflet");
library(leaflet);

install.packages("maps");
library(maps)

install.packages("skimr")
library(skimr)

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

#Load datasets 
folder_path <- "../project/files/";
data <- readRDS(file.path(folder_path, "solar_dataset.RData"));
stations <- fread(file.path(folder_path, "station_info.csv"));
vars <- readRDS(file.path(folder_path, "additional_variables.RData"));

#quick view of datasets data and vars dimensions
dim(data)
dim(stations)
dim(vars)

### Date 
colnames(data)[1]

### Station names
colnames(data)[2:99]

### Principal components
colnames(data)[100:456]

### Last row with information 5113
which(data$Date == '20071231');

#Validation technique to see if +90% of the data in the columns is unique.
#In case of vars (additional_variables) it is found that there are columns that do not fulfill the condition of 
#up to 90% unique values.
constant_variables <- function(x,threshold=0.9){
  n_unique<- table(sapply(x,function(x){length(unique(x))>threshold*length(x)}));
  accepted<-identical(as.numeric(n_unique),as.numeric(ncol(x)))
  return(accepted)
}
constant_variables(data[1:5113,2:99]);
constant_variables(stations);
constant_variables(vars);

#Validation technique to find the specific column names for dataset vars (additional_variables) 
#where there are less than 60% unique values
constant_variables2 <- function(x,threshold=0.6){
  n_unique_values <- sapply(x, function(x){length(unique(x))>threshold*length(x)});
  constant_variables <- names(n_unique_values)[n_unique_values == FALSE];
  return(constant_variables);
}
constant_var_2 <- constant_variables2(vars[,2:101]);
constant_var_2;
length(constant_var_2)==ncol(vars[,2:101]);

#Validation technique to find missing values in the data sets. NAs
#It is recognized that no NAs exist both for data (solar_dataset) and stations (stations_info)
# Up to 7% NAs exist for vars (additional_variables dataset)
sum(is.na(data[1:5113,2:99])) 
sapply(data[1:5113,2:99], function(x){sum(is.na(x))}); 

sum(is.na(stations)) 
sapply(stations, function(x){sum(is.na(x))}); 

sum(is.na(vars[,2:101]));
sapply(vars[,2:101], function(x){sum(is.na(x))}); 
sapply(vars[,2:101], function(x){100*(sum(is.na(x)))/length(x)});
as.numeric(sort(sapply(vars[,2:101], function(x){100*(sum(is.na(x)))/length(x)}),decreasing = TRUE)[1]);

# After giving a quick overview check on vars (additional_variables dataset) it is recognized that a large 
# amount of values as zero exist. There are columns with up to 76% values as zero (column V4233)
sort(sapply(vars[,2:101], function(x){100*(sum(x==0,na.rm = TRUE))/length(x)}),decreasing = TRUE)[1];

#calculate the total % considering both conditions NAs and zero values. Up to 78% columns (column V2793)
sapply(vars[,2:101], function(x){100*(sum(x==0,na.rm = TRUE)+sum(is.na(x)))/length(x)});
as.numeric(sort(sapply(vars[,2:101], function(x){100*(sum(is.na(x))+sum(x==0,na.rm = TRUE))/length(x)}),decreasing = TRUE))[1];

#Lets do the same for rows/dates in the dataset.(team to think on what to do based on the result)
apply(vars[,2:101],MARGIN=1, function(x){100*(sum(x==0,na.rm = TRUE)+sum(is.na(x)))/length(x)})
as.numeric(sort(apply(vars[,2:101],MARGIN=1, function(x){100*(sum(x==0,na.rm = TRUE)+sum(is.na(x)))/length(x)}),decreasing = TRUE))[1];

#TEAM used skimr library to get a high overview in the data for var.
#see blog: https://www.littlemissdata.com/blog/simple-eda
skim(vars)

#lets aim to fill up the missing values using Amelia package.
missmap(vars)

# calling to amelia function; m=3, this parameter is the number of data set results generated through Amelia.
#after Amelia is used we get 0% missing values.
View(vars)
completed_data <- amelia(vars, m=3, idvars = c("Date"));

#Amelia package recognizes that certain variablesV7834, V1353, V5706, V1386, V6394, V5673, V4266, V7865, V4953, V505,
#V4234, V4233, V345, V6985, V1225, V2073, V1785, V1817, V377, V6137, V6825, V2074, V4697, V3977 are perfectly collinear
#with another variable in the data. That said, these columns can be removed from vars (team to validate)
vars_cleansed_amelia<- (completed_data$imputations$imp1 + completed_data$imputations$imp2+ completed_data$imputations$imp3)/3;
as.numeric(sort(sapply(vars_cleansed_amelia, function(x){100*(sum(is.na(x)))/length(x)}),decreasing = TRUE)[1]);
missmap(vars_cleansed_amelia)

#lets aim to fill up the missing values alternatively using Mice package.
results<- mice(vars, m=3);
vars_cleansed_mice <- (complete(results, 1) + complete(results, 2) + complete(results, 3))/3;
as.numeric(sort(sapply(vars_cleansed_mice, function(x){100*(sum(is.na(x)))/length(x)}),decreasing = TRUE)[1]);
missmap(vars_cleansed_mice)

# Function for removal of redundant variables from dataset
remove_redundant <- function(correlations,redundant_threshold){
  redundancy<-apply(correlations,2,function(x){which(x>redundant_threshold)});
  redundancy<-redundancy[which(sapply(redundancy,length)>1)]
  
  redundant_variables<-c();
  for (i in redundancy){
    imp<-sort(correlations[1,i],decreasing = TRUE);
    redundant_variables<-c(redundant_variables,names(imp)[2:length(i)])
  }
  redundant_variables<-unique(redundant_variables);
  return(redundant_variables);
} 

remove_irrelevant<-function(correlations,irrelevant_threshold, target){
  index <- which(target == colnames(correlations));
  
  #Function for removal of irrelevant variables from dataset
  relevance<-correlations[index,-index];
  irrelevant_variables<-names(relevance)[is.na(relevance) | relevance<irrelevant_threshold];
  return(irrelevant_variables);
}

#Usage of the functions
cors <- abs(cor(vars_cleansed_mice));
target <- "#we need to specify the target variable"; ##VANIA - I don't understand the idea here

redundant_vars <- remove_redundant(cors, 0.9);
vars_cleansed_mice[, -redundant_vars, with= F]

irrelevant_vars <- remove_irrelevant(cors, 0.5, target);
vars_cleansed_mice[, -irrelevant_vars, with= F]


#Analysis for solar_data set PCAs to see where the Percentage of Explained Variance is more relevant.
var <- sapply(data[,100:456], var);
prop_var <- var / sum(var);

ggplot(data.frame(prop_var[1:9]),
       aes(x = 1:9 , y = prop_var[1:9])) +
  geom_col(width = 0.7) +
  geom_line(col = "blue") +
  theme_bw() +
  labs(x = "Principal Component",
       y = "Percentage of Explained Variance")

barplot(prop_var)
plot(prop_var[1:10], type = "lines")

#80% of the Variance is explained with the first 9 PCAs
sum(prop_var[1:9])

prop_var_acum <- cumsum(prop_var)
which(prop_var_acum >= 0.8)[1] #Here we can add a slider to choose the threshold

#Important variables.Dimensionality reduction is commonly performed in machine learning projects for 
#computational and/or model accuracy optimization.Use filterVarImp() of caret library to select the top 5 most 
#relevant variables
library(caret);

select_important<-function(y, n_vars, dat){
  varimp <- filterVarImp(x = dat, y=y, nonpara=TRUE);
  varimp <- data.table(variable=rownames(varimp),imp=varimp[, 1]);
  varimp <- varimp[order(-imp)];
  selected <- varimp$variable[1:n_vars];
  return(selected);
}

test <- data[1:5113,]
select_important(y = test$HOOK, n_vars = 5, test[,100:109]);

variables <- sapply(test[,2:99], select_important, n_vars = 5, dat = test[,100:109])
table(variables)

#Check multicollinearity  --- 
res <- cor(test[,100:109]);
round(res, 2)
plot(test[,100:109])

#use of library PerformanceAnalytics to check correlation between PCAs
library("PerformanceAnalytics")
chart.Correlation(test[,100:109], histogram=TRUE, pch=19)

# Correlations between Predictors and Response. Heatmap ---
col<- colorRampPalette(c("blue", "white", "red"))(20)
res <- cor(test[,2:99], test[,100:109]);
round(res, 2)
heatmap(x = res, col = col, symm = F)

#GGPLOT ---
library(ggplot2)
ggplot(test, aes(x = PC1, y = ACME)) +
  geom_point(col = "blue")

#Density
plot(density(test$ACME), col = "blue", main = "Density function ACME");

#Maps 
mapStates = map("state", fill = TRUE, plot = FALSE)

m <- leaflet(data = mapStates) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
  addCircles(data = stations, lng = ~elon, lat = ~nlat, popup = ~stid, color = 'red');
m


###ADDISON EDA (Filling-in the missing values)

data$Date <- as.Date(data$Date, format="%Y%m%d") #formatting date
vars$Date <- as.Date(vars$Date, format="%Y%m%d") #formatting date

vars2<-vars[1:6909,2:101] #excluding the date in the dataset in which EDA will be performed

is.na(vars2) #check null values

sapply(vars2, function(x) sum(is.na(x))) #check how many are null values in each column

install.packages("imputeTS") #using imputeTS package to fill-in null values
library(imputeTS)

vars_no_null<-na_mean(vars2,option='median') #filling-in NAs with median (didn't use mean so that data won't be affected by outliers)

sapply(vars_no_null, function(x) sum(is.na(x))) #double-check if vars_no_null doesn't contain any missing values 

t(sapply(vars_no_null, summary)) #performing EDA (basic stats) to the clean dataset without null values

###ADDISON EDA (Outlier Detection/Removal/Correction) 
###Reference Link: https://www.r-bloggers.com/a-new-way-to-handle-multivariate-outliers/

install.packages("psych")
library(psych)

outlier(vars_no_null) #Q-Q plot of mahalanobis values versus the quantiles of the 100 columns from psych package

#calculation of mahalanobis 
MD <- mahalanobis(vars_no_null[, 1:100], colMeans(vars_no_null[, 1:100]), cov(vars_no_null[, 1:100])) 

alpha <- .001 #k degrees of freedom for chi-square computation to set cut-off score for outliers. We can adjust the alpha based on how many outliers we want to identify.
cutoff <- (qchisq(p = 1 - alpha, df = ncol(vars_no_null))) #cutoff score using chi-square
outliers <- which(MD > cutoff) #identified outliers are 800 (11.5% of the dataset)
excluded <- outliers #identified outliers to be excluded in the dataset (I have doubt if we need to exclude outliers. Option is to just scale the dataset.)
vars_no_null_clean <- vars_no_null[-excluded, ]
dim(vars_no_null_clean) #6,109 out of 6,909 have been retained (11.5% of the dataset were removed.)

outlier(vars_no_null_clean) #checking Q-Q plot of mahalanobis values to see the improvement

boxplot(vars_no_null) #we can also check the boxplot of the original dataset with outliers
boxplot(vars_no_null_clean) #versus the boxplot of the clean dataset without outliers

t(sapply(vars_no_null_clean, summary)) #performing EDA (basic stats) to the clean dataset without null values and without outliers

###ADDISON EDA (Data Scaling - Subtracting mean and dividing by SD) 

vars_no_null_scaled <- as.data.frame(scale(vars_no_null)) #Scaling the data with outliers.
vars_no_null_clean_scaled <- as.data.frame(scale(vars_no_null_clean)) #Scaling the data without outliers. Option if we remove outliers.

boxplot(vars_no_null_scaled) 
boxplot(vars_no_null_clean_scaled) #option if we decide to remove outliers.

#VANIA EDA ON STATIONS 



