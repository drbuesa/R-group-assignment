# Packages installation
#install.packages(
  c("data.table", "dplyr", "ggplot2", "Amelia", "mice", "outliers", 
    "caret", "leaflet", "maps", "skimr");

library(data.table);
library(dplyr);
library(ggplot2);
library(Amelia);
library(mice);
library(outliers);
library(caret);
library(leaflet);
library(maps);
library(skimr);
library(psych);
  
#Load datasets 

folder_path <- "/Users/drodriguez45/Documents/GitHub/R-group-assignment/";

data <- readRDS(file.path(folder_path, "./files/solar_dataset.RData"));
stations <- fread(file.path(folder_path, "./files/station_info.csv"));
additional_vars <- readRDS(file.path(folder_path, "./files/additional_variables.RData"));

#Check datasets dimensions
dim(data); 
dim(stations);
dim(additional_vars);

### Date 
colnames(data)[1]

### Station names
stationsNames <- colnames(data)[2:99]

### Principal components
pc <- colnames(data)[100:456]

### Last row with information 5113
which(data$Date == '20071231');
data <- cbind(data, month = as.numeric(sapply(data$Date, substr, 5,6)));
data <- data[1:5113,]

###################################### ANALYSIS ON SOLAR DATASET ######################################

#EDA on solar_dataset

summary(data[, ..stationsNames])
sum(is.na(data))
boxplot(data[, ..stationsNames]) #boxplot 

#Plots by solar station 
plot(density(data$ACME), col = "blue", main = "Density function ACME");
plot(data$ACME, col = "blue", main = "ACME", type = "line"); #Clear seasonality per year

#Zoom on year 1994 as an example
ggplot(data[Date %in% "19940101":"19941231"]) +
  geom_point(aes(x = month, y = ACME, color = month)) +
  theme_bw() +
  labs(x = "Month of the year", y = "Solar Station Production")+
  scale_x_discrete(labels = 1:12, limits = c(1:12))+
  scale_color_gradient2(midpoint=6, low="blue", mid="orange",
                        high="blue")



#Maps 
mapStates = map("state", fill = TRUE, plot = FALSE)

m <- leaflet(data = mapStates) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
  addCircles(data = stations, lng = ~elon, lat = ~nlat, popup = ~stid, color = 'red');
m


#Principal Component Analysis to see the Percentage of Explained Variance

variance <- sapply(data[, ..pc], var);
prop_var <- variance / sum(variance);
#80% of the Variance is explained with the first 10 PCAs
prop_var_acum <- cumsum(prop_var);
which(prop_var_acum >= 0.8)[1];

ggplot() +
  geom_col(aes(x = 1:10 , y = prop_var[1:10]), width = 0.7) +
  geom_line(aes(x = 1:10 , y = prop_var_acum[1:10]), col = "blue") +
  theme_bw() +
  labs(x = "Principal Component", y = "Percentage of Explained Variance") +
  scale_x_discrete(labels = paste0("PC", seq(1,10)), limits = c(1:10))



#Important variables. Dimensionality reduction is commonly performed in machine learning projects for 
#computational and/or model accuracy optimization.
#Use filterVarImp() of caret library to select the top 5 most relevant variables

select_important<-function(y, n_vars, dat){
  varimp <- filterVarImp(x = dat, y=y, nonpara=TRUE);
  varimp <- data.table(variable=rownames(varimp),imp=varimp[, 1]);
  varimp <- varimp[order(-imp)];
  selected <- varimp$variable[1:n_vars];
  return(selected);
}

important_pc <- sapply(data[, ..stationsNames], select_important, n_vars = 5, dat = data[, PC1:PC10]);
table(important_pc);

#Check multicollinearity  --- By definition PC shoudn´t be correlated
correlation <- cor(data[, PC1:PC10]);
round(correlation, 2);
plot(data[, PC1:PC10]);

# Correlations between Predictors and Response. Heatmap ---

col<- colorRampPalette(c("blue", "white", "red"))(20)
res <- cor(data[, ..stationsNames], data[ ,PC1:PC10]);
round(res, 2)
heatmap(x = res, col = col, symm = F)

############################### ANALYSIS ON ADDITIONAL VARIABLES DATASET ######################################

#Validation technique to see if +90% of the data in the columns is unique.
#In case of additional_vars it is found that there are columns 
#that do not fulfill the condition of up to 90% unique values

constant_variables <- function(x, threshold=0.9){
  n_unique<- table(sapply(x, function(x){length(unique(x)) > threshold*length(x)}));
  accepted<-identical(as.numeric(n_unique),as.numeric(ncol(x)))
  return(accepted)
}

constant_variables(data);
constant_variables(stations);
constant_variables(additional_vars);

#Validation technique to find the specific column names for dataset vars (additional_variables) 
#where there are less than 60% unique values
constant_variables2 <- function(x,threshold=0.6){
  n_unique_values <- sapply(x, function(x){length(unique(x))>threshold*length(x)});
  constant_variables <- names(n_unique_values)[n_unique_values == FALSE];
  return(constant_variables);
}
constant_var_2 <- constant_variables2(additional_vars[, -"Date"]);
constant_var_2;
length(constant_var_2) == ncol(additional_vars[, -"Date"]);

#Validation technique to find missing values in the data sets. NAs
#It is recognized that no NAs exist both for data (solar_dataset) and stations (stations_info)
#Up to 7% NAs exist for additional_vars (additional_variables dataset)

sum(is.na(data)) 

sum(is.na(stations)) 

sum(is.na(additional_vars[, -"Date"]));
max(sapply(additional_vars[,-"Date"], function(x){100 * (sum(is.na(x))) / length(x)}));

# After giving a quick overview check on vars (additional_variables dataset) it is recognized that a large 
# amount of values as zero exist. All the columns have at least a 65,8% of zeros
sort(sapply(additional_vars[, -"Date"], function(x){100 * (sum(x == 0, na.rm = TRUE))/length(x)}), decreasing = TRUE);

#And at least a 71% of zeros and missing values
sort(sapply(additional_vars[, -"Date"], function(x){100 * (sum(is.na(x)) + sum(x == 0, na.rm = TRUE)) / length(x)}), decreasing = TRUE);


#TEAM used skimr library to get a high overview in the data for additional_vars
#see blog: https://www.littlemissdata.com/blog/simple-eda

skim(additional_vars);

################################# ANNEX - ADDITIONAL ANALYSIS ################################

#Fill up the missing values using Amelia package.
missmap(additional_vars);

# calling to amelia function; m=3, this parameter is the number of data set results generated through Amelia.
#after Amelia is used we get 0% missing values.

completed_data <- amelia(additional_vars[, -"Date"], m=3);

#Amelia package recognizes that certain variablesare perfectly collinear
#with another variable in the data. That said, these columns can be removed from vars

vars_cleansed_amelia <- (completed_data$imputations$imp1 + completed_data$imputations$imp2 + completed_data$imputations$imp3)/3;

exclude <- list("V7834", "V1353", "V5706", "V1386", "V6394", "V5673", "V4266", "V7865", 
                "V4953", "V505", "V4234", "V4233", "V345", "V6985", "V1225", "V2073",
                "V1785", "V1817", "V377", "V6137", "V6825", "V2074", "V4697", "V3977");

vars_cleansed_amelia <- vars_cleansed_amelia[, -exclude, With = F]

sum(is.na(vars_cleansed_amelia))

missmap(vars_cleansed_amelia)

#Same can be done using Mice package
results<- mice(additional_vars, m=3);
vars_cleansed_mice <- (complete(results, 1) + complete(results, 2) + complete(results, 3))/3;
as.numeric(sort(sapply(vars_cleansed_mice, function(x){100*(sum(is.na(x)))/length(x)}),decreasing = TRUE)[1]);
missmap(vars_cleansed_mice)

summary(vars_cleansed_amelia)

#Outlier Detection/Removal/Correction) 
#Reference Link: https://www.r-bloggers.com/a-new-way-to-handle-multivariate-outliers/


outlier(vars_cleansed_amelia) #Q-Q plot of mahalanobis values versus the quantiles of the 100 columns from psych package


install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")



#Calculation of mahalanobis 
MD <- mahalanobis(vars_cleansed_amelia, colMeans(vars_cleansed_amelia), cov(vars_cleansed_amelia)) 

alpha <- .001 #k degrees of freedom for chi-square computation to set cut-off score for outliers. We can adjust the alpha based on how many outliers we want to identify.
cutoff <- (qchisq(p = 1 - alpha, df = ncol(vars_cleansed_amelia))) #cutoff score using chi-square
outliers <- which(MD > cutoff) #identified outliers are 800 (11.5% of the dataset)

#Data Scaling - Subtracting mean and dividing by SD 

vars_cleansed_amelia_scaled <- as.data.table(scale(vars_cleansed_amelia)) #Scaling the data with outliers.

#Function for removal of redundant variables from dataset
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

#Remove redundant variables
cors <- abs(cor(vars_cleansed_amelia_scaled));
redundant_threshold <- 0.9

redundant_vars <- remove_redundant(cors, redundant_threshold);
additional_vars_clean <- vars_cleansed_amelia_scaled[, -redundant_vars, with= F]

important_add_vars <- sapply(data[, ..stationsNames], select_important, n_vars = 20, dat = additional_vars_clean[1:5113, V6409:V3977]);
table(important_add_vars);
sort(table(important_add_vars), decreasing = T)
predictors_add <- names(sort(table(important_add_vars), decreasing = T))[1:5]
