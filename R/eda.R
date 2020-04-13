library(data.table);
library(ggplot2);

#Load datasets 
#Test GitHub

folder_path <- "/Users/drodriguez45/Documents/GitHub/R-group-assignment/files/";

data <- readRDS(file.path(folder_path, "solar_dataset.RData"));
stations <- fread(file.path(folder_path, "station_info.csv"));
vars <- readRDS(file.path(folder_path, "additional_variables.RData"));


dim(data)
dim(vars)

### Date 

colnames(data)[1]

### Station names

colnames(data)[2:99]

### Principal components 

colnames(data)[100:456]

###▌ Last row with information 5113
which(data$Date == '20071231');

#PCA Analysis 

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

prop_var_acum <- cumsum(prop_var)
which(prop_var_acum >= 0.8)[1] #Here we can add a slider to choose the threshold

#Important variables 

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
install.packages("leaflet");
install.packages("maps")
library(leaflet);
library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)

m <- leaflet(data = mapStates) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
  addCircles(data = stations, lng = ~elon, lat = ~nlat, popup = ~stid, color = 'red');
m 
