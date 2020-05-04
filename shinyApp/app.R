pacman::p_load(shiny,modelr,tidyverse,randomForest,rpart,rpart.plot,data.table,dplyr,ggplot2,Amelia,mice,outliers,caret,leaflet,maps,skimr,PerformanceAnalytics)

# library(data.table);
# library(ggplot2);

folder_path <- "C:/Fidelis/IE_University/Programs/R Programming/project/";

data <- readRDS(file.path(folder_path, "solar_dataset.RData"));
stations <- fread(file.path(folder_path, "station_info.csv"));
vars <- readRDS(file.path(folder_path, "additional_variables.RData"));

var <- sapply(data[,100:456], var);
prop_var <- var / sum(var);

prop_var_acum <- cumsum(prop_var)


#############################################
#install.packages("shiny")
# library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
  label = "Choose number of PCAs",
  value = 10, min = 1, max = 357),
  plotOutput("ggplot")
  
)

server <- function(input, output, session) {
  output$ggplot <- renderPlot({
    ggplot(data.frame(prop_var_acum[1:input$num]),
           aes(x = 1:input$num , y = 100*prop_var_acum[1:input$num])) +
      geom_col(width = 0.7) +
      geom_line(col = "blue") +
      theme_bw() +
      labs(x = "Principal Component",
           y = "Percentage of Explained Variance")
  })
}

shinyApp(ui = ui, server = server)
