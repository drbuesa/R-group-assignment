library(shiny)
library(data.table)
library(leaflet)
library(maps)
library(ggplot2)



# Load data

solar_dataset <- readRDS("../files/solar_dataset.RData");
stations <- fread("../files/station_info.csv");
vars <- readRDS("../files/additional_variables.RData");


### Station names
stationsNames <- colnames(solar_dataset)[2:99]
data <- solar_dataset[1:5113,2:99]

################## Define UI ############################
ui <- fluidPage(
  titlePanel("EDA - Solar Energy Prediction"),
  h4("May 22nd 2020"),
  
  sidebarLayout(
    sidebarPanel (
      
      selectInput(inputId = "station", label = "Choose Solar Stations:",
                  choices = stationsNames, selected = "ACME"),
      
      selectInput(inputId = "plot.type","Choose Plot Type:", 
                  list(Boxplot = "Boxplot", Histogram = "Histogram", Density = "Density")),
      
      img(src = "IE-HST.png", height = 200, width = 220)),
    
    
    # output              
    mainPanel(
      
      h3("Map"),
      leafletOutput("mymap"),
      
      h3("Summary"),
      verbatimTextOutput("sum"),
      
      
      h3(textOutput("caption")),
      plotOutput(outputId = "plot")
    )
  )
)

################### server #################

server <- shinyServer(function(input, output, session){
  
  compute_data <- reactive({
    if (length(input$station) > 0){
      data_var <- as.data.frame(data[, input$station, with = F]);
    } else {
      data_var <- data.frame();
    }
    return(data_var);
  })
  
  output$mymap <- renderLeaflet({
    lon <- stations[stid == input$station,]$elon
    ltt <- stations[stid == input$station,]$nlat
    stationid <- stations[stid == input$station,]$stid
    
    my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white');
    
    
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng = lon, lat = ltt, zoom = 5) %>%
      addAwesomeMarkers(lng = lon, lat = ltt, popup = stationid, icon = my_icon);
      #addCircles(data = stations, lng = lon, lat = ltt, popup = stationid, color = 'green');
    
  })
  
  
  output$sum_title <- renderText({
    ("Summary")
  })
  
  output$sum <- renderPrint({
    summary(compute_data())
  })
  
  output$caption<-renderText({
    switch(input$plot.type,
           "Boxplot"    =   "Boxplot",
           "Histogram" =    "Histogram",
           "Density"    =   "Density plot")
  })
  
  compute_plot <- reactive({
    if (length(input$station) > 0){
      if(input$plot.type == "Boxplot"){
        ggplot(compute_data(), aes(x = "", y = compute_data()[,input$station])) + geom_boxplot() + 
          labs(x = input$station, y = "Joules per square meter")
      }else if(input$plot.type == "Histogram"){
        ggplot(compute_data(), aes(x = compute_data()[,input$station])) + geom_histogram() + 
          labs(x = input$station, y = "Frecuency")
       }else{
         ggplot(compute_data(), aes(x = compute_data()[,input$station], y = ..scaled.. )) + geom_density() + 
           labs(x = input$station, y = "Scaled")
         }
      }
  })
  
  # Print plot
  output$plot <- renderPlot({
    compute_plot();
  })
  
}) 

shinyApp(ui = ui, server = server)
