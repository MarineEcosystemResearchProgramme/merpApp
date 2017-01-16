library(shiny)
library(leaflet)
library(merpWS)
library(merpData)
library(jsonlite)
library(magrittr)
library(ggplot2)
library(ggmap)
library(reshape2)

alldatasets <- get_cefas_datasets()
alldatasets <- alldatasets$to_download
param <- alldatasets$Name

myfilter <- function(mydata, latrange, lonrange){
  names(mydata) <- tolower(names(mydata))
  if(length(grep(names(mydata), pattern = "lat"))) names(mydata)[grep(names(mydata), pattern = "lat")] <- "lat"
  else{print("No latitude data can be found!"); stop()}
  
  if(length(grep(names(mydata), pattern = "lon"))) names(mydata)[grep(names(mydata), pattern = "lon")] <- "lon"
  else{print("No longitude data can be found!"); stop()}
  mydata <- mydata[, c("lat", "lon")]
  mydata <- mydata[mydata$lat > latrange[1] & mydata$lat < latrange[2] & mydata$lon > lonrange[1] & mydata$lon < lonrange[2], ]
  # mydata[sample(size = nb, x = 1:nrow(mydata), replace = F), ]
  mydata
}

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "CEFAS temperature data set", choices = paste("SWT0", c(9, 4, 2), sep = "")),
      sliderInput("sliderlonrange1", label = "Longitudinal range", min = -20, max = 20, value = c(-10, 10)),
      sliderInput("sliderlatrange1", label = "Latitudinal range", min = 40, max = 80, value = c(50, 70)),
      # sliderInput("mynumber", label = "number of data points to display", min = 1, max = 500, value = 10),
      width = 4
    ),
    
    mainPanel(
      leafletOutput("mymap"))),
  
  p(),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("slidertimerange", label = "Time range", min = 1950, max = 2016, value = c(1990, 2010)),
      sliderInput("sliderlonrange2", label = "Longitudinal range", min = -20, max = 20, value = c(-10, 10)),
      sliderInput("sliderlatrange2", label = "Latitudinal range", min = 40, max = 80, value = c(50, 70)),
      sliderInput("myresolution", label = "Spatial unit", min = 0.5, max = 10, value = 1),
      width = 4
    ),
    
    mainPanel(
      plotOutput("time_series"))),
  p()
)

server <- function(input, output, session) {
  
  points <- reactive({
    download_cefas_data(alldatasets$Id[grep(alldatasets$Name, pattern = input$dataset)]) 
    # myfilter(res)
    # attempt at finding coordinates
  })
  
  # http://leaflet-extras.github.io/leaflet-providers/preview/
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      # addProviderTiles("Stamen.TonerLite",
      #                  options = providerTileOptions(noWrap = TRUE)
      addProviderTiles("Esri.WorldTerrain",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # addMarkers(data = myfilter(points(), nb = input$mynumber))
      addCircles(data = myfilter(points(),  latrange = input$sliderlatrange1, lonrange = input$sliderlonrange1),
                 color = "black", fillColor = "blue")
    # could have addCircles instead?
  })
  
  output$time_series <- renderPlot({
    mydata <- points()
    # mydata <- download_cefas_data(alldatasets$Id[grep(alldatasets$Name, pattern = "SWT09")]) 
    mytimes <- strsplit(as.character(mydata$Time), split = " ")
    mytimes <- sapply(mytimes, function(x) x[1])
    mytimes <- strsplit(mytimes, split = "/", fixed = T)
    mytimes <- sapply(mytimes, function(x) x[3])
    mydf <- data.frame(mytimes, mydata$tC, mydata$Lat, mydata$Long)
    names(mydf) <- c("mytimes", "tC", "lat", "lon")
    # for(i in 1:ncol(mydf)) mydf[, i] <- as.numeric(mydf[, i])
    mydf <- mydf[as.numeric(as.character(mydf$mytimes)) > input$slidertimerange[1] & 
                   as.numeric(as.character(mydf$mytimes)) < input$slidertimerange[2], ]
    mydf <- mydf[mydf$lon > input$sliderlonrange2[1] & mydf$lon < input$sliderlonrange2[2], ]
    mydf <- mydf[mydf$lat > input$sliderlatrange2[1] & mydf$lat < input$sliderlatrange2[2], ]
    
    breakx <- seq(min(floor(mydf$lon)), max(ceiling(mydf$lon)), by = input$myresolution)
    breaky <- seq(min(floor(mydf$lat)), max(ceiling(mydf$lat)), by = input$myresolution)
    cellx <- cut(mydf$lon, breaks = breakx, labels = breakx[1:(length(breakx) - 1)])
    celly <- cut(mydf$lat, breaks = breaky, labels = breaky[1:(length(breaky) - 1)])
    mycoord <- paste(cellx, celly, sep = "_")
    
    res <- data.frame(tapply(mydf$tC, list(factor(mydf$mytimes), factor(mycoord)), mean))
    res <- data.frame(year = as.numeric(rownames(res)), res)
    res <- melt(res, id.vars = "year")
    
    ggplot() + geom_line(data = res, aes(x = year, y = value, col = variable)) +
      theme(legend.position = "none")
  })
  
}

shinyApp(ui, server)