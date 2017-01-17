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
mychoices <- c("SWT02 - Fishing Survey System", "SWT05 - Ecology Research Programme",
               "SWT12 - Mnemiopsis", "SWT17 - ESM2")
mymonths <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
meanr <- function(x) mean(x, na.rm = T)

myfilter <- function(mydata, latrange, lonrange, nb){
  names(mydata) <- tolower(names(mydata))
  if(length(grep(names(mydata), pattern = "lat"))) names(mydata)[grep(names(mydata), pattern = "lat")] <- "lat"
  else{print("No latitude data can be found!"); stop()}
  
  if(length(grep(names(mydata), pattern = "lon"))) names(mydata)[grep(names(mydata), pattern = "lon")] <- "lon"
  else{print("No longitude data can be found!"); stop()}
  mydata <- mydata[, c("lat", "lon")]
  mydata <- mydata[mydata$lat > latrange[1] & mydata$lat < latrange[2] & mydata$lon > lonrange[1] & mydata$lon < lonrange[2], ]
  mydata[sample(c(1:nrow(mydata)),size = nb, replace = F), ]
}

ui <- fluidPage(
  
  h2("Spatial distribution of CEFAS temperature data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "CEFAS temperature data set", choices = mychoices),
      sliderInput("sliderlonrange1", label = "Longitudinal range", min = -20, max = 20, value = c(-10, 10)),
      sliderInput("sliderlatrange1", label = "Latitudinal range", min = 40, max = 80, value = c(50, 70)),
      sliderInput("maxnb", label = "Max number of points", min = 100, max = 500000, value = 1000),
      width = 4
    ),
    
    mainPanel(
      leafletOutput("mymap"))),
  
  p(),
  
  h2("Smoothed time series of CEFAS temperature data"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("slidertimerange", label = "Time range", min = 1950, max = 2016, value = c(1980, 2010)),
      sliderInput("sliderlonrange2", label = "Longitudinal range", min = -20, max = 20, value = c(2, 10)),
      sliderInput("sliderlatrange2", label = "Latitudinal range", min = 40, max = 80, value = c(50, 70)),
      selectInput("month", label = "Month", choices = mymonths),
      # sliderInput("myresolution", label = "Each time series corresponds to a square cell of", min = 0.5, max = 10, value = 1),
      sliderInput("mysmoother", label = "Smoothing", min = 0.1, max = 2, value = 0.5),
      width = 4
    ), 
    
    mainPanel(
      plotOutput("time_series"))),
  p()
)

server <- function(input, output, session) {
  
  points <- reactive({
    download_cefas_data(alldatasets$Id[grep(alldatasets$Name, pattern = substr(input$dataset, 1, 5))]) 
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldTerrain",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircles(data = myfilter(points(),  latrange = input$sliderlatrange1, lonrange = input$sliderlonrange1, nb = input$maxnb),
                 color = "black", fillColor = "blue")
  })
  
  output$time_series <- renderPlot({
    mydata <- points()
    mytimes <- strsplit(as.character(mydata$Time), split = " ")
    mytimes <- sapply(mytimes, function(x) x[1])
    mytimes <- strsplit(mytimes, split = "/", fixed = T)
    myears <- sapply(mytimes, function(x) x[3])
    mymonths <- sapply(mytimes, function(x) x[2])
    
    mydf <- data.frame(myears, mymonths, mydata$tC, mydata$Lat, mydata$Long)
    names(mydf) <- c("myears", "mymonths", "tC", "lat", "lon")
    
    mydf <- mydf[as.numeric(as.character(mydf$myears)) > input$slidertimerange[1] & 
                   as.numeric(as.character(mydf$myears)) < input$slidertimerange[2], ]
    mydf <- mydf[mydf$lon > input$sliderlonrange2[1] & mydf$lon < input$sliderlonrange2[2], ]
    mydf <- mydf[mydf$lat > input$sliderlatrange2[1] & mydf$lat < input$sliderlatrange2[2], ]
    
    mydf <- mydf[mydf$mymonths == input$month, ]
    mydf$myears <- as.numeric(as.character(mydf$myears))
    
    # breakx <- seq(min(floor(mydf$lon)), max(ceiling(mydf$lon)), by = input$myresolution)
    # breaky <- seq(min(floor(mydf$lat)), max(ceiling(mydf$lat)), by = input$myresolution)
    # cellx <- cut(mydf$lon, breaks = breakx, labels = breakx[1:(length(breakx) - 1)])
    # celly <- cut(mydf$lat, breaks = breaky, labels = breaky[1:(length(breaky) - 1)])
    # mycoord <- paste(cellx, celly, sep = "_")
    
    # res <- data.frame(tapply(mydf$tC, list(factor(mydf$myears), factor(mycoord)), mean))
    # res <- data.frame(year = as.numeric(rownames(res)), res)
    # res <- melt(res, id.vars = "year")
    # res <- tapply(mydf$tC, factor(mydf$myears), meanr)
    
    ggplot(data = mydf, aes(x = myears, y = tC)) +
      geom_point() +
      geom_smooth(span = input$mysmoother, se = TRUE) +
      labs(x = "Year", y = "Mean temperature") +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
            panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"))
  })
  
}

shinyApp(ui, server)
