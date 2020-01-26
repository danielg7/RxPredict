#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinyBS)
library("bsplus")
library("lutz")

click <- NA
rxData <- NA
lat <- NA
long <- NA

source("functions.R")

shinyServer(function(input, output, session) {
  
  
   
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(-93.65, 39, zoom = 4) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)})
  
  observeEvent(input$map_click,{
    leafletProxy('map') %>% clearMarkers()
    click <<- input$map_click
    lat <<- click$lat
    long <<- click$lng
    
    if(is.null(click))
      return()
    leafletProxy('map')%>%addMarkers(lng = long, lat = lat)
  })
  
  observeEvent(input$button,{
    if(is.null(click))
      return()
    
    withProgress(message = 'Downloading data...', value = 0, {
    
    getData <- latlongReturnDF(lat = lat, long = long, unitConvert = TRUE)
    
    incProgress(1/5,message = "Interpolating data...")
    
    interpData <- interpolateTime(getData,type = "spline")
    
    incProgress(2/5,message = "Calculating timezone...")
    
    tz_fromInput <- tz_lookup_coords(lat, long, method = "accurate")
    
    interpData$dt <- with_tz(interpData$dt, tzone = tz_fromInput)
    
    print(input$fuelModel)
    
    incProgress(3/5,message = "Calculating fire behavior...", detail = "(This may take a while.)")
    
    interpData2 <- fxn_firebehavior(interpData, input$fuelModel,LH = input$liveHFM,LW = input$liveWFM)
    
    incProgress(4/5,message = "Calculating prescription...")
    
    rxData <<- prescription(df = interpData2,
                           tempHi = input$rxTempInput[2],
                           tempLo = input$rxTempInput[1],
                           windHi = input$rxWindInput[2],
                           windLo = input$rxWindInput[1],
                           rhHi = input$rxRHInput[2],
                           rhLo = input$rxRHInput[1],
                           ffmHi = input$rxFFMInput[2],
                           ffmLo = input$rxFFMInput[1],
                           flHi = input$rxFLInput[2],
                           flLo = input$rxFLInput[1])
    
    incProgress(5/5,message = "Done!")
    
    
    
    })
    updateTabsetPanel(session, "nav",selected = "Output")
  })
  
  output$rxPlot <- renderPlot({
    validate(
      need(input$button, 'Please select a location and press the button!'))
    outRxPlot <- rxPlot(df = rxData,
                        lat = lat,
                        long = long,
                        rhHi = input$rxRHInput[2],
                        rhLo = input$rxRHInput[1],
                        wsHi = input$rxWindInput[2],
                        wsLo = input$rxWindInput[1],
                        tempHi = input$rxTempInput[2],
                        tempLo = input$rxTempInput[1],
                        ffmHi = input$rxFFMInput[2],
                        ffmLo = input$rxFFMInput[1],
                        flHi = input$rxFLInput[2],
                        flLo = input$rxFLInput[1])
    outRxPlot
    })
})

