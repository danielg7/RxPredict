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

click <- NA
rxData <- NA

source("functions.R")

shinyServer(function(input, output, session) {
  
  
   
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(-93.65, 39, zoom = 4) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)})
  
  observeEvent(input$map_click,{
    leafletProxy('map') %>% clearMarkers()
    click <<- input$map_click
    if(is.null(click))
      return()
    leafletProxy('map')%>%addMarkers(lng = click$lng, lat = click$lat)
  })
  
  observeEvent(input$button,{
    if(is.null(click))
      return()
    
    withProgress(message = 'Downloading data...', value = 0, {
    
    getData <- latlongReturnDF(lat = click$lat, long = click$lng, unitConvert = TRUE)
    interpData <- interpolateTime(getData,type = "spline")
    interpData$dt <- with_tz(interpData$dt, tzone = input$tz_input)
    interpData2 <- fxn_firebehavior(interpData, input$fuelModel,LH = input$liveHFM,LW = input$liveWFM)
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
    
    
    })
    updateTabsetPanel(session, "nav",selected = "Output")
  })
  
  output$rxPlot <- renderPlot({
    validate(
      need(input$button, 'Please select a location and press the button!'))
    outRxPlot <- rxPlot(df = rxData)
    outRxPlot
    })
})

