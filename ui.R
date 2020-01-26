#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(firebehavioR)
library(shinyBS)
library("bsplus")
data(fuelModels, fuelMoisture,coForest)


#

# Model Inputs ------------------------------------------------------------------

### Live herbaceous fuel moisture

input_LHFM <- sliderInput(inputId = "liveHFM",
                          label = "Live herbaceous fuel moisture:",
                          min = 1,
                          max = 120, value = 80) %>%
  shinyInput_label_embed(shiny_iconlink() %>% bs_attach_modal(id_modal = "helpModalFM"))




### Live woody fuel moisture

input_LWFM <- sliderInput(inputId = "liveWFM",
                          label = "Live woody fuel moisture:",
                          min = 60,
                          max = 150, value = 130) %>%
  shinyInput_label_embed(
   shiny_iconlink() %>%
    bs_attach_modal(id_modal = "helpModalFM"))

### Fuel Model

list_fuelModel <- row.names(fuelModels)
names(list_fuelModel) <- paste(row.names(fuelModels)," - ",fuelModels$description,sep="")

input_FuelModel <- selectInput("fuelModel", "Fuel Model:",choices = list_fuelModel) %>%
  shinyInput_label_embed(shiny_iconlink() %>%
                           bs_embed_tooltip("Select primary fuel model."))

# Prescription Inputs ------------------------------------------------------------------

### Temperature

input_Temp <- sliderInput(inputId = "rxTempInput",
                          label = "Temperature Prescription:",
                          min = 1,
                          max = 100, value = c(30,75)) %>%
  shinyInput_label_embed(shiny_iconlink() %>%
                           bs_embed_tooltip("Select range of temperatures for prescription."))

### Wind

input_Wind <- sliderInput(inputId = "rxWindInput",
                          label = "Wind Prescription:",
                          min = 1,
                          max = 24, value = c(2,6)) %>%
  shinyInput_label_embed(shiny_iconlink() %>%
                           bs_embed_tooltip("Select range of wind speeds for prescription."))

### RH
  
input_RH <- sliderInput(inputId = "rxRHInput",
                        label = "RH Prescription:",
                        min = 1,
                        max = 100, value = c(8,18)) %>%
  shinyInput_label_embed(shiny_iconlink() %>%
                           bs_embed_tooltip("Select range of relative humidities for prescription."))

### Fine Fuel Moisture

input_FFM <- sliderInput(inputId = "rxFFMInput",
                         label = "1-hr Fuel Moisture Prescription:",
                         min = 1,
                         max = 45, value = c(4,8)) %>%
  shinyInput_label_embed(shiny_iconlink() %>%
                           bs_embed_tooltip("Select range of 1 hr fuel moistures for prescription."))

### Flame Length

input_FL <- sliderInput(inputId = "rxFLInput",
                        label = "Flame Length:",
                        min = 1,
                        max = 45, value = c(1,12)) %>%
  shinyInput_label_embed(shiny_iconlink() %>%
                           bs_embed_tooltip("Select range of flame lengths for prescription. Assumes no slope and no wind adjustment factor."))


# Main UI -----------------------------------------------------------------

navbarPage(title = "Prescription Forecast!",id = "nav",
           tabPanel("Input",
                    sidebarLayout(
                      sidebarPanel(
                        tabsetPanel(
                          tabPanel(title = "Model Inputs",
                                   bs_modal(
                                     id = "helpModalFM",
                                     title = "Live Fuel Moisture Help",
                                     body = includeMarkdown("MD/help_fuelmoistures.md"),
                                     size = "large"), #helpFuelModels
                                   input_LHFM,
                                   input_LWFM,
                                   input_FuelModel
                          ),
                          tabPanel(title = "Prescription Inputs",
                                   input_Temp,
                                   input_RH,
                                   input_Wind,
                                   input_FFM,
                                   input_FL
                                   )
                        ),
                      #  hr(),
                       # selectInput("tz_input", "Time Zone:",
                        #            c("PST" = "PST",
                         #             "MST" = "MST",
                          #            "MST / Arizona" = "America/Phoenix",
                           #           "CST" = "CST",
                            #          "EST" = "EST")) %>%
                #          shinyInput_label_embed(shiny_iconlink() %>%
                 #                                  bs_embed_tooltip("Select time zone of unit.")),
                        actionButton("button", "Calculate")
                        
                     ),
                      mainPanel(leafletOutput("map"), shiny_iconlink() %>%
                                                           bs_embed_tooltip("Select location for forecast.")
                                )
                    )
           ),
           tabPanel("Output",
                    plotOutput("rxPlot", height = "1200px")),
           tabPanel("About",
                    includeMarkdown("MD/about.md")),
           
           # activate tooltips, popovers, and MathJax
           use_bs_tooltip(),
           use_bs_popover()
           
           )
