# Testing

library("ggplot2")
library("cowplot")
library("devtools")
library("scales")
library("lubridate")
library("firebehavioR")
#install_github("BigelowLab/ndfd")

library("ndfd")

zipReturnDF <- function(zip, unitConvert){
  inputQuery <- query_this(what = 'zipcodes', zipCodeList = zip, element = c('rh','temp','wspd'))
  outputQuery <- NDFD(inputQuery)
  
  ws <- outputQuery$data$get_data(name = "wind-speed")
  rh <- outputQuery$data$get_data(name = "humidity")
  tmp <- outputQuery$data$get_data(name = "temperature")
  
  names(ws)[3] <- "windspeed"
  names(rh)[3] <- "rh"
  names(tmp)[3] <- "temp"
  
  returnDF <- data.frame("dt" = ws$start_valid_time,"windspeed" = ws$windspeed,"rh" = rh$rh,"temp" = tmp$temp)
  
  if(unitConvert == TRUE)
  {
    returnDF$windspeed <- returnDF$windspeed * 2.236936
    returnDF$temp <- returnDF$temp * 1.8 + 32
  }
  
  return(returnDF)
}

latlongReturnDF <- function(lat, long, unitConvert){
  inputQuery <- query_this(what = 'point', lon = long, lat = lat, element = c('rh','temp','wspd'))
  outputQuery <- NDFD(inputQuery)
  
  ws <- outputQuery$data$get_data(name = "wind-speed")
  rh <- outputQuery$data$get_data(name = "humidity")
  tmp <- outputQuery$data$get_data(name = "temperature")
  
  names(ws)[3] <- "windspeed"
  names(rh)[3] <- "rh"
  names(tmp)[3] <- "temp"
  
  returnDF <- data.frame("dt" = ws$start_valid_time,"windspeed" = ws$windspeed,"rh" = rh$rh,"temp" = tmp$temp)
  
  if(unitConvert == TRUE)
  {
    returnDF$windspeed <- returnDF$windspeed * 2.236936
    returnDF$temp <- returnDF$temp * 1.8 + 32
  }
  
  return(returnDF)
}

#test <- zipReturnDF(zips,unitConvert = TRUE)

interpolateTime <- function(DF,type){
  if(type == "spline"){
    rh <- data.frame(spline(DF$dt,DF$rh,method = "fmm", n = 500))
    temp <- data.frame(spline(DF$dt,DF$temp,method = "fmm", n = 500))
    ws <- data.frame(spline(DF$dt,DF$windspeed,method = "fmm", n = 500))
    
  }
  
  if(type == "linear"){
    rh <- data.frame(approx(DF$dt,DF$rh,rule = 2, method = "linear", ties = mean, n = 1000))
    temp <- data.frame(approx(DF$dt,DF$temp,rule = 2, method = "linear", ties = mean, n = 1000))
    ws <- data.frame(approx(DF$dt,DF$windspeed,rule = 2, method = "linear", ties = mean, n = 1000))
  }
  
  names(rh) <- c("dt","rh")
  names(temp) <- c("dt","temp")
  names(ws) <- c("dt","windspeed")
  
  returnDF <- merge(rh,temp,by="dt")
  returnDF <- merge(returnDF, ws, by = "dt")
  
  try(returnDF[which(returnDF$windspeed < 0),]$windspeed <- 0)
  
  returnDF$dt <- as.POSIXct(returnDF$dt,origin = "1970-01-01")
  
  returnDF$ffm <- ffm(method = "anderson",rh = returnDF$rh,temp = (returnDF$temp - 32)/1.8)$fm1hr
  returnDF$ffm10 <- ffm(method = "anderson",rh = returnDF$rh,temp = (returnDF$temp - 32)/1.8)$fm10hr
  returnDF$ffm100 <- ffm(method = "anderson",rh = returnDF$rh,temp = (returnDF$temp - 32)/1.8)$fm100hr
  returnDF$ffmlitter <- ffm(method = "anderson",rh = returnDF$rh,temp = (returnDF$temp - 32)/1.8)$fmLitter
  
  
  return(returnDF)
}

#testOutput <- interpolateTime(test,type = "spline")


#testOutput$dt <- with_tz(testOutput$dt, tzone = "MST")

diagPlot <- function(df,origDF){
  library("cowplot")
  
  RH <- ggplot(data = df, aes(x = dt, y = rh))+
    geom_point(color = "gray")+
    geom_point(data = origDF, aes(x = dt, y = rh), color="red")+
    xlab("Date / Time")+
    ylab("Relative Humidity (%)")
  
  
  WS <- ggplot(data = df, aes(x = dt, y = windspeed))+
    geom_point(color = "gray")+
    geom_point(data = origDF, aes(x = dt, y = windspeed),color="red")+
    xlab("Date / Time")+
    ylab("Wind Speed (mph)")
  
  tmp <- ggplot(data = df, aes(x = dt, y = temp))+
    geom_point(color = "gray")+
    geom_point(data = origDF, aes(x = dt, y = temp),color="red")+
    xlab("Date / Time")+
    ylab("Temperature (F)")
  
  returnGrid <- plot_grid(RH, WS, tmp, labels = c('A', 'B', 'C'), label_size = 12)
  
  return(returnGrid)  
}

#diagPlot(testOutput,test)

data(fuelModels, fuelMoisture,coForest)

fxn_firebehavior <- function(inputDF, fuelModel, LH = 120, LW = 130){
  
  
  fuelmodel_input <- fuelModels[fuelModel,]
  
  
  exampCrownFuel <- data.frame(
    CBD = rep(mean(coForest$cbd_kgm3),nrow(inputDF)),
    FMC = 100,
    CBH = rep(mean(coForest$cbh_m),nrow(inputDF)),
    CFL = rep(mean(coForest$cfl_kgm2),nrow(inputDF))
  )
  
  fuelMoisture <- data.frame(inputDF$ffmlitter,
                             inputDF$ffm,
                             inputDF$ffm10,
                             inputDF$ffm100,
                             LH,
                             LW)
  
  exampEnviro <- data.frame(
    slope = 0,
    windspeed = inputDF$windspeed * 1.609344,
    direction = 0,
    waf = 1
  )
  
  outputDF <- data.frame(FL = rep(NA,nrow(inputDF)),
                         ROS = rep(NA,nrow(inputDF)))
  
  for(i in 1:nrow(inputDF)){
    try(outputIn <- rothermel(surfFuel = fuelmodel_input,
                          moisture = fuelMoisture[i,],
                          crownFuel = exampCrownFuel[i,],
                          enviro = exampEnviro[i,]))
    #print(str(outputIn))
    
    try(
    outputDF$FL[i] <- as.numeric(outputIn$fireBehavior$`Flame Length [m]`) * 3.2808)
    
    try(
    outputDF$ROS[i] <- as.numeric(outputIn$fireBehavior$`Rate of Spread [m/min]`) * 3.2808)
    
    print(i)
  }
  
  combined <- cbind(inputDF,outputDF)
  
  return(combined)
  
}

#testOutput2 <- fxn_firebehavior(testOutput, "GR3")

prescription <- function(df,tempHi = 75,tempLo = 50, windHi = 8, windLo = 3,rhHi = 40, rhLo = 20, ffmLo = 4, ffmHi = 8, flLo = 1, flHi = 3){
  df$tempRx <- NA
  df$windRx <- NA
  df$rhRx <- NA
  df$ffmRx <- NA
  df$flRx <- NA
  
  try(df[which(df$windspeed <= windHi & df$windspeed >= windLo),]$windRx <- "In Prescription")
  try(df[which(is.na(df$windRx)),]$windRx <- "Out of Prescription")
  
  try(df[which(df$temp <= tempHi & df$temp >= tempLo),]$tempRx <- "In Prescription")
  try(df[which(is.na(df$tempRx)),]$tempRx <- "Out of Prescription")
  
  try(df[which(df$rh <= rhHi & df$rh >= rhLo),]$rhRx <- "In Prescription")
  try(df[which(is.na(df$rhRx)),]$rhRx <- "Out of Prescription")
  
  try(df[which(df$ffm <= ffmHi & df$ffm >= ffmLo),]$ffmRx <- "In Prescription")
  try(df[which(is.na(df$ffmRx)),]$ffmRx <- "Out of Prescription")
  
  try(df[which(df$FL <= flHi & df$FL >= flLo),]$flRx <- "In Prescription")
  try(df[which(is.na(df$flRx)),]$flRx <- "Out of Prescription")
  
  return(df)
}

#test4 <- prescription(df = testOutput2)

rxPlot <- function(df){
  library("cowplot")
  
  cols <- c("In Prescription" = "darkgreen", "Out of Prescription" = "gray")
  
  
  RH <- ggplot(data = df, aes(x = dt, y = rh, color = rhRx))+
    scale_colour_manual("RH Prescription", values = cols)+
    geom_point()+
    xlab("")+
    ylab("Relative Humidity (%)")+
    scale_x_datetime(labels = date_format("%F %H%M"),
                     date_breaks = "4 hours")+
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour="lightgray", size=0.5))+
    theme(axis.text.x=element_blank())
  
  
  
  WS <- ggplot(data = df, aes(x = dt, y = windspeed, color = windRx))+
    scale_colour_manual("Wind Prescription", values = cols)+
    geom_point()+
    xlab("")+
    ylab("Wind Speed (mph)")+
    scale_x_datetime(labels = date_format("%F %H%M"),
                     date_breaks = "4 hours")+
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour="lightgray", size=0.5))+
    theme(axis.text.x=element_blank())
  
  tmp <- ggplot(data = df, aes(x = dt, y = temp, color = tempRx))+
    scale_colour_manual("Temperature Prescription", values = cols)+
    geom_point()+
    xlab("")+
    ylab("Temperature (F)")+
    scale_x_datetime(labels = date_format("%F %H%M"),
                     date_breaks = "4 hours")+
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour="lightgray", size=0.5))+
    theme(axis.text.x=element_blank())
  
  ffm <- ggplot(data = df, aes(x = dt, y = ffm, color = ffmRx))+
    scale_colour_manual("1-hr Fuel Moisture Prescription", values = cols)+
    geom_point()+
    xlab("")+
    ylab("1-hr Fuel Moisture (%)")+
    scale_x_datetime(labels = date_format("%F %H%M"),
                     date_breaks = "4 hours")+
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour="lightgray", size=0.5))+
    theme(axis.text.x=element_blank())
  
  FL <- ggplot(data = df, aes(x = dt, y = FL, color = flRx))+
    scale_colour_manual("Flame Length Prescription", values = cols)+
    geom_point()+
    xlab("Date / Time")+
    ylab("Flame Length (ft)")+
    scale_x_datetime(labels = date_format("%F %H%M"),
                     date_breaks = "4 hours")+
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour="lightgray", size=0.5))+
    theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1)) 
  
  returnGrid <- plot_grid(RH, WS, tmp, ffm, FL, label_size = 12,align = "v",ncol=1)
  
  return(returnGrid)  
}

#rxPlot(test4)


