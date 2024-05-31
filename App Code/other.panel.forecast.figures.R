#------------------------------------------------------------------------------#
#                                                                              #
#                     Plotting Panel Figures - All Models                      #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function reads in the other model's forecast read in the dashboard, and #
# combines it with the produced ARIMA/GLM/GAM/Prophet forecasts. After         #
# cleaning up the data, the function then merges the all forecasts together.   #
# Next, it creates a panel of figures for a given location, forecast period    #
# date, horizon, and calibration length. The list of figures are then returned #
# as a product of the function.                                                #
#------------------------------------------------------------------------------#
#                         By: Amanda Bleichrodt                                #
#------------------------------------------------------------------------------#
other.panel.forecast.figures <- function(formatted.forecast.input, 
                                         formatted.forecast.Other.input,
                                         date.type.input, yAxisScale.input,
                                         yAxisLabel.input, dateBreaks.input,
                                         startY.input, dataDot.input,
                                         errorGLM.input, location.input) {
  

  
#------------------------------------------------------------------------------#
# Creating the 'not-in' function -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the 'not-in' function. Therefore, `%!in%` now    #
# can be used as the inverse of the built-in `%in%` function.                  #
#------------------------------------------------------------------------------#
  
  `%!in%` <- function(x, y) {
    
    !(x %in% y)
    
  }
  
#------------------------------------------------------------------------------#
# Reading in inputs from the main script ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names for manipulation throughout the remainder of the code.                 #
#------------------------------------------------------------------------------#
  
  ###########################
  # Formatted Forecast list #
  ###########################
  orignalData.input <- formatted.forecast.input
  
  ############################################
  # Reading in the other formatted forecasts #
  ############################################
  other.formatted.forecasts <- formatted.forecast.Other.input
  
  #############
  # Date type #
  #############
  date.Figure <- date.type.input
  
  ################
  # Y-axis scale #
  ################
  yAxis.scale <- yAxisScale.input
  
  ################
  # Y-axis label #
  ################
  yAxisLabel <- yAxisLabel.input
  
  ###############
  # Date breaks #
  ###############
  dateBreaks <- dateBreaks.input
  
  #################################
  # Starting point for the y-axis #
  #################################
  startY <- startY.input 
  
  ####################
  # Size of data dot #
  ####################
  sizeDataDot <- dataDot.input
  
  ####################
  # Error term - GLM #
  ####################
  errorGLM <- errorGLM.input 
  
  ######################
  # Original Locations #
  ######################
  locationOrg <- location.input
    
  ###############################################################
  # Creating and empty list for figures - Forecast period dates #
  ###############################################################
  listData <- list()
  
  ###################################################
  # Creating and empty list for figures - Locations #
  ###################################################
  listData1 <- list()
  
  #############################################################
  # Creating and empty list for figures - Calibration periods #
  #############################################################
  listData2 <- list()
  
  ###########################################################
  # Creating and empty list for figures - Forecast horizons #
  ###########################################################
  listData3 <- list()
  
  #########################
  # Final list of figures #
  #########################
  finalList <- list()
  
  
#------------------------------------------------------------------------------#
# Error for running the figures without the dashboard results ------------------
#------------------------------------------------------------------------------#
# About: This section returns an error if a user trys to load other files      #
# prior to running the full dashboard.                                         #
#------------------------------------------------------------------------------#
  
  if(all(any(is.null(orignalData.input) || length(orignalData.input) == 0) & !is.null(other.formatted.forecasts))){
    
    # Error to return
    return("ERROR1")
    
  }
  
#------------------------------------------------------------------------------#
# Potential errors with the loaded data ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks for errors in the column names and file names of  #
# the loaded data.                                                             #
#------------------------------------------------------------------------------#
  
  for(i in 1:length(other.formatted.forecasts)){
    
    # Indexed file
    data <- other.formatted.forecasts[[i]]
    
    #############################
    # Checking the column names #
    #############################
    
    # Expected
    expectedNames <- c("Date", "data", "median", "LB", "UB")
    
    # Observed
    observedNames <- c(colnames(data))
    
    # Checking if they match each other
    if(any(expectedNames != observedNames)){
      
      # Returning an error
      return("ERROR2")
      
    }
    
    ##########################
    # Checking the file name #
    ##########################
    
    # Indexed file name
    dataName <- names(other.formatted.forecasts)[i]
    
    # Checking for the word horizon #
    horizonModel <- qdapRegex::ex_between(dataName, "-", "-calibration")[[1]][1]
    
    # Horizon
    horizon <- qdapRegex::ex_between(horizonModel, "-", "-")[[1]][1]
    
    # Checking for the word calibration #
    calibration <- qdapRegex::ex_between(dataName, paste0(horizonModel, "-"), "-")[[1]][1]
    
    # Checking if they match what is expected
    if(any(horizon != "horizon" || calibration != "calibration")){
      
      # Returning an error
      return("ERROR3")
      
    }
    
    ###############################
    # Checking date specification #
    ###############################
    
    # Pulling the calibration period length
    caliLength <- qdapRegex::ex_between(dataName, paste0(calibration, "-"), "-")[[1]][1]
    
    # Pulling the location
    location <- qdapRegex::ex_between(dataName, paste0(calibration, "-", caliLength, "-"), "-")[[1]][1]
    
    # Pulling the date
    date <- qdapRegex::ex_between(dataName,  paste0(location, "-"), ".csv")[[1]][1]
    
    # Checking the date
    if(all(date.Figure == 'year' & nchar(date) != 4)){
      
      # Returning an Error
      return("ERROR4")
      
    }else if(all(date.Figure %in% c("week", "day") & nchar(date) != 10)){
      
      return("ERROR4")
      
    }
    
    ##########################
    # Checking the locations #
    ##########################
    if(location %!in% c(locationOrg)){
      
      return("ERROR5")
      
    }
    
  }
  
#------------------------------------------------------------------------------#
# Fixing the scale for the time series data -------------------------------------
#------------------------------------------------------------------------------#
# About: This section handles the scale variable, and ensures that something   #
# is specified and can be used below.                                          #    
#------------------------------------------------------------------------------#
  
  # Indicator for to use the original scale 
  if(yAxis.scale == "Original" || is.null(yAxis.scale) || is.null(yAxis.scale)){
    
    scaleIndicator <- 0
    
  # Indicator to use the log-10 scale   
  }else if(yAxis.scale == "Log(Base 10)"){
    
    scaleIndicator <- 1
    
  }
  
#------------------------------------------------------------------------------#
# Fixing the input for data dot size -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section works with the data size variable. It sets the default   #
# size to 2, unless the user specifies otherwise.                              #
#------------------------------------------------------------------------------#
  
  # Default 
  if(sizeDataDot == 2 || is.null(sizeDataDot) || is.null(sizeDataDot) || sizeDataDot == 0){
    
    sizeOfDataPoint <- 2
    
  # User specified 
  }else{
    
    sizeOfDataPoint <- sizeDataDot
    
  }
  
#------------------------------------------------------------------------------#
# Cleaning the other models ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the other forecasts read into the dashboard for #
# later merging with the ARIMA/GLM/GAM/Prophet models. It creates a single     #
# data frame of forecasts that can be used to produce panel figures.           #
#------------------------------------------------------------------------------#
  
  # Creating the empty data frame for merging - Other Forecasts
  allOtherForecasts <- NULL
  
  #####################################
  # Looping through the read-in files #
  #####################################
  for(i in 1:length(other.formatted.forecasts)){
    
    # Determining the name of the indexed forecast
    indexedOtherName <- names(other.formatted.forecasts[i])
    
    ##########################################
    # Pulling information from the file name #
    ##########################################
    
    # Model Abbr. 
    modelAbbr <- qdapRegex::ex_between(indexedOtherName, "", "-")[[1]][1]
    
    # Sub-setting location/group name 
    subModelAbbr <- qdapRegex::ex_between(indexedOtherName, paste0(modelAbbr, "-"), "-")[[1]][1]
    
    # Horizon
    horizon <- qdapRegex::ex_between(indexedOtherName, "horizon-", "-")[[1]][1]
    
    # Calibration period 
    calibration <- qdapRegex::ex_between(indexedOtherName, "calibration-", "-")[[1]][1]
    
    # Location
    location <- qdapRegex::ex_between(indexedOtherName, paste0("calibration-", calibration, "-"), "-")[[1]][1]
    
    # Checking the location
    if(location %!in% locationOrg){
      
      print("Error1")
      return("Error1")
      
    }
    
    # Determining the forecast period from the name
    forecastPeriod <- qdapRegex::ex_between(indexedOtherName, paste0(location, "-"), ".csv")[[1]][1]
    
    # Creating the full model name
    if(subModelAbbr == ""){
      
      model <- modelAbbr
      
    }else{
      
      model <- paste0(modelAbbr, "-", subModelAbbr)
      
    }
    
    if(date.Figure %in% c("week", "day")){
      
    ############################################
    # Adding the information to the data frame #
    ############################################
    data.temp <- other.formatted.forecasts[[i]] %>%
      dplyr::mutate(Model = model,
                    Horizon = horizon,
                    Calibration = calibration,
                    Location = location,
                    ForecastDate = anytime::anydate(forecastPeriod),
                    Date = anytime::anydate(Date))
    
    }else{
      
      ############################################
      # Adding the information to the data frame #
      ############################################
      data.temp <- other.formatted.forecasts[[i]] %>%
        dplyr::mutate(Model = model,
                      Horizon = horizon,
                      Calibration = calibration,
                      Location = location,
                      ForecastDate = as.numeric(forecastPeriod),
                      Date = as.numeric(Date))
      
    }
    
    #################################################
    # Merging with the rest of the read-in forecasts #
    #################################################
    allOtherForecasts <- rbind(allOtherForecasts, data.temp)
    
  }
  
  
#------------------------------------------------------------------------------#
# Cleaning the ARIMA/GLM/GAM/Prophet models ------------------------------------
#------------------------------------------------------------------------------#
# About: This section makes sure that the ARIMA/GLM/GAM/Prophet models are     #
# merged into a list that is the same format as the "Other" models forecast.   #
# This allows for merging in the next step, and later plotting of forecast     #
# panels.                                                                      #
#------------------------------------------------------------------------------#
  
  # Creating the empty data frame for merging - ARIMA/GLM/GAM/Prophet Forecasts
  allForecasts <- NULL
  
  ###############################################################
  # Running only if ARIMA/GLM/GAM/Prophet forecasts are entered #
  ###############################################################
  if(!is.null((orignalData.input))){
  
  #########################################################
  # Cleaning up the ARIMA, GLM, GAM and Prophet forecasts #
  #########################################################
  for(i in 1:length(orignalData.input)){
    
    # Determining the name of the indexed forecast
    nameIndex <- names(orignalData.input[i])
    
    # Model type 
    model <- strsplit(nameIndex, "[-]")[[1]][1]
    
    # Sub-setting location/group name 
    location <- strsplit(nameIndex, "[-]")[[1]][2]
    
    # Forecast period for weekly or daily data 
    if(date.Figure %in% c("week", "day")){
      
      # Determining the forecast period from the name
      forecastPeriod <- substring(nameIndex, regexpr("-", nameIndex) + (nchar(location) + 2))
      
      # Forecast period for yearly or time index data 
    }else{
      
      # Determining the forecast period from the name
      forecastPeriod <- strsplit(nameIndex, "[-]")[[1]][3]
      
    }
    
    ###############################################
    # Handling the NAs in the data - ARIMA models #
    ###############################################
    if(model == "ARIMA"){
      
      # Data for plot 
      data.for.plot <- orignalData.input[[i]] %>% # Re-naming the orginal data 
        dplyr::mutate(median = ifelse(is.na(median), data, median), # Handling NAs for the median model fit
                      LB = ifelse(is.na(LB), data, LB), # Handling the NAs for the LB model fit
                      UB = ifelse(is.na(UB), data, UB)) # Handling the NAs for the UB model fit
     
      
    ###############################################
    # Handling the NAs in the data - Other models #
    ###############################################
    }else{
      
      # Renaming the data 
      data.for.plot <- orignalData.input[[i]]
      
    }
    
    ########################################################
    # Handling dates in the forecast files - weeks or days #
    ########################################################
    if(date.Figure %in% c("week", "day")){
      
      # Dates on x-axis
      data.for.plot <- data.for.plot %>%
        mutate(dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data
      
      # Forecast horizon
      horizon <- nrow(data.for.plot[data.for.plot$dates > anytime::anydate(forecastPeriod),])
      
    ##############################################################
    # Handling dates in the forecast files - years or time index #
    ##############################################################
    }else{
      
      # Dates on x-axis
      data.for.plot <- data.for.plot %>%
        mutate(dates = as.numeric(Date)) # Changing years and time index to numeric 
      
      # Forecast horizon
      horizon <- nrow(data.for.plot[data.for.plot$dates > as.numeric(forecastPeriod),])
      
    }
    
    #############################
    # Calibration period length #
    #############################
    calibration <- nrow(data.for.plot) - as.numeric(horizon)
    
    ##########################################
    # Final data to merge with the main list #
    ##########################################
    final.data <- data.for.plot %>%
      dplyr::mutate(Date = dates,
                    ForecastDate = forecastPeriod,
                    Model = model,
                    Location = location,
                    Horizon = horizon,
                    Calibration = calibration) %>%
      dplyr::select(Date, data, median, LB, UB, Model, Horizon, Calibration, Location, ForecastDate)
      
    #############################
    # Saving the data in a list #
    #############################
    allForecasts <- rbind(allForecasts, final.data)
    
  } # End of loop for figure data 
    
  
  } # End of if-statement
  
#------------------------------------------------------------------------------#
# Merging data frames ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section merges together the other files read into the dashboard  #
# and the existing models run within the dashboard.                            #  
#------------------------------------------------------------------------------#
  
  ###############################
  # Merging the two data frames #
  ###############################
  plotData <- rbind(allOtherForecasts, allForecasts)
  
#------------------------------------------------------------------------------#
# Unique Values ----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates unique list of horizons, calibration periods,    #
# locations, and forecast period dates. This is then used to group forecasts   #
# for plotting in the next code chunk.                                         #
#------------------------------------------------------------------------------#
  
  #########################
  # Forecast horizon list #
  #########################
  horizonList <- c(unique(plotData$Horizon))
                   
  ###########################
  # Calibration period list #
  ###########################
  calibrationList <- c(unique(plotData$Calibration))
  
  #################
  # Location list #
  #################
  locationList <- c(unique(plotData$Location))
  
  ######################
  # Forecast Date List #
  ######################
  forecastDateList <- sort(c(unique(plotData$ForecastDate)))
  
#------------------------------------------------------------------------------#
# Creating the panel figures data ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the loops that will be used to form the figure  #
# including, filtering the data by the grouping variables.                     #
#------------------------------------------------------------------------------#
  
  #####################################
  # Looping through forecast horizons #
  #####################################
  for(h in 1:length(horizonList)){
    
    # Indexed forecast horizon
    indexedHorizon <- horizonList[h]
    
    #######################################
    # Looping through calibration periods #
    #######################################
    for(c in 1:length(calibrationList)){
      
      # Indexed calibration 
      indexedCalibration <- calibrationList[c]
      
      ######################################
      # Looping through forecast locations #
      ######################################
      for(l in 1:length(locationList)){
        
        # Indexed location
        indexedLocation <- locationList[l]
        
        #########################################
        # Looping through forecast period dates #
        #########################################
        for(f in 1:length(forecastDateList)){
          
          # Indexed forecast date
          indexedForecastDate <- forecastDateList[f] 
          
          ################################################################
          # Filtering the date for the indexed set of grouping variables #
          ################################################################
          if(date.Figure %in% c("week", "day")){
            
          dataFiltered <- plotData %>%
            dplyr::mutate(Date = anytime::anydate(Date)) %>%
            dplyr::filter(Horizon == indexedHorizon, # Filtering horizon
                          Calibration == indexedCalibration, # Filtering calibration
                          Location == indexedLocation, # Filtering location
                          ForecastDate == indexedForecastDate) # Filtering forecast date
           
          }else{
            
            dataFiltered <- plotData %>%
              dplyr::mutate(Date = as.numeric(Date)) %>%
              dplyr::filter(Horizon == indexedHorizon, # Filtering horizon
                            Calibration == indexedCalibration, # Filtering calibration
                            Location == indexedLocation, # Filtering location
                            ForecastDate == indexedForecastDate) # Filtering forecast date
          }
          
          #########################################################
          # Handling when a combination of factors DOES NOT exist #
          #########################################################
          if(nrow(dataFiltered) == 0){
            
            listData[f] <- NA # Replacing with NA
            listData[l] <- NA # Replacing with NA
            listData[c] <- NA # Replacing with NA
            listData[h] <- NA # Replacing with NA
            
            # Skipping to next loop iteration 
            next
            
          }
        
          
#------------------------------------------------------------------------------#
# Adjusting the scale of the y-axis --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adjusts the scale of the y-axis based upon the user's    #
# choice or lack of choice in the main dashboard. The default setting is the   #
# is the original scale. However, multiple options are available.              #
#------------------------------------------------------------------------------#
          
    if(scaleIndicator == 0){
            
      ##################################
      # Variable to use for the y-axis #
      ##################################
      dataFiltered$medianVar <- dataFiltered$median
      
      ##########################
      # Variable to use for UB #
      ##########################
      dataFiltered$UBVar <- dataFiltered$UB
      
      ##########################
      # Variable to use for LB #
      ##########################
      dataFiltered$LBVar <- dataFiltered$LB
      
      ############################
      # Variable to use for data #
      ############################
      dataFiltered$dataVar <- dataFiltered$data
      
      ######################################################
      # Adjusting the y-axis - determining the max y value #
      ######################################################
      maxValue <- max(dataFiltered[,-c(1, 6:10)], na.rm = T)
      
      ####################################################################
      # Min value of y-axis: Used if user does not want to start at zero #
      ####################################################################
      minValue <- floor(min(dataFiltered[,-c(1, 6:10)], na.rm = T))
      
      ########################################
      # Determining the breaks in the y-axis #
      ########################################
      breaks.graph <- ifelse(maxValue/6 < 1, 1, floor(maxValue/6))
      
      ############################
      # Handling the ARIMA Model #
      ############################
      dataFiltered <- dataFiltered %>%
        dplyr::mutate(LB = ifelse(Model == "ARIMA" & is.na(LB), data, LB),
                      UB = ifelse(Model == "ARIMA" & is.na(UB), data, UB))
      
    }else{
      
      ################################################
      # Adding the log-transformed variable - Median #
      ################################################
      dataFiltered$logMedian <- log10(dataFiltered$median + 1)
      
      ############################################
      # Adding the log-transformed variable - UB #
      ############################################
      dataFiltered$logUB <- log10(dataFiltered$UB + 1)
      
      ############################################
      # Adding the log-transformed variable - LB #
      ############################################
      dataFiltered$logLB <- log10(dataFiltered$LB + 1)
      
      ##############################################
      # Adding the log-transformed variable - Data #
      ##############################################
      dataFiltered$logData <- log10(dataFiltered$data + 1)

      ##################################
      # Variable to use for the y-axis #
      ##################################
      dataFiltered$medianVar <- dataFiltered$logMedian
      
      ##########################
      # Variable to use for UB #
      ##########################
      dataFiltered$UBVar <- dataFiltered$logUB
      
      ##########################
      # Variable to use for LB #
      ##########################
      dataFiltered$LBVar <- dataFiltered$logLB
      
      ############################
      # Variable to use for data #
      ############################
      dataFiltered$dataVar <- dataFiltered$logData
      
      ######################################################
      # Adjusting the y-axis - determining the max y value #
      ######################################################
      maxValue <- max(dataFiltered[,-c(1:10)], na.rm = T)
      
      ####################################################################
      # Min value of y-axis: Used if user does not want to start at zero #
      ####################################################################
      minValue <- floor(min(dataFiltered[,-c(1:10)], na.rm = T))
      
      ########################################
      # Determining the breaks in the y-axis #
      ########################################
      breaks.graph <- ifelse(maxValue/6 < 1.5, 0.50, floor(maxValue/6))
      
      ############################
      # Handling the ARIMA Model #
      ############################
      dataFiltered <- dataFiltered %>%
        dplyr::mutate(logLB = ifelse(Model == "ARIMA" & is.na(logLB), logData, logLB),
                      logUB = ifelse(Model == "ARIMA" & is.na(logUB), logData, logUB))
      
    }
  
#------------------------------------------------------------------------------#
# Determining the starting for the y-axis --------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines whether the start the y-axis at zero or the   #
# minimum value of the data set.                                               #
#------------------------------------------------------------------------------#

    ########################################################
    # Runs if the user selects to start the y-axis at zero #
    ########################################################
    if(startY == "0" || is.null(startY) || is.na(startY)){
      
      # Start value
      start <- 0
      
    ################################################################
    # Runs if the user does not select to start the y-axis at zero #
    ################################################################
    }else{
      
      # Start value
      start <- minValue
      
    }
          
#------------------------------------------------------------------------------#
# Cleaning up, and preparing the dates -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the dates for panel figure. It allows users to  #
# select the number of breaks in the date labels.                              #
#------------------------------------------------------------------------------#

    ################################################
    # Handling dates in the forecast files - Weeks #
    ################################################
    if(date.Figure %in% c("week")){
            
      # Vertical line
      breakLine <- unique(anytime::anydate(dataFiltered$ForecastDate))
      
      # Breaks
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
        
        breaksLabel <- paste0(1, " week")
        
      }else{
        
        breaksLabel <- paste0(dateBreaks, " weeks")
        
      }
            
      # X-axis breaks 
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anytime::anydate(dataFiltered$Date)), max(anytime::anydate(dataFiltered$Date)), by = breaksLabel))  # X-axis breaks
     
      ###############################################
      # Handling dates in the forecast files - Days #
      ###############################################    
      }else if(date.Figure %in% c("week")){
      
      # Vertical line
      breakLine <- unique(anytime::anydate(dataFiltered$ForecastDate))
      
      # Breaks
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
        
        breaksLabel <- paste0(1, " day")
        
      }else{
        
        breaksLabel <- paste0(dateBreaks, " days")
        
      }
      
      # X-axis breaks 
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anytime::anydate(dataFiltered$Date)), max(anytime::anydate(dataFiltered$Date)), by = breaksLabel))  # X-axis breaks
      
      ##############################################################
      # Handling dates in the forecast files - years or time index #
      ##############################################################
      }else{

        # Vertical line
        breakLine <- unique(as.numeric(dataFiltered$ForecastDate))
        
        # Breaks
        if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
          
          breaksLabel <- 1
          
        }else{
          
          breaksLabel <- as.numeric(dateBreaks)
          
        }
            
        # X-axis breaks
        xAxisBreaks <- scale_x_continuous(breaks = seq(min(dataFiltered$Date), max(dataFiltered$Date), by = breaksLabel))  # X-axis breaks
            
      }
          
#------------------------------------------------------------------------------#
# Y-Axis Label -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows for the customization of the y-axis by users. The #
# value used here is what is selected by users in the UI side.                 #
#------------------------------------------------------------------------------#
          
  if(is.null(yAxisLabel) || is.na(yAxisLabel)){
            
    yAxisLabelFinal <- "Count"
            
  }else{
            
    yAxisLabelFinal <- yAxisLabel
            
   }
          
#------------------------------------------------------------------------------#
# Plotting the panel figure ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the above data to plot the panel of forecast        #
# figures corresponding to the given location, calibration period, horizon,    #
# and forecast date. It then is saved to the list of figures that is           #
# outputted to the main dashboard.                                             #
#------------------------------------------------------------------------------#
  
  ###################################################
  # Determining the order to show the model figures #
  ###################################################
          
  # Adjusting for SLR
  dataFiltered <- dataFiltered %>%
    dplyr::mutate(Model = ifelse(errorGLM == "Normal" & Model == "GLM", "SLR", Model)) # Changing GLM to SLR if Normal 
          
  # Filtering to include only the ARIMA, GLM, GAM and Prophet models
  modelsFilteredFirst <- dataFiltered %>%
            dplyr::filter(Model %in% c("ARIMA", "GLM","SLR", "GAM", "Prophet"))
  
  # Filtering to include non-baseline models
  modelsFilteredSecond <- dataFiltered %>%
    dplyr::filter(Model %!in% c("ARIMA", "GLM","SLR", "GAM", "Prophet"))
  
  # Order to show model figures 
  distinctModels <- c(unique(modelsFilteredFirst$Model), unique(modelsFilteredSecond$Model))
  
  # Ordering the Model variable as a factor 
  dataFiltered <- dataFiltered %>%
            mutate(Model = factor(Model, levels = distinctModels))

  #################################
  # Plotting the forecast figures #
  #################################
  panel <- ggplot(dataFiltered, aes(x = Date, y = medianVar)) +
            facet_wrap(~Model) +
            geom_ribbon(aes(ymin = LBVar, ymax = UBVar), fill = "grey90") + # 95% PI ribbon
            geom_line(aes(x = Date, y = UBVar), linetype = "dashed", size = 0.65) + # UB
            geom_line(aes(x = Date, y = LBVar), linetype = "dashed", size = 0.65) + # LB
            geom_line(color = "red", size = 0.9) + # Median line
            geom_point(aes(x = Date, y = dataVar), color = "black", shape = 1, size = as.numeric(sizeOfDataPoint)) +  # Data points
            geom_vline(xintercept = breakLine, linetype = "dashed") + # Vertical line
            xAxisBreaks + # X axis breaks (i.e., Date)
            scale_y_continuous(breaks = seq(start, maxValue + start, by = breaks.graph), # Y-axis breaks
                               limits = c(start, maxValue)) +  # Y-axis limits
            labs(title = "", # Title
                 y = yAxisLabelFinal)  + # Y-axis labels
            theme_classic() + # Base theme
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Switching x-axis labels horizontal
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 10), # Plot title
                  axis.title.y = element_text(size = 10), # Y-axis label
                  axis.title.x=element_blank(), # Removing the x-axis label
                  panel.grid.major = element_line(color = "grey95"))
          
          #############################################
          # Adding Forecast date loop figures to list #
          #############################################
          listData[f] <- list(panel)
  
          # Title
          title1  <- paste0(dataFiltered[1,9], "-", dataFiltered[1,10])
          
          # Checking if the title includes ARIMA, GLM, GAM, SLR, or Prophet
          if(grepl('ARIMA|GLM|GAM|Propet|SLR', title1)){
            
            title1 <- paste0("a", title1)
              
          }
          
          # Adding label to list element
          names(listData)[f] <- paste0(title1, "-",indexedCalibration, "-", indexedHorizon)   
          
          } # End of loop for forecast dates 
        
        ###############################################################
        # Adding forecast date loop figures and location loop figures #
        ###############################################################
        listData1 <- c(listData1, listData)
        
        } # End of loop for locations
      
      #######################################################
      # Adding location and calibration period figure lists #
      #######################################################
      listData2 <- c(listData2, listData1)
      
      } # End of calibration loop 
    
    ##########################
    # Final list combination #
    ##########################
    listData3 <- c(listData3, listData2)
    
  } # End of horizon loop 
  
  ###############################
  # Removing NAs for final list #
  ###############################
  finalList <- listData3[!is.na(listData3)]
  
  # Removing unique rows
  ListtoExport <- unique(finalList)
  
  ################
  # Adding Names #
  ################
  for(i in 1:length(ListtoExport)){
    
    # Label for list
    titleList <- paste0(unique(ListtoExport[[i]][["data"]][["Location"]]), " - ", ListtoExport[[i]][["layers"]][[6]][["data"]][["xintercept"]])

    # Update figure name
    names(ListtoExport)[i] <- titleList

  }

  ############################
  # Returning the final list #
  ############################
  return(ListtoExport)
}
  
  
  
  
      
      
    