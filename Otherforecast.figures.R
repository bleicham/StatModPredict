#------------------------------------------------------------------------------#
#                                                                              #
#                            Plotting Other Figures                            #
#                                                                              #
#------------------------------------------------------------------------------#
#                         By: Amanda Bleichrodt                                #
#------------------------------------------------------------------------------#
Otherforecast.figures <- function(formattedForecastInput, date.type.input,
                                  scaleYAxis.input, yAxisLabel.input, 
                                  dateBreaks.input, startYPoint.input, 
                                  dotSize.input) {
  
#------------------------------------------------------------------------------#
# Reading in inputs from the main script ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names for manipulation throughout the remainder of the code.                 #
#------------------------------------------------------------------------------#
  
  ###########################
  # Formatted Forecast list #
  ###########################
  formatted.forecast.Figure <- formattedForecastInput
  
  #############
  # Date type #
  #############
  date.Figure <- date.type.input
  
  ###################
  # Scale of y-axis #
  ###################
  scaleY <- scaleYAxis.input
  
  ################
  # Y-Axis label #
  ################
  yAxisLabel <- yAxisLabel.input
  
  ###############
  # Date breaks #
  ###############
  dateBreaks <- dateBreaks.input
  
  #######################
  # Start point, Y Axis #
  #######################
  startYAxis <- startYPoint.input
  
  ############
  # Dot size #
  ############
  dotSizeData <- dotSize.input
  
  ###########################
  # Creating the empty list #
  ###########################
  figureList <- list()
  
  
#------------------------------------------------------------------------------#
# Fixing the scale for the time series data -------------------------------------
#------------------------------------------------------------------------------#
# About: This section handles the scale variable, and ensures that something   #
# is specified and can be used below.                                          #    
#------------------------------------------------------------------------------#
  
  # Indicator for to use the original scale 
  if(scaleY == "Original" || is.null(scaleY) || is.null(scaleY)){
    
    scaleIndicator <- 0
    
    # Indicator to use the log-10 scale   
  }else if(scaleY == "Log(Base 10)"){
    
    scaleIndicator <- 1
    
  }
  
#------------------------------------------------------------------------------#
# Fixing the input for data dot size -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section works with the data size variable. It sets the default   #
# size to 2, unless the user specifies otherwise.                              #
#------------------------------------------------------------------------------#
  
  # Default 
  if(dotSizeData == 2 || is.null(dotSizeData) || is.null(dotSizeData) || dotSizeData == 0){
    
    sizeOfDataPoint <- 2
    
  # User specified 
  }else{
    
    sizeOfDataPoint <- dotSizeData
    
  }
  
#------------------------------------------------------------------------------#
# Looping through formatted forecasts for plotting -----------------------------
#------------------------------------------------------------------------------#
# About: This section loops through formatted forecasts and creates a list of  #
# of forecast figures.                                                         #
#------------------------------------------------------------------------------#
  for(i in 1:length(formatted.forecast.Figure)){
    
    # Determining the name of the indexed forecast
    nameIndex <- names(formatted.forecast.Figure[i])
    
    # Model Abbr. 
    modelAbbr <- qdapRegex::ex_between(nameIndex, "", "-")[[1]][1]
    
    # Sub-setting location/group name 
    subModelAbbr <- qdapRegex::ex_between(nameIndex, paste0(modelAbbr, "-"), "-")[[1]][1]
    
    # Horizon
    horizon <- qdapRegex::ex_between(nameIndex, "horizon-", "-")[[1]][1]
    
    # Calibration period 
    calibration <- qdapRegex::ex_between(nameIndex, "calibration-", "-")[[1]][1]
    
    # Location
    location <- qdapRegex::ex_between(nameIndex, paste0("calibration-", calibration, "-"), "-")[[1]][1]
    
    # Determining the forecast period from the name
    forecastPeriod <- qdapRegex::ex_between(nameIndex, paste0(location, "-"), ".csv")[[1]][1]
    
    #######################
    # Formatting the date #
    #######################
    if(date.Figure %in% c("week", "day")){
      
      data.for.plot <- formatted.forecast.Figure[[i]] %>%
        dplyr::mutate(Date = anytime::anydate(Date)) 
      
    }else{
      
      data.for.plot <- formatted.forecast.Figure[[i]] %>%
        dplyr::mutate(Date = as.numeric(Date)) 
      
    }
    

#------------------------------------------------------------------------------#
# Adjusting the scale of the y-axis --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adjusts the scale of the y-axis based upon the user's    #
# choice or lack of choice in the main dashboard. The default setting is the   #
# is the original scale. However, multiple options are available.              #
#------------------------------------------------------------------------------#
    
    ###########################
    # No data transformations #
    ###########################
    if(scaleIndicator == 0){
      
      # Variable to use for the y-axis 
      data.for.plot$medianVar <- data.for.plot$median
    
      # Variable to use for UB 
      data.for.plot$UBVar <- data.for.plot$UB
      
      # Variable to use for LB 
      data.for.plot$LBVar <- data.for.plot$LB
      
      # Variable to use for data 
      data.for.plot$dataVar <- data.for.plot$data
      
      # Adjusting the y-axis
      maxValue <- max(data.for.plot[,-c(1, 10)], na.rm = T)
      
      # Min value of y-axis: Used if user does not want to start at zero 
      minValue <- floor(min(data.for.plot[,-c(1, 10)], na.rm = T))
    
      # Determining the breaks in the y-axis
      breaks.graph <- ifelse(maxValue/10 < 1, 1, floor(maxValue/10))
    
      # Handling INF: May happen with some figures 
      if(maxValue == Inf){
        
        # Saving an NA
        figureList[[i]] <- NA
        
        # Adding name to list element
        names(figureList)[i] <- nameIndex
        
        # Next loop
        next
        
      }
    
    #########################
    # Log-10 Transformation #
    #########################
    }else{
      
      # Adding the log-transformed variable - Median 
      data.for.plot$logMedian <- log10(data.for.plot$median + 1)
      
      # Adding the log-transformed variable - UB 
      data.for.plot$logUB <- log10(data.for.plot$UB + 1)
      
      # Adding the log-transformed variable - LB 
      data.for.plot$logLB <- log10(data.for.plot$LB + 1)
      
      # Adding the log-transformed variable - Data 
      data.for.plot$logData <- log10(data.for.plot$data + 1)
      
      # Variable to use for the y-axis 
      data.for.plot$medianVar <- data.for.plot$logMedian
    
      # Variable to use for UB 
      data.for.plot$UBVar <- data.for.plot$logUB
      
      # Variable to use for LB 
      data.for.plot$LBVar <- data.for.plot$logLB
      
      # Variable to use for data 
      data.for.plot$dataVar <- data.for.plot$logData
      
      # Adjusting the y-axis - determining the max y value 
      maxValue <- max(data.for.plot[,-c(1:10)], na.rm = T)
      
      # Min value of y-axis: Used if user does not want to start at zero 
      minValue <- floor(min(data.for.plot[,-c(1:10)], na.rm = T))
      
      # Determining the breaks in the y-axis 
      breaks.graph <- ifelse(maxValue/10 < 1.5, 0.50, floor(maxValue/10))
      
      # Handling INF: May happen with some figures 
      if(maxValue == Inf){
        
        # Saving an NA
        figureList[[i]] <- NA
        
        # Adding name to list element
        names(figureList)[i] <- nameIndex
        
        # Next loop
        next
        
      }
      
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
  if(startYAxis == "0" || is.null(startYAxis) || is.na(startYAxis)){
      
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
# About: This section prepares the dates for individual figure. It allows users to  #
# select the number of breaks in the date labels.                              #
#------------------------------------------------------------------------------#    
    
  ################################################
  # Handling dates in the forecast files - Weeks #
  ################################################
  if(date.Figure %in% c("week")){
    
    # Dates on x-axis
    data.for.plot <- data.for.plot %>%
      mutate(dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data
      
    # Vertical line
    breakLine <- anytime::anydate(forecastPeriod)
    
    # Breaks
    if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
      
      breaksLabel <- paste0(1, " week")
      
    }else{
      
      breaksLabel <- paste0(dateBreaks, " weeks")
      
    }
      
    # X-axis breaks 
    xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(data.for.plot$dates)), max(anydate(data.for.plot$dates)), by = breaksLabel))  # X-axis breaks
    
    ###############################################
    # Handling dates in the forecast files - Days #
    ###############################################
    }else if(date.Figure %in% c("day")){
      
      # Dates on x-axis
      data.for.plot <- data.for.plot %>%
        mutate(dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data
      
      # Vertical line
      breakLine <- anytime::anydate(forecastPeriod)
      
      # Breaks
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
        
        breaksLabel <- paste0(1, " day")
        
      }else{
        
        breaksLabel <- paste0(dateBreaks, " days")
        
      }
      
      # X-axis breaks 
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(data.for.plot$dates)), max(anydate(data.for.plot$dates)), by = breaksLabel))  # X-axis breaks
      
    ##############################################################
    # Handling dates in the forecast files - years or time index #
    ##############################################################
    }else{
      
      # Dates on x-axis
      data.for.plot <- data.for.plot %>%
        mutate(dates = as.numeric(Date)) # Changing years and time index to numeric 
      
      # Vertical line
      breakLine <- as.numeric(forecastPeriod)
      
      # Breaks
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
        
        breaksLabel <- 1
        
      }else{
        
        breaksLabel <- as.numeric(dateBreaks)
        
      }
      
      # X-axis breaks
      xAxisBreaks <- scale_x_continuous(breaks = seq(min(data.for.plot$dates, na.rm = T), max(data.for.plot$dates, na.rm = T), by = breaksLabel))  # X-axis breaks
      
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
# Label for figure list --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the labels for the list of figures. The titles   #
# created here, are then included as titles for the figures in the main UI.    #
#------------------------------------------------------------------------------#
    
  if(subModelAbbr == ""){
      
    title <- paste0(modelAbbr, "-", location, "-", forecastPeriod)
      
  }else{
      
    title <- paste0(modelAbbr, "-", subModelAbbr, "-", location, "-", forecastPeriod)
      
  }

#------------------------------------------------------------------------------#
# Plotting the individual figures - Other models -------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the individual forecast figures for the other    #
# files read into the dashboard.                                               #
#------------------------------------------------------------------------------#
    
  individual.figure <- ggplot(data.for.plot, aes(x = dates, y = medianVar, text = paste('Date: ', dates, '<br>Median:', round(as.numeric(median), 2)), group = 1)) +
      geom_ribbon(aes(ymin = LBVar, ymax = UBVar), fill = "grey90")+ # 95% PI ribbon
      geom_line(aes(x = dates, y = UBVar, text = paste('Date: ', dates, '<br>UB:', round(as.numeric(UB), 2)), group = 1), linetype = "dashed", size = 0.65) + # UB
      geom_line(aes(x = dates, y = LBVar, text = paste('Date: ', dates, '<br>LB:', round(as.numeric(LB), 2)), group = 1), linetype = "dashed", size = 0.65) + # LB
      geom_line(color = "red", size = 0.9) + # Median line
      geom_point(aes(x = dates, y = dataVar, text = paste('Date: ', dates, '<br>Count:', data)), color = "black", shape = 1, size = as.numeric(sizeOfDataPoint)) + # Data points
      geom_vline(xintercept = breakLine, linetype = "dashed") + # Vertical line
      xAxisBreaks + # X axis breaks (i.e., dates)
      scale_y_continuous(breaks = seq(start, maxValue + start, by = breaks.graph), # Y-axis breaks
                         limits = c(start, maxValue)) +  # Y-axis limits
      labs(title = "", # Title
           y = yAxisLabel) + # Y-axis labels
      theme_classic() + # Base theme
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Switching x-axis labels horizontal
            plot.title = element_text(hjust = 0.5, face = "bold", size = 10), # Plot title
            axis.title.y = element_text(size = 10), # Y-axis label
            axis.title.x=element_blank(), # Removing the x-axis label
            panel.grid.major = element_line(color = "grey95"))
    
    ####################################
    # Saving the plot in the main list #
    ####################################
    figureList[[i]] <- individual.figure
    
    # Adding name to list element
    names(figureList)[i] <- title
    
  } # End of loop

  
  ######################
  # Returning the list #
  ######################
  return(figureList)
  
} # End of function


