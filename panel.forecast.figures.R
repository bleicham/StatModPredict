#------------------------------------------------------------------------------#
#                                                                              #
#                          Plotting Panel Figures                              #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file reads in the formatted forecasts created during an earlier step in #
# the toolbox and the date type of the original data. It then merges all of the#
# data together and forms panel plots for the available data.                  #
#------------------------------------------------------------------------------#
#                         By: Amanda Bleichrodt                                #
#------------------------------------------------------------------------------#
panel.forecast.figures <- function(formatted.forecast.input, data.type.input,
                                   smoothing.input, scaleYAxis.input, yAxisLabel.input, 
                                   dateBreaks.input, startYPoint.input, 
                                   dotSize.input){
  
#------------------------------------------------------------------------------#
# Reading in inputs from the main script ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names for manipulation throughout the remainder of the code.                 #
#------------------------------------------------------------------------------#
  
  ###########################
  # Formatted Forecast list #
  ###########################
  formatted.forecast.Figure <<- formatted.forecast.input

  #############
  # Date type #
  #############
  date.Figure <<- data.type.input
  
  ###################
  # Smoothing input #
  ###################
  smoothing.Figure <<- smoothing.input
  
  ###################
  # Scale of y-axis #
  ###################
  scaleY <<- scaleYAxis.input
  
  ################
  # Y-Axis label #
  ################
  yAxisLabel <<- yAxisLabel.input
  
  ###############
  # Date breaks #
  ###############
  dateBreaks <<- dateBreaks.input
  
  #######################
  # Start point, Y Axis #
  #######################
  startYAxis <<- startYPoint.input
  
  ############
  # Dot size #
  ############
  dotSizeData <<- dotSize.input
  
  ###########################
  # Creating the empty list #
  ###########################
  figureListLocationLoop <- list()
  
  #####################################
  # Creating the empty list for final #
  #####################################
  finalList <- list()
  
  #################################
  # Creating the empty data frame #
  #################################
  allData <- data.frame()
  
  ####################################
  # Creating and empty list for data #
  ####################################
  listData <- list()
  
  
  
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
# About: This sections loops through each formatted forecasts, adds columns    #
# for the forecast period, model, and location and then combines them all into #
# one data frame for later plotting.                                           #
#------------------------------------------------------------------------------#
for(i in 1:length(formatted.forecast.Figure)){

  # Determining the name of the indexed forecast
  nameIndex <- names(formatted.forecast.Figure[i])

  # Model type
  model.Figure <- strsplit(nameIndex, "[-]")[[1]][1]

  # Sub-setting location/group name
  locationGroupName <- strsplit(nameIndex, "[-]")[[1]][2]

  # Forecast period for weekly or daily data
  if(date.Figure %in% c("week", "day")){

    # Determining the forecast period from the name
    forecastPeriod <- substring(nameIndex, regexpr("-", nameIndex) + (nchar(locationGroupName) + 2))

    # Forecast period for yearly or time index data
    }else{

      # Determining the forecast period from the name
      forecastPeriod <- strsplit(nameIndex, "[-]")[[1]][3]

      }

  # Smoothing input indicator
  if(is.null(smoothing.Figure)){

    smoothingIndicator <- 0

  }else if(0 <= smoothing.Figure & smoothing.Figure <= 1){

    smoothingIndicator <- 0

  }else{

    smoothingIndicator <- 1

  }

  ###############################################
  # Handling the NAs in the data - ARIMA models #
  ###############################################
  if(model.Figure == "ARIMA" || (smoothingIndicator == 1 & model.Figure != "Prophet")){

    # Data for plot
    data.for.plot <- formatted.forecast.Figure[[i]] %>% # Re-naming the orginal data
      dplyr::mutate(median = ifelse(is.na(median), data, median), # Handling NAs for the median model fit
                    LB = ifelse(is.na(LB), data, LB), # Handling the NAs for the LB model fit
                    UB = ifelse(is.na(UB), data, UB)) # Handling the NAs for the UB model fit

  ###############################################
  # Handling the NAs in the data - Other models #
  ###############################################
  }else{

    # Renaming the data
    data.for.plot <- formatted.forecast.Figure[[i]]

  }

  ########################################################
  # Handling dates in the forecast files - weeks or days #
  ########################################################
  if(date.Figure %in% c("week", "day")){

    # Dates on x-axis
    data.for.plot <- data.for.plot %>%
      mutate(dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data

    ##############################################################
    # Handling dates in the forecast files - years or time index #
    ##############################################################
  }else{

    # Dates on x-axis
    data.for.plot <- data.for.plot %>%
      mutate(dates = as.numeric(Date)) # Changing years and time index to numeric

  }

  ##########################################
  # Final data to merge with the main list #
  ##########################################
  final.data <- data.for.plot %>%
    dplyr::mutate(forecastPeriod = forecastPeriod,
                  model = model.Figure,
                  location = locationGroupName)

  # Handling NAs
  if(any(final.data$UB == Inf)){

    final.data <- NA

  }

  # Saving the data in a list
  listData[[i]] <- final.data

} # End of loop for figure data

  ##########################################
  # Combining list of data frames into one #
  ##########################################
  allData <- as.data.frame(do.call(rbind, listData))


#------------------------------------------------------------------------------#
# Creating the panel figures ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the main data frame created above, and forms       #
# multiple panel figures, faceting by model. One panel corresponds to one      #
# location and forecast period. There can be up to four figures in one panel.  #
#------------------------------------------------------------------------------#

  #############################################
  # Looping through possible forecast periods #
  #############################################

  # Creating the list of unique forecast periods
  forecastPeriodsUnique <- unique(allData$forecastPeriod)

  ###########################
  # Starting the outer loop #
  ###########################
  for(d in 1:length(forecastPeriodsUnique)){

    # Indexed forecast period
    indexedForecastPeriod <- forecastPeriodsUnique[d]

    # Filtering data to include only indexed forecast period
    dataFilteredPeriod <- allData %>%
      dplyr::filter(forecastPeriod == indexedForecastPeriod) # Filtering by forecast period

    ######################################
    # Looping through possible locations #
    ######################################

    # Creating a list of unique locations
    locationsUnqiue <- unique(allData$location)

    #######################
    # Starting inner loop #
    #######################
    for(l in 1:length(locationsUnqiue)){

      # Indexed location
      indexedLocation <- locationsUnqiue[l]

      # Filtering data by location
      dataFilteredFinal <- dataFilteredPeriod %>%
        dplyr::filter(location == indexedLocation) # Filtering by locations

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

      ####################################
      # Setting up for graphing the data #
      ####################################

        # Variable to use for the y-axis 
        dataFilteredFinal$medianVar <- dataFilteredFinal$median
        
        # Variable to use for UB 
        dataFilteredFinal$UBVar <- dataFilteredFinal$UB
        
        # Variable to use for LB 
        dataFilteredFinal$LBVar <- dataFilteredFinal$LB
        
        # Variable to use for data 
        dataFilteredFinal$dataVar <- dataFilteredFinal$data
        
        # Adjusting the y-axis
        maxValue <- max(dataFilteredFinal[,-c(1,6:9)], na.rm = T)
        
        # Determining the breaks in the y-axis
        breaks.graph <- ifelse(maxValue/10 < 3, 1, floor(maxValue/10 + 5))
        
        # Min value of y-axis: Used if user does not want to start at zero 
        minValue <- floor(min(dataFilteredFinal[,-c(1,6:9)], na.rm = T))
      
      ###########################
      # Log-transformation data #
      ###########################
      }else{
        
        # Adding the log-transformed variable - Median 
        dataFilteredFinal$logMedian <- log10(dataFilteredFinal$median + 1)
        
        # Adding the log-transformed variable - UB 
        dataFilteredFinal$logUB <- log10(dataFilteredFinal$UB + 1)
        
        # Adding the log-transformed variable - LB 
        dataFilteredFinal$logLB <- log10(dataFilteredFinal$LB + 1)
        
        # Adding the log-transformed variable - Data 
        dataFilteredFinal$logData <- log10(dataFilteredFinal$data + 1)
        
        # Variable to use for the y-axis 
        dataFilteredFinal$medianVar <- dataFilteredFinal$logMedian
        
        # Variable to use for UB 
        dataFilteredFinal$UBVar <- dataFilteredFinal$logUB
        
        # Variable to use for LB 
        dataFilteredFinal$LBVar <- dataFilteredFinal$logLB
        
        # Variable to use for data 
        dataFilteredFinal$dataVar <- dataFilteredFinal$logData
        
        # Adjusting the y-axis - determining the max y value 
        maxValue <- max(dataFilteredFinal[,-c(1:9)], na.rm = T)
        
        # Min value of y-axis: Used if user does not want to start at zero 
        minValue <- floor(min(dataFilteredFinal[,-c(1:9)], na.rm = T))
        
        # Determining the breaks in the y-axis 
        breaks.graph <- ifelse(maxValue/10 < 6, 0.25, floor(maxValue/10))
        
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
# About: This section prepares the dates for panel figure. It allows           #
# users to select the number of breaks in the date labels.                     #
#------------------------------------------------------------------------------# 

    ################################################
    # Handling dates in the forecast files - Weeks #
    ################################################
    if(date.Figure %in% c("week")){
        
      # Dates on x-axis
      dataFilteredFinal <- dataFilteredFinal %>%
        mutate(dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data
        
      # Vertical line
      breakLine <- anytime::anydate(unique(dataFilteredFinal$forecastPeriod))
        
      # Breaks
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
          
        breaksLabel <- paste0(1, " week")
          
      }else{
          
        breaksLabel <- paste0(dateBreaks, " weeks")
        
      }
        
      # X-axis breaks 
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(dataFilteredFinal$dates)), max(anydate(dataFilteredFinal$dates)), by = breaksLabel))  # X-axis breaks
    
    ################################################
    # Handling dates in the forecast files - Days #
    ################################################
    }else if(date.Figure == "day"){
      
      # Dates on x-axis
      dataFilteredFinal <- dataFilteredFinal %>%
        mutate(dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data
      
      # Vertical line
      breakLine <- anytime::anydate(unique(dataFilteredFinal$forecastPeriod))
      
      # Breaks
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
        
        breaksLabel <- paste0(1, " day")
        
      }else{
        
        breaksLabel <- paste0(dateBreaks, " days")
        
      }
      
      # X-axis breaks 
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(dataFilteredFinal$dates)), max(anydate(dataFilteredFinal$dates)), by = breaksLabel))  # X-axis breaks
      
    ##############################################################
    # Handling dates in the forecast files - years or time index #
    ##############################################################
    }else{
      
      # Dates on x-axis
      dataFilteredFinal <- dataFilteredFinal %>%
        mutate(dates = as.numeric(Date)) # Changing years and time index to numeric 
      
      # Vertical line
      breakLine <- as.numeric(unique(dataFilteredFinal$forecastPeriod))
      
      # Breaks
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
        
        breaksLabel <- 1
        
      }else{
        
        breaksLabel <- as.numeric(dateBreaks)
        
      }
      
      # X-axis breaks
      xAxisBreaks <- scale_x_continuous(breaks = seq(min(dataFilteredFinal$dates, na.rm = T), max(dataFilteredFinal$dates, na.rm = T), by = breaksLabel))  # X-axis breaks
      
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
# Plotting the panel figures  ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the panel forecast figures for the main          #
# portion of the dashboard.                                                    #
#------------------------------------------------------------------------------#      
      
  ##############
  # Plot title #
  ##############
  title2 <- paste0(indexedLocation, "-", indexedForecastPeriod)

  ###########################
  # Creating the plot panel #
  ###########################
  panel <- ggplot(dataFilteredFinal, aes(x = dates, y = medianVar, text = paste('Date: ', dates, '<br>Median:', round(as.numeric(medianVar), 2)), group = 1)) +
        facet_grid(~model) +
        geom_ribbon(aes(ymin = LBVar, ymax = UBVar), fill = "grey90")+ # 95% PI ribbon
        geom_line(aes(x = dates, y = UBVar, text = paste('Date: ', dates, '<br>UB:', round(as.numeric(UBVar), 2)), group = 1), linetype = "dashed", size = 0.65) + # UB
        geom_line(aes(x = dates, y = LBVar, text = paste('Date: ', dates, '<br>LB:', round(as.numeric(LBVar), 2)), group = 1), linetype = "dashed", size = 0.65) + # LB
        geom_line(color = "red", size = 0.9) + # Median line
        geom_point(aes(x = dates, y = dataVar, text = paste('Date: ', dates, '<br>Count:', dataVar)), color = "black", shape = 1, size = as.numeric(sizeOfDataPoint)) + # Data points
        geom_vline(xintercept = breakLine, linetype = "dashed") + # Vertical line
        xAxisBreaks + # X axis breaks (i.e., dates)
        scale_y_continuous(breaks = seq(start, maxValue + (breaks.graph), by = breaks.graph), # Y-axis breaks
                           limits = c(start, maxValue + breaks.graph)) +  # Y-axis limits
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
  figureListLocationLoop[[l]] <- panel

  # Adding name to list element
  names(figureListLocationLoop)[l] <- title2

  } # End of inner loop
  
  # Final list to export
  finalList <- c(figureListLocationLoop, finalList)
  
  } # End of outer loop

  ############################
  # Returning the final list #
  ############################

  return(finalList)

} # End of function
    

  