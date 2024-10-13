#------------------------------------------------------------------------------#
#                                                                              #
#                          Plotting Panel Figures                              #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file reads in the formatted forecasts read in from an earlier function  #
# and forms panel figures. Panel figures contain figures for each model-       #
# calibration period length-forecast period date combination. There is up to   #
# four panels per graph (i.e., one for each model) and user can customize each #
# panel. The final figures are outputted as a list, and user can then save the #
# list of panels to their personal computer.                                   #
#------------------------------------------------------------------------------#
#                         Author: Amanda Bleichrodt                            #
#------------------------------------------------------------------------------#
panel.forecast.figures.other <- function(formatted.forecast.input, formatted.forecast.other.input,
                                         other.panel.input, data.type.input,
                                         smoothing.input, scaleYAxis.input, yAxisLabel.input, 
                                         dateBreaks.input, startYPoint.input, 
                                         dotSize.input, linetype.input, lineColor.input,
                                         lineWidth.input, dotColor.input, boundtype.input,
                                         boundWidth.input, boundColor.input,
                                         ribbonColor.input, yLabelSize.input, yLabelFace.input,
                                         yTickSize.input, yTickBreaks.input, xAxisLabel.input,
                                         xAxisLabelSize.input, xAxisLabelFace.input,
                                         xAxisTickSize.input, quantile.input){
  
#------------------------------------------------------------------------------#
# Reading in inputs from the main script ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names for manipulation throughout the remainder of the code.                 #
#------------------------------------------------------------------------------#
  
  ###########################
  # Formatted Forecast list #
  ###########################
  formatted.forecast.Figure <- formatted.forecast.input
  
  #####################################
  # Formatted Forecast - Other models #
  #####################################
  formatted.forecast.Figure.other <- formatted.forecast.other.input

  #############
  # Date type #
  #############
  date.Figure <- data.type.input
  
  ###################
  # Smoothing input #
  ###################
  smoothing.Figure <- smoothing.input
  
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
  
  #############
  # Dot color #
  #############
  dotColorData <- dotColor.input
  
  ####################
  # Median line type #
  ####################
  MLinetype <- linetype.input
  
  #####################
  # Median line color #
  #####################
  MLineColor <- lineColor.input
  
  #####################
  # Median line width #
  #####################
  MLineWidth <- lineWidth.input
  
  ###################
  # Bound line type #
  ###################
  BLineType <- boundtype.input
  
  ####################
  # Bound line width #
  ####################
  BLineWidth <- boundWidth.input
  
  ####################
  # Bound line color #
  ####################
  BLineColor <- boundColor.input
  
  #####################
  # Y-Axis Label size #
  #####################
  YAxisLabelSize <- yLabelSize.input
  
  #####################
  # Y-Axis Label face #
  #####################
  YAxisLabelFace <- yLabelFace.input
  
  ####################
  # Y-Axis Tick Size #
  ####################
  YAxisTickSize <- yTickSize.input
  
  #################
  # Y-Axis Breaks #
  #################
  YAxisBreaks <- yTickBreaks.input
  
  ################
  # X-Axis Label #
  ################
  XAxisLabel <- xAxisLabel.input
  
  #####################
  # X-Axis Label Size #
  #####################
  xAxisLabelSize <- xAxisLabelSize.input
  
  #####################
  # X-Axis Label Face #
  #####################
  xAxisLabelFace <- xAxisLabelFace.input
  
  ####################
  # X-Axis Tick Size #
  ####################
  xAxisTickSize <- xAxisTickSize.input
  
  ##################
  # Quantile input #
  ##################
  quantileSelection <- quantile.input

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
# Combining the forecast lists -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section combines the forecasts from the dashboard and the        #
# forecasts read in by the user.                                               #
#------------------------------------------------------------------------------#
  
  #####################################################
  # Combining the list if all forecasts are available #
  #####################################################
  if(!is.null(formatted.forecast.Figure)){
    
    # Combined list
    combinedList <- c(formatted.forecast.Figure, formatted.forecast.Figure.other)
    
  ######################################
  # Including only the other forecasts #
  ######################################
  }else{
    
    # Combined list
    combinedList <- formatted.forecast.Figure.other
    
  }
  
  
#------------------------------------------------------------------------------#
# Creating the indicator for the median line type ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the indicator to determine the line type for the #
# median line.                                                                 #
#------------------------------------------------------------------------------#
  
  linetypeFinal <- switch (MLinetype,
                           "Solid" = 1,
                           "Dashed" = 2,
                           "Dotted" = 3,
                           "Dotdash" = 4,
                           "Longdash" = 5,
                           "Twodash" = 6)
  
#------------------------------------------------------------------------------#
# Creating the indicator for the bound line type -------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the indicator to determine the line type for the #
# bounds lines.                                                                #
#------------------------------------------------------------------------------#
  
  BlinetypeFinal <- switch (BLineType,
                            "Solid" = 1,
                            "Dashed" = 2,
                            "Dotted" = 3,
                            "Dotdash" = 4,
                            "Longdash" = 5,
                            "Twodash" = 6)
  
#------------------------------------------------------------------------------#
# Creating the face-type variable (Y) ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the variable hosting the 'face' specification    #
# for the y-axis label. While the user has all available options, this code    #
# fixes the name to match the argument needed for ggplot.                      #
#------------------------------------------------------------------------------#
  
  YFaceLabelFinal <- switch(YAxisLabelFace,
                            "Plain" = "plain",
                            "Bold" = "bold",
                            "Italic" = "italic", 
                            "Bold & Italic" = "bold.italic") 
  
#------------------------------------------------------------------------------#
# Creating the face-type variable (X) ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the variable hosting the 'face' specification    #
# for the x-axis label. While the user has all available options, this code    #
# fixes the name to match the argument needed for ggplot.                      #
#------------------------------------------------------------------------------#
  
  XFaceLabelFinal <- switch(xAxisLabelFace,
                            "Plain" = "plain",
                            "Bold" = "bold",
                            "Italic" = "italic", 
                            "Bold & Italic" = "bold.italic") 
  
  
#------------------------------------------------------------------------------#
# Fixing the scale for the forecast data ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section handles the scale variable, and ensures that something   #
# is specified and can be used below.                                          #    
#------------------------------------------------------------------------------#

  ###########################################
  # Indicator for to use the original scale #
  ###########################################
  if(scaleY == "Original" || is.null(scaleY) || is.null(scaleY)){

    scaleIndicator <- 0

  #####################################
  # Indicator to use the log-10 scale #
  #####################################
  }else if(scaleY == "Log(Base 10)"){

    scaleIndicator <- 1

  }

#------------------------------------------------------------------------------#
# Fixing the input for data dot size -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section works with the data size variable. It sets the default   #
# size to 2, unless the user specifies otherwise.                              #
#------------------------------------------------------------------------------#

  ##################
  # Default option #
  ##################
  if(dotSizeData == 2 || is.null(dotSizeData) || is.null(dotSizeData) || dotSizeData == 0){

    sizeOfDataPoint <- 2

  
  #########################
  # User specified option #
  #########################
  }else{

    sizeOfDataPoint <- dotSizeData

  }
  
#------------------------------------------------------------------------------#
# Creating the smoothing indicator ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the indicator for smoothing the data. If the     #
# option is not available or is not selected, it indicates that smoothing was  #
# not applied to the data prior to fitting the model. If smoothing was applied #
# the indicator is equal to one.                                               #
#------------------------------------------------------------------------------#
  
  ############################################
  # Smoothing input indicator - No Smoothing #
  ############################################
  if(is.null(smoothing.Figure)){
    
    smoothingIndicator <- 0
    
  ############################################
  # Smoothing input indicator - No Smoothing #
  ############################################
  }else if(0 <= smoothing.Figure & smoothing.Figure <= 1){
    
    smoothingIndicator <- 0
    
  #########################################
  # Smoothing input indicator - Smoothing #
  #########################################
  }else{
    
    smoothingIndicator <- 1
    
  }

#------------------------------------------------------------------------------#
# Looping through formatted forecasts for plotting -----------------------------
#------------------------------------------------------------------------------#
# About: This sections loops through each formatted forecasts, adds columns    #
# for the forecast period date, model, location, and calibration period size   #
# and then combines them all into one data frame for later plotting.           #
#------------------------------------------------------------------------------#
  
  for(i in 1:length(combinedList)){
  
    ################################################
    # Determining the name of the indexed forecast #
    ################################################
    nameIndex <- names(combinedList[i])
    
    #############################################################################
    # Pulling the required information from the forecast name: Dashboard models #
    #############################################################################
    if(strsplit(nameIndex, "[-]")[[1]][1] %in% c("ARIMA", "GLM", "GAM", "Prophet")){
    
      # Model type
      model.Figure <- strsplit(nameIndex, "[-]")[[1]][1]
    
      # Location/group name
      locationGroupName <- strsplit(nameIndex, "[-]")[[1]][2]
      
      # Calibration
      calibration.FF <- qdapRegex::ex_between(nameIndex, "Calibration-", "(")[[1]][1]
    
      # Forecast period date - week or day
      if(date.Figure %in% c("week", "day")){
    
        forecastPeriod <- anytime::anydate(paste0(strsplit(nameIndex, "[-]")[[1]][3], "-", strsplit(nameIndex, "[-]")[[1]][4], "-", strsplit(nameIndex, "[-]")[[1]][5]))
        
      # Forecast period date - year or time index 
      }else{
        
        # Forecast period date
        forecastPeriod <- strsplit(nameIndex, "[-]")[[1]][3]
        
      }
      
    #########################################################################
    # Pulling the required information from the forecast name: Other models #
    #########################################################################
    }else{
      
      # Model type
      model.Figure <- paste0(strsplit(nameIndex, "[-]")[[1]][1], "-", strsplit(nameIndex, "[-]")[[1]][2])
      
      # Location/group name
      locationGroupName <- strsplit(nameIndex, "[-]")[[1]][7]
      
      # Calibration
      calibration.FF <- strsplit(nameIndex, "[-]")[[1]][6]
      
      # Forecast period date - week or day
      if(date.Figure %in% c("week", "day")){
        
        # Pulling the year
        yearChar <- strsplit(strsplit(nameIndex, "[-]")[[1]][10], "[.]")[[1]][1]
        
        # Forecast period date
        forecastPeriod <- anytime::anydate(paste0(yearChar, "-", strsplit(nameIndex, "[-]")[[1]][8], "-", strsplit(nameIndex, "[-]")[[1]][9]))
        
      # Forecast period date - year or time index 
      }else{
        
        # Forecast period date
        forecastPeriod <- as.numeric(strsplit(strsplit(nameIndex, "[-]")[[1]][8], "[.]")[[1]][1])
        
      }
      
    } # End of 'else' for other models 
    
    ##########################################################
    # Handling the NAs in the data - ARIMA models, Smoothing #
    ##########################################################
    if(model.Figure == "ARIMA" || (smoothingIndicator == 1 & model.Figure != "Prophet")){
  
      # Data for plot
      data.for.plot <- combinedList[[i]] %>% # Re-naming the orginal data
        dplyr::mutate(median = ifelse(is.na(median), data, median), # Handling NAs for the median model fit
                      LB = ifelse(is.na(LB), data, LB), # Handling the NAs for the LB model fit
                      UB = ifelse(is.na(UB), data, UB)) # Handling the NAs for the UB model fit
  
    ###############################################
    # Handling the NAs in the data - Other models #
    ###############################################
    }else{
  
      # Renaming the data
      data.for.plot <- combinedList[[i]]
  
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
                    location = locationGroupName,
                    calibration = calibration.FF)
  
    # Handling NAs
    if(any(final.data$UB == Inf)){
  
      final.data <- NA
  
    }
  
    #############################
    # Saving the data in a list #
    #############################
    listData[[i]] <- final.data
  
  } # End of loop for figure data

  ##########################################
  # Combining list of data frames into one #
  ##########################################
  allData <- as.data.frame(do.call(rbind, listData))
  
  
#------------------------------------------------------------------------------#
# Determining unique amounts ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines the unique lists of calibration period        #
# lengths, forecast period dates, and locations for the loops below. It also   #
# calculates the length of the list to store the plots within for export.      #
#------------------------------------------------------------------------------#
  
  ##########################################################
  # Creating the list of unique calibration period lengths #
  ##########################################################
  calibrationPeriodUnique <- unique(allData$calibration)
  
  ################################################
  # Creating the list of unique forecast periods #
  ################################################
  forecastPeriodsUnique <- unique(allData$forecastPeriod)
  
  #######################################
  # Creating a list of unique locations #
  #######################################
  locationsUnqiue <- unique(allData$location)
  
  ###########################
  # Creating the list index #
  ###########################
  listIndex <- (length(calibrationPeriodUnique)*length(forecastPeriodsUnique)*length(locationsUnqiue)) 
  
#------------------------------------------------------------------------------#
# Creating the panel figures ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the main data frame created above, and forms       #
# multiple panel figures, faceting by model. One panel corresponds to one      #
# location, forecast period, and calibration period length. There can be up to #
# four figures in one panel.                                                   # 
#------------------------------------------------------------------------------#
  
  ####################################
  # Starting the loop - Calibrations #
  ####################################
  for(c in 1:length(calibrationPeriodUnique)){
    
    ##########################################
    # Starting the forecast period date loop #
    ##########################################
    for(d in 1:length(forecastPeriodsUnique)){
      
      ###############################
      # Starting the locations loop #
      ###############################
      for(l in 1:length(locationsUnqiue)){
        
        ###################################################################################################
        # Filtering the data for the needed calibration period length, forecast period date, and location #
        ###################################################################################################
        dataFilteredFinal <- allData %>%
          dplyr::filter(location == locationsUnqiue[l], # Filtering by location 
                        calibration == calibrationPeriodUnique[c], # Filtering by calibration period length
                        forecastPeriod == forecastPeriodsUnique[d]) # Filtering by forecast period date 
        
        #######################
        # Checking for errors #
        #######################
        if(nrow(dataFilteredFinal) == 0){
          
          next

        }else{
          
#------------------------------------------------------------------------------#
# Adjusting the scale of the y-axis --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adjusts the scale of the y-axis based upon the user's    #
# choice or lack of choice in the main dashboard. The default setting is the   #
# is the original scale. However, multiple options are available.              #
#------------------------------------------------------------------------------#
       
        ######################################################
        # Runs if the user selects to use the original scale #
        ######################################################
        if(scaleIndicator == 0){
  
          ##################################
          # Variable to use for the y-axis #
          ##################################
          dataFilteredFinal$medianVar <- dataFilteredFinal$median
          
          ##########################
          # Variable to use for UB #
          ##########################
          dataFilteredFinal$UBVar <- dataFilteredFinal$UB
          
          ##########################
          # Variable to use for LB #
          ##########################
          dataFilteredFinal$LBVar <- dataFilteredFinal$LB
          
          ############################
          # Variable to use for data #
          ############################
          dataFilteredFinal$dataVar <- dataFilteredFinal$data
          
          ######################################################
          # Adjusting the y-axis - determining the max y value #
          ######################################################
          maxValue <- max(dataFilteredFinal[,-c(1,6:10)], na.rm = T)
          
          ########################################
          # Determining the breaks in the y-axis #
          ########################################
          
          # Break calculation
          breakCalculation <- maxValue/YAxisBreaks
          
          # Determining the number of breaks
          breaks.graph <- case_when(
            
            breakCalculation > 1 ~ floor(breakCalculation),
            breakCalculation <= 1 ~ round(breakCalculation, 2)
            
          )
          
          ####################################################################
          # Min value of y-axis: Used if user does not want to start at zero #
          #################################################################### 
          minValue <- floor(min(dataFilteredFinal[,-c(1,6:10)], na.rm = T))
        
        ############################################
        # Runs if the selects to use the log-scale #
        ############################################
        }else{
          
          ################################################
          # Adding the log-transformed variable - Median #
          ################################################
          dataFilteredFinal$logMedian <- log10(dataFilteredFinal$median + 1)
          
          ############################################
          # Adding the log-transformed variable - UB #
          ############################################
          dataFilteredFinal$logUB <- log10(dataFilteredFinal$UB + 1)
          
          ############################################
          # Adding the log-transformed variable - LB #
          ############################################
          dataFilteredFinal$logLB <- log10(dataFilteredFinal$LB + 1)
          
          ##############################################
          # Adding the log-transformed variable - Data #
          ##############################################
          dataFilteredFinal$logData <- log10(dataFilteredFinal$data + 1)
          
          ##################################
          # Variable to use for the y-axis #
          ##################################
          dataFilteredFinal$medianVar <- dataFilteredFinal$logMedian
          
          ##########################
          # Variable to use for UB #
          ##########################
          dataFilteredFinal$UBVar <- dataFilteredFinal$logUB
          
          ##########################
          # Variable to use for LB #
          ##########################
          dataFilteredFinal$LBVar <- dataFilteredFinal$logLB
          
          ############################
          # Variable to use for data #
          ############################
          dataFilteredFinal$dataVar <- dataFilteredFinal$logData
          
          ######################################################
          # Adjusting the y-axis - determining the max y value #
          ######################################################
          maxValue <- max(dataFilteredFinal[,-c(1:10)], na.rm = T)
          
          ####################################################################
          # Min value of y-axis: Used if user does not want to start at zero #
          #################################################################### 
          minValue <- floor(min(dataFilteredFinal[,-c(1:10)], na.rm = T))
          
          ########################################
          # Determining the breaks in the y-axis #
          ########################################
          
          # Break calculation
          breakCalculation <- maxValue/YAxisBreaks
          
          # Determining the number of breaks 
          breaks.graph <- case_when(
            
            breakCalculation > 1 ~ floor(breakCalculation),
            breakCalculation <= 1 ~ round(breakCalculation, 2)
            
          ) 
          
        } # End of `if-else`
      
#------------------------------------------------------------------------------#
# Determining the starting value for the y-axis --------------------------------
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
# Plotting the panel figures  -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the panel forecast figures for the main          #
# portion of the dashboard.                                                    #
#------------------------------------------------------------------------------# 
          
        ####################################
        # Setting the orders of the models #
        ####################################
        important_models <- c("ARIMA", "GAM", "GLM", "Prophet")
        
        # Unique model names 
        all_models <- unique(dataFilteredFinal$model)  # Get all unique model names
          
        # Reorder the 'model' factor 
        dataFilteredFinal$model <- factor(dataFilteredFinal$model, 
                                          levels = c(important_models, setdiff(all_models, important_models)))
          
      
        ###########################
        # Creating the plot panel #
        ###########################
        panel <- ggplot(dataFilteredFinal, aes(x = dates, y = medianVar)) +
          facet_wrap(~ model, ncol = 4) +
          geom_ribbon(aes(ymin = LBVar, ymax = UBVar), fill = ribbonColor.input)+ # 95% PI ribbon
          geom_line(linetype = BlinetypeFinal, aes(x = dates, y = UBVar), size = boundWidth.input, color = BLineColor) + # UB
          geom_line(linetype = BlinetypeFinal, aes(x = dates, y = LBVar), size = boundWidth.input, color = BLineColor) + # LB
          geom_line(color = MLineColor, size = MLineWidth, linetype = linetypeFinal) + # Median line
          geom_point(aes(x = dates, y = dataVar), color = dotColorData, shape = 1, size = as.numeric(sizeOfDataPoint)) + # Data points
          geom_vline(xintercept = breakLine, linetype = "dashed") + # Vertical line
          xAxisBreaks + # X axis breaks (i.e., dates)
          scale_y_continuous(breaks = seq(start, maxValue + (breaks.graph), by = breaks.graph), # Y-axis breaks
                             limits = c(start, maxValue + breaks.graph)) +  # Y-axis limits
          labs(title = "", # Title
               y = yAxisLabel, # Y-axis labels
               x = XAxisLabel) + # X-axis labels
          theme_classic() + # Base theme
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = xAxisTickSize), # Switching x-axis labels horizontal
                axis.title.y = element_text(size = YAxisLabelSize, face = YFaceLabelFinal), # Y-axis label
                axis.title.x = element_text(size = xAxisLabelSize), # X-Axis Label
                axis.text.y = element_text(size = YAxisTickSize, face = XFaceLabelFinal), # Y-Axis tick options 
                panel.grid.major = element_line(color = "grey95")) # Background grid lines 

        ####################################
        # Saving the plot in the main list #
        ####################################
        finalList[[listIndex]] <- panel
        
        # Adding the name 
        names(finalList)[listIndex] <- paste0(locationsUnqiue[l], "-", forecastPeriodsUnique[d], "-Calibration-", calibrationPeriodUnique[c], " (", quantileSelection, "% PI)")
        
        # Subtracting one from the index 
        listIndex <- listIndex-1
        
        }
        
      } # End of locations loop 
      
    } # End of forecast period date loop
    
  } # End of calibration loop 
  
  #################################
  # Returning the list of figures #
  #################################
  
  # Removing NA
  finalList <- finalList[!sapply(finalList, is.null)]
    
  return(finalList)
  
} # End of function
        







    

  