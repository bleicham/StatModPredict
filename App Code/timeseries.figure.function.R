#------------------------------------------------------------------------------#
#                                                                              #
#                     Producing the time-series figures                        #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This function takes in the crude data and user selected locations. It #
# then determines the date composition, and filters the data to include the    #
# locations selected by the user. Additionally, the function accounts for the  #
# user specified time-series figure options and incorporates them into the     #
# exported figures. The function then outputs two figures, one for the plotly  #
# figure which shows in the dashboard, and one to be exported by the user.     #
#------------------------------------------------------------------------------#
#                         Author: Amanda Bleichrodt                            #
#------------------------------------------------------------------------------#
timeseries.figure.function <- function(timeseries.input, location.input,
                                       dateType.input, forecastLineShow,
                                       forecastDatesStart, forecastDatesEnd,
                                       scaleYAxis, yAxisLabel, dateBreaks,
                                       mainTitle, mainTitleFace, mainTitleSize,
                                       mainTitleOrien, yAxisTitleSize, 
                                       yAxisTitleFace, yAxisBreaks, 
                                       yAxisStartVal, yAxisTickSize, xAxisLabel,
                                       xAxisLabelSize, xAxisLabelFace, 
                                       xAxisTickSize
                                     ){
  
#------------------------------------------------------------------------------#
# Reading in the inputs --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names to be used for the function.                                           #
#------------------------------------------------------------------------------#

  ##########################
  # Time-series data input #
  ##########################
  timeseries.data <- timeseries.input
  
  #######################
  # Location data input #
  #######################
  locations <- location.input
  
  ###################
  # Date Type input #
  ###################
  dateType <- dateType.input
  
  ###############################################
  # Indicator if forecast lines should be shown #
  ###############################################
  lineIndicator <- forecastLineShow
  
  #########################
  # Start forecast period #
  #########################
  startForecastPeriod <- forecastDatesStart
  
  #######################
  # End forecast period #
  #######################
  EndForecastPeriod <- forecastDatesEnd
  
  ###################
  # Scale indicator #
  ###################
  scaleTimeseries <- scaleYAxis 
  
  ################
  # Y-axis label #
  ################
  yAxisInput <- yAxisLabel
  
  #####################
  # Date breaks input #
  #####################
  numOfDateBreaks <- dateBreaks
  
  ##############
  # Main title #
  ##############
  figTitle <- mainTitle
  
  ###################
  # Main title face #
  ###################
  figTitleFace <- mainTitleFace
  
  ###################
  # Main title size #
  ###################
  figTitleSize <- mainTitleSize
  
  ##########################
  # Main title orientation #
  ##########################
  figTitleOrien <- mainTitleOrien
  
  #####################
  # Y-Axis Title Size #
  #####################
  figYTitleSize <- yAxisTitleSize 
  
  #####################
  # Y-Axis Title face #
  #####################
  figYTitleFace <- yAxisTitleFace
  
  #################
  # Y-Axis Breaks #
  #################
  figYAxisBreaks <- (yAxisBreaks - 1)
  
  ################
  # Y-Axis Start #
  ################
  figYAxisStart <- yAxisStartVal
  
  ##########################
  # Y-Axis Tick Label Size #
  ##########################
  figYAxisTickSize <- yAxisTickSize
  
  ################
  # X-Axis Label #
  ################
  figXAxisLabel <- xAxisLabel
  
  #####################
  # X-Axis Label Size #
  #####################
  figXAxisLabelSize <- xAxisLabelSize
  
  #####################
  # X-Axis Label Face #
  #####################
  figXAxisLabelFace <- xAxisLabelFace
  
  ####################
  # X-Axis Tick Size #
  ####################
  figXAxisTickSize <- xAxisTickSize
  
  ####################
  # List for figures #
  ####################
  TimseriesList <- list()


#------------------------------------------------------------------------------#
# Fixing the scale for the time series data -------------------------------------
#------------------------------------------------------------------------------#
# About: This section handles the scale variable, and ensures that something   #
# is specified and can be used below.                                          #    
#------------------------------------------------------------------------------#
  
  ###########################################
  # Indicator for to use the original scale #
  ###########################################
  if(scaleTimeseries == "Original" || is.null(scaleTimeseries) || is.null(scaleTimeseries)){
    
    scaleIndicator <- 0
  
  #####################################
  # Indicator to use the log-10 scale #
  #####################################
  }else if(scaleTimeseries == "Log(Base 10)"){
    
    scaleIndicator <- 1
    
  }

#------------------------------------------------------------------------------#
# Filtering data based on selected locations -----------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the crude data to only choose the locations      #
# selected by the user.                                                        #
#------------------------------------------------------------------------------#

  ##################################
  # Setting a standard date header #
  ##################################
  names(timeseries.data)[1] <- "Dates"
  
  ##########################
  # Data from wide to long #
  ##########################
  data.for.plot <- timeseries.data %>%
    pivot_longer(-Dates, names_to = "Location", values_to = "Count")


#------------------------------------------------------------------------------#
# Formatting the y-axis start options ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section formats options for the start of the y-axis, either zero #
# or the minimum value in the data set.                                        #
#------------------------------------------------------------------------------#

  #################################################
  # Switch statement to determine the start value #
  #################################################
  figYAxisStartFormat <- switch(figYAxisStart,
    "Minimum Value in Data" = min(data.for.plot[-c(1:2)]),
    "Zero" = 0
  )

#------------------------------------------------------------------------------#
# Adjusting the scale of the y-axis --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adjusts the scale of the y-axis based upon the user's    #
# choice or lack of choice in the main dashboard. The default setting is the   #
# is the original scale. However, multiple options are available.              #
# Additionally, users can select to either start the data at zero or the min.  #
# value in the data and the number of breaks in the y-axis.                    #
#------------------------------------------------------------------------------#

  ######################################################
  # Runs if the user selects to use the original scale #
  ######################################################
  if(scaleIndicator == 0){
    
    ##################################
    # Variable to use for the y-axis #
    ##################################
    y_variable <- data.for.plot$Count
    
    ######################################################
    # Adjusting the y-axis - determining the max y value #
    ######################################################
    maxValue <- max(data.for.plot[,-c(1,2)], na.rm = T)
    
    ####################################
    # Determining breaks in the y-axis #
    ####################################
    
      # Break calculation
      breakCalculation <- maxValue/figYAxisBreaks
      
      # Determining the number of breaks 
      breaks.graph <- case_when(
        
        breakCalculation > 1 ~ floor(breakCalculation),
        breakCalculation <= 1 ~ round(breakCalculation, 2)
        
        ) 
      
  ############################################
  # Runs if the selects to use the log-scale #
  ############################################
  }else{
  
    #######################################
    # Adding the log-transformed variable #
    #######################################
    data.for.plot$logCount <- log10(data.for.plot$Count + 1)
    
    ##################################
    # Variable to use for the y-axis #
    ##################################
    y_variable <- data.for.plot$logCount
    
    ######################################################
    # Adjusting the y-axis - determining the max y value #
    ######################################################
    maxValue <- max(data.for.plot[,-c(1:3)], na.rm = T)
  
    ########################################
    # Determining the breaks in the y-axis #
    ########################################
    
    # Break calculation
    breakCalculation <- maxValue/figYAxisBreaks
    
    # Determining the number of breaks 
    breaks.graph <- case_when(
      
      breakCalculation > 1 ~ floor(breakCalculation),
      breakCalculation <= 1 ~ round(breakCalculation, 2)
      
    ) 
    
  } # End of if-else

#------------------------------------------------------------------------------#
# Handling dates ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the labels for the dates, including if the user #
# does not change the number of date breaks.                                   #
#------------------------------------------------------------------------------#

  ##########################
  # Handling dates - Weeks #
  ##########################
  if(dateType %in% c("week")){
    
      # Dates on x-axis
      data.for.plot <- data.for.plot %>%
        mutate(Dates = anytime::anydate(Dates)) # Handling dates if working with weekly data
      
      # Breaks
      if(numOfDateBreaks < 1 || is.na(numOfDateBreaks) || is.null(numOfDateBreaks)){
        
        breaksLabel <- paste0(1, " week")
        
      }else{
        
        breaksLabel <- paste0(numOfDateBreaks, " weeks")
        
      }
      
      # X-axis breaks 
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(data.for.plot$Dates), max(data.for.plot$Dates), by = breaksLabel))  # X-axis breaks
      
      # Creating the vector of line breaks
      lineBreaksVector <- seq.Date(anytime::anydate(startForecastPeriod), anytime::anydate(EndForecastPeriod), by = breaksLabel)

  #########################
  # Handling dates - Days #
  #########################
  }else if(dateType %in% c("day")){
  
    # Dates on x-axis
    data.for.plot <- data.for.plot %>%
      mutate(Dates = anytime::anydate(Dates)) # Handling dates if working with daily data
    
    # Breaks
    if(numOfDateBreaks < 1 || is.na(numOfDateBreaks) || is.null(numOfDateBreaks)){
      
      breaksLabel <- paste0(1, " day")
      
    }else{
      
      breaksLabel <- paste0(numOfDateBreaks, " days")
      
    }
    
    # X-axis breaks 
    xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(data.for.plot$Dates), max(data.for.plot$Dates), by = breaksLabel))  # X-axis breaks
    
    # Creating the vector of line breaks
    lineBreaksVector <- seq.Date(anytime::anydate(startForecastPeriod), anytime::anydate(EndForecastPeriod), by = breaksLabel)
    
  ##########################################
  # Handling dates - Years or Time Indexes #  
  ##########################################
  }else{
    
    # Dates on x-axis
    data.for.plot <- data.for.plot %>%
      mutate(Dates = as.numeric(Dates)) # Changing years and time index to numeric 
    
    # Breaks
    if(numOfDateBreaks < 1 || is.na(numOfDateBreaks) || is.null(numOfDateBreaks)){
      
      breaksLabel <- 1
      
    }else{
      
      breaksLabel <- as.numeric(numOfDateBreaks)
      
    }
    
    # X-axis breaks 
    xAxisBreaks <- scale_x_continuous(breaks = seq(min(data.for.plot$Dates), max(data.for.plot$Dates), by = breaksLabel))  # X-axis breaks
    
    # Creating the vector of line breaks
    lineBreaksVector <- seq(as.numeric(startForecastPeriod), as.numeric(EndForecastPeriod), by = breaksLabel)
    
  }

#------------------------------------------------------------------------------#
# Determining if the line breaks for forecast periods should be shown ----------
#------------------------------------------------------------------------------#
# About: This section determines if the line breaks should be shown on the     #
# time series figure, based on the users input.                                #
#------------------------------------------------------------------------------#

  ######################
  # Plotting the lines #
  ######################
  if(lineIndicator == F || is.null(lineIndicator)){
    
      plottedLines <- NULL
    
    ##########################
    # Not plotting the lines #
    ##########################
    }else{
    
      plottedLines <- geom_vline(xintercept  = lineBreaksVector, color = "black", linetype = "dashed") 
    
  }


#------------------------------------------------------------------------------#
# Formatting the main title face options ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section formats the user-input for the main title face to the    #
# corresponding option in the plot.                                            #
#------------------------------------------------------------------------------#

  ###############
  # Switch case #
  ###############
  mainTitleFaceFormat <- switch(figTitleFace,
    "Plain" = "plain",
    "Bold" = "bold",
    "Italic" = "italic",
    "Bold & Italic" = "bold.italic"
  )
  
#------------------------------------------------------------------------------#
# Formatting the main title orientation options --------------------------------
#------------------------------------------------------------------------------#
# About: This section formats the user-input for the main title orientation to #
# the corresponding option in the plot.                                        #
#------------------------------------------------------------------------------#
  
  ###############
  # Switch Case #
  ###############
  mainTitleOrienFormat <- switch (figTitleOrien,
                                  "Left" = 0,
                                  "Center" = 0.5,
                                  "Right" = 1
  )

#------------------------------------------------------------------------------#
# Formatting the y-axis label face options -------------------------------------
#------------------------------------------------------------------------------#
# About: This section formats the user-input for the y-axis label face to the  #
# corresponding option in the plot.                                            #
#------------------------------------------------------------------------------#
  
  ###############
  # Switch case #
  ###############
  figYTitleFaceFormat <- switch(figYTitleFace,
                                "Plain" = "plain",
                                "Bold" = "bold",
                                "Italic" = "italic",
                                "Bold & Italic" = "bold.italic"
  )
  
#------------------------------------------------------------------------------#
# Formatting the x-axis label face options -------------------------------------
#------------------------------------------------------------------------------#
# About: This section formats the user-input for the x-axis label face to the  #
# corresponding option in the plot.                                            #
#------------------------------------------------------------------------------#
  
  ###############
  # Switch case #
  ###############
  figXTitleFaceFormat <- switch(figXAxisLabelFace,
                                "Plain" = "plain",
                                "Bold" = "bold",
                                "Italic" = "italic",
                                "Bold & Italic" = "bold.italic"
  )
  
#------------------------------------------------------------------------------#
# Plotting the time series -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the data from above to plot the time series data    #
# for the locations selected by the user.                                      #
#------------------------------------------------------------------------------#

  #######################################
  # Figure to be outputted for 'plotly' #
  #######################################
  figure <- ggplot(data.for.plot, aes(x = Dates, y = y_variable, color = Location)) +
    plottedLines + # Plotting the vertical lines if selected
    geom_line(aes(group= 1, text = paste('Date: ', Dates,
                                         '<br>Count: ', Count))) + # Plotly labels
    xAxisBreaks + # X axis breaks (i.e., dates)
    scale_y_continuous(breaks = seq(figYAxisStartFormat, maxValue + breaks.graph, by = breaks.graph), # Y-axis breaks
                       limits = c(figYAxisStartFormat, maxValue)) + # Y-axis limits
    labs(color = "", # Removing lab title 
         y = yAxisInput, # Y-axis label 
         title = figTitle, # Main figure title 
         x = figXAxisLabel) + # X-axis label 
    theme_classic() + # Base theme
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = figXAxisTickSize), # Formatting the labels x-axis ticks 
          plot.title = element_text(hjust = mainTitleOrienFormat, face = mainTitleFaceFormat, size = figTitleSize), # Plot title
          axis.title.y = element_text(size = figYTitleSize, face = figYTitleFaceFormat), # Y-axis label
          axis.title.x = element_text(size = figXAxisLabelSize, face = figXTitleFaceFormat), # X-Axis label 
          axis.text.y = element_text(size = figYAxisTickSize)) # Formatting the labels y-axis ticks 
  
  #######################################
  # Figure to be outputted for 'ggplot' #
  #######################################
  figure1 <- ggplot(data.for.plot, aes(x = Dates, y = y_variable, color = Location)) +
    plottedLines + # Plotting the vertical lines if selected
    geom_line() + # Obtaining the lines 
    xAxisBreaks + # X axis breaks (i.e., dates)
    scale_y_continuous(breaks = seq(figYAxisStartFormat, maxValue + breaks.graph, by = breaks.graph), # Y-axis breaks
                       limits = c(figYAxisStartFormat, maxValue)) + # Y-axis limits
    labs(color = "", # Removing lab title 
         y = yAxisInput, # Y-axis label 
         title = figTitle, # Main figure title 
         x = figXAxisLabel) + # X-axis label 
    theme_classic() + # Base theme
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = figXAxisTickSize), # Formatting the labels x-axis ticks 
          plot.title = element_text(hjust = mainTitleOrienFormat, face = mainTitleFaceFormat, size = figTitleSize), # Plot title
          axis.title.y = element_text(size = figYTitleSize, face = figYTitleFaceFormat), # Y-axis label
          axis.title.x = element_text(size = figXAxisLabelSize, face = figXTitleFaceFormat), # X-Axis label 
          axis.text.y = element_text(size = figYAxisTickSize)) # Formatting the labels y-axis ticks 
  
  ###########################################
  # Exporting the list of time-series plots #
  ###########################################
  TimseriesList <- list(figure, figure1)
  
  ##################################################
  # Returning the list as a result of the function #
  ##################################################
  return(TimseriesList)

}