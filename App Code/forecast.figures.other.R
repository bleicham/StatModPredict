#------------------------------------------------------------------------------#
#                                                                              #
#                            Plotting Figures                                  #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file takes the vetted forecast files read into the model comparison     #
# page, and then produced forecast figures for each file. The forecast figure  #
# follows the same format as for the ARIMA, GLM, GAM and Prophet figures.      #
# Additionally, the same figure formatting options are available.              #
#------------------------------------------------------------------------------#
#                         Author: Amanda Bleichrodt                            #
#------------------------------------------------------------------------------#
forecast.figures.other <- function(formatted.forecast.input, data.type.input,
                             smoothing.input, scaleYAxis.input, yAxisLabel.input, 
                             dateBreaks.input, startYPoint.input, 
                             dotSize.input, linetype.input, lineColor.input,
                             lineWidth.input, dotColor.input, boundtype.input,
                             boundWidth.input, boundColor.input,
                             ribbonColor.input, yLabelSize.input, yLabelFace.input,
                             yTickSize.input, yTickBreaks.input, xAxisLabel.input,
                             xAxisLabelSize.input, xAxisLabelFace.input,
                             xAxisTickSize.input){

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
  
  #############
  # Smoothing #
  #############
  smoothing.input.figure <<- smoothing.input
  
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
  
  #############
  # Dot color #
  #############
  dotColorData <<- dotColor.input
  
  ####################
  # Median line type #
  ####################
  MLinetype <<- linetype.input
  
  #####################
  # Median line color #
  #####################
  MLineColor <<- lineColor.input
  
  #####################
  # Median line width #
  #####################
  MLineWidth <<- lineWidth.input
  
  ###################
  # Bound line type #
  ###################
  BLineType <<- boundtype.input
  
  ####################
  # Bound line width #
  ####################
  BLineWidth <<- boundWidth.input
  
  ####################
  # Bound line color #
  ####################
  BLineColor <<- boundColor.input
  
  #####################
  # Y-Axis Label size #
  #####################
  YAxisLabelSize <<- yLabelSize.input
  
  #####################
  # Y-Axis Label face #
  #####################
  YAxisLabelFace <<- yLabelFace.input
  
  ####################
  # Y-Axis Tick Size #
  ####################
  YAxisTickSize <<- yTickSize.input
  
  #################
  # Y-Axis Breaks #
  #################
  YAxisBreaks <<- yTickBreaks.input
  
  ################
  # X-Axis Label #
  ################
  XAxisLabel <<- xAxisLabel.input
  
  #####################
  # X-Axis Label Size #
  #####################
  xAxisLabelSize <<- xAxisLabelSize.input
  
  #####################
  # X-Axis Label Face #
  #####################
  xAxisLabelFace <<- xAxisLabelFace.input
  
  ####################
  # X-Axis Tick Size #
  ####################
  xAxisTickSize <<- xAxisTickSize.input
  
  ###########################
  # Creating the empty list #
  ###########################
  figureList <- list()


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
  
  ##################
  # Default Option #
  ##################
  if(dotSizeData == 2 || is.null(dotSizeData) || is.null(dotSizeData) || dotSizeData == 0){
    
    sizeOfDataPoint <- 2
    
  #########################
  # User specified Option #
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
  if(is.null(smoothing.input.figure)){
    
    smoothingIndicator <- 0
    
  ############################################
  # Smoothing input indicator - No Smoothing #
  ############################################
  }else if(0 <= smoothing.input.figure & smoothing.input.figure <= 1){
    
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
# About: This section pulls the information to format the data for plotting,   #
# determining information about the indexed forecast (i.e., model name,        #
# location, and calibration period size), and then sets up all of the custom   #
# options for graphing. Finally, the loop creates both the dynamic             #
# figures which are outputted as a list.                                       #
#------------------------------------------------------------------------------#
  for(i in 1:length(formatted.forecast.Figure)){
  
    ################################################
    # Determining the name of the indexed forecast #
    ################################################
    nameIndex <- names(formatted.forecast.Figure[i])
    
    #####################################
    # Pulling the necessary information #
    #####################################
  
    #####################################################
    # Determining the forecast period date: Week or Day #
    #####################################################
    if(date.Figure %in% c("week", "day")){
      
      # Determining the forecast period date from the name
      forecastPeriod <- anytime::anydate(paste0(strsplit(nameIndex, "[-]")[[1]][8], "-", strsplit(nameIndex, "[-]")[[1]][9], "-", strsplit(nameIndex, "[-]")[[1]][10]))
      
    ############################################################
    # Determining the forecast period date: Year or Time Index #
    ############################################################
    }else{
      
      # Determining the forecast period date from the name
      forecastPeriod <- strsplit(strsplit(nameIndex, "[-]")[[1]][8], "[.]")[[1]][1]
      
    }
    
    ################################
    # Handling the NAs in the data #
    ################################
    data.for.plot <- formatted.forecast.Figure[[i]]
      
#------------------------------------------------------------------------------#
# Adjusting the scale of the y-axis and number of breaks -----------------------
#------------------------------------------------------------------------------#
# About: This section adjusts the scale of the y-axis based upon the user's    #
# choice or lack of choice in the main dashboard. Users can either select the  #
# original or log-10 scale. Additionally, this section determines the          #
# minimum, maximum, and number of breaks in the y-axis.                        #
#------------------------------------------------------------------------------#
  
    ######################################################
    # Runs if the user selects to use the original scale #
    ######################################################
    if(scaleIndicator == 0){
      
      ##################################
      # Variable to use for the y-axis #
      ##################################
      data.for.plot$medianVar <- data.for.plot$median
      
      ##########################
      # Variable to use for UB #
      ##########################
      data.for.plot$UBVar <- data.for.plot$UB
      
      ##########################
      # Variable to use for LB #
      ##########################
      data.for.plot$LBVar <- data.for.plot$LB
      
      ############################
      # Variable to use for data #
      ############################
      data.for.plot$dataVar <- data.for.plot$data
      
      ######################################################
      # Adjusting the y-axis - determining the max y value #
      ######################################################
      maxValue <- max(data.for.plot[,-1], na.rm = T)
      
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
      minValue <- floor(min(data.for.plot[,-1], na.rm = T))
      
      ##############################################
      # Handling INF: May happen with some figures #
      ##############################################
      if(maxValue == Inf){
        
        # Saving an NA
        figureList[[i]] <- NA
        
        # Adding name to list element
        names(figureList)[i] <- nameIndex
        
        # Next loop
        next
        
      }
      
    ############################################
    # Runs if the selects to use the log-scale #
    ############################################
    }else{
      
      ###############################################
      # Adding the log-transformed variable: Median #
      ###############################################
      data.for.plot$logMedian <- log10(data.for.plot$median + 1)
      
      ###########################################
      # Adding the log-transformed variable: UB #
      ###########################################
      data.for.plot$logUB <- log10(data.for.plot$UB + 1)
      
      ###########################################
      # Adding the log-transformed variable: LB #
      ###########################################
      data.for.plot$logLB <- log10(data.for.plot$LB + 1)
      
      #############################################
      # Adding the log-transformed variable: Data #
      #############################################
      data.for.plot$logData <- log10(data.for.plot$data + 1)
      
      ##########################################
      # Variable to use for the y-axis: Median #
      ##########################################
      data.for.plot$medianVar <- data.for.plot$logMedian
      
      ##########################
      # Variable to use for UB #
      ##########################
      data.for.plot$UBVar <- data.for.plot$logUB
      
      ##########################
      # Variable to use for LB #
      ##########################
      data.for.plot$LBVar <- data.for.plot$logLB
      
      ############################
      # Variable to use for data #
      ############################
      data.for.plot$dataVar <- data.for.plot$logData
      
      ######################################################
      # Adjusting the y-axis - determining the max y value #
      ######################################################
      maxValue <- max(data.for.plot[,-c(1:5)], na.rm = T)
      
      ####################################################################
      # Min value of y-axis: Used if user does not want to start at zero #
      ####################################################################
      minValue <- floor(min(data.for.plot[,-c(1:5)], na.rm = T))
      
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
      
      ##############################################
      # Handling INF: May happen with some figures #
      ##############################################
      if(maxValue == Inf){
        
        # Saving an NA
        figureList[[i]] <- NA
        
        # Adding name to list element
        names(figureList)[i] <- nameIndex
        
        # Next loop
        next
        
      } # End of 'INF'
      
    } # End of 'log scale'
 
#------------------------------------------------------------------------------#
# Determining the starting point for the y-axis --------------------------------
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
# Preparing the X-Axis ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the needed information for the X-Axis.          #
# Specifically, depending on if working with weekly, daily, yearly or time     #
# index data, the date is formatted and the x-axis breaks are created. The     #
# user has some options available as it relates to the structure of the        #
# X-Axis, such as the number of breaks.                                        #
#------------------------------------------------------------------------------#
    
    ####################################################
    # Cleaning up the dates in the X-Axis: Weekly data #
    ####################################################
    if(date.Figure %in% c("week")){
    
      # Formatting the dates 
      data.for.plot <- data.for.plot %>%
        mutate(dates = anytime::anydate(Date)) 
    
      ##############################
      # Creating the vertical line #
      ##############################
      breakLine <- anytime::anydate(forecastPeriod)
    
      ##################################################
      # Creating the date breaks: Nothing is Specified #
      ##################################################
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
        
        breaksLabel <- paste0(1, " week")
        
      ###############################################################
      # Creating the date breaks: Unique dates breaks are specified #
      ###############################################################
      }else{
        
        breaksLabel <- paste0(dateBreaks, " weeks")
        
      }
    
      ############################################
      # Creating the statement for X-Axis breaks #
      ############################################
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(data.for.plot$dates)), max(anydate(data.for.plot$dates)), by = breaksLabel))  # X-axis breaks
    
    ###################################################
    # Cleaning up the dates in the X-Axis: Daily data #
    ###################################################
    }else if(date.Figure == "day"){
      
      # Formatting the dates 
      data.for.plot <- data.for.plot %>%
        mutate(dates = anytime::anydate(Date)) 
      
      ##############################
      # Creating the vertical line #
      ##############################
      breakLine <- anytime::anydate(forecastPeriod)
      
      ##################################################
      # Creating the date breaks: Nothing is Specified #
      ##################################################
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
        
        breaksLabel <- paste0(1, " day")
        
      ###############################################################
      # Creating the date breaks: Unique dates breaks are specified #
      ###############################################################
      }else{
        
        breaksLabel <- paste0(dateBreaks, " days")
        
      }
      
      ############################################
      # Creating the statement for X-Axis breaks #
      ############################################
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(data.for.plot$dates)), max(anydate(data.for.plot$dates)), by = breaksLabel))  # X-axis breaks
      
    ################################################################
    # Cleaning up the dates in the X-Axis: Year or Time Index data #
    ################################################################
    }else{

      # Formatting the dates 
      data.for.plot <- data.for.plot %>%
        mutate(dates = as.numeric(Date)) 
      
      ##############################
      # Creating the vertical line #
      ##############################
      breakLine <- as.numeric(forecastPeriod)
      
      ##################################################
      # Creating the date breaks: Nothing is Specified #
      ##################################################
      if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
        
        breaksLabel <- 1
        
      ###############################################################
      # Creating the date breaks: Unique dates breaks are specified #
      ###############################################################
      }else{
        
        breaksLabel <- as.numeric(dateBreaks)
        
      }
      
      ############################################
      # Creating the statement for X-Axis breaks #
      ############################################
      xAxisBreaks <- scale_x_continuous(breaks = seq(min(data.for.plot$dates, na.rm = T), max(data.for.plot$dates, na.rm = T), by = breaksLabel))  # X-axis breaks
      
    } # End of statements formatting the X-Axis
    
#------------------------------------------------------------------------------#
# Plotting the individual figures  ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the individual forecast figures for the main     #
# portion of the dashboard.                                                    #
#------------------------------------------------------------------------------#
  
    ######################
    # Plotting the graph #
    ######################
    individual.figure <- ggplot(data.for.plot, aes(x = dates, y = medianVar)) +
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
    figureList[[i]] <- individual.figure
    
    # Adding name to list element
    names(figureList)[i] <- nameIndex
  
  } # End of loop

  ######################
  # Returning the list #
  ######################
  return(figureList)

} # End of function


