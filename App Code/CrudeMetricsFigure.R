#------------------------------------------------------------------------------#
#                                                                              #
#                            Crude Metrics Figures                             #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function plots the crude metrics calculated in another function, and    #
# exports them to the dashboard. The dashboard takes in the crude metrics,     #
# either the model fit metrics or the forecast performance metrics along with  #
# user customization selections from the user. It only plots the MSE, MAE, WIS #
# and PI for each of the models. Additionally, the figure that outputs depends #
# on the number of forecast period dates selected by the user. If only one     #
# date is selected, bar-charts are outputted. If multiple forecast period      #
# dates is selected, time series figures are exported.                         #
#------------------------------------------------------------------------------#
#                       Author: Amanda Bleichrodt                              #
#------------------------------------------------------------------------------#
CrudeMetricsFigure <- function(crudeMetrics, dateType, scaleY.input, 
                               MSELabel.input,  MAELabel.input,  WISLabel.input,
                               PILabel.input, YAxisLabelSize.input,
                               YAxisLabelFace.input, YAxisTickSize.input,
                               YAxisBreaks.input, YAxisStart.input, 
                               xAxisLabel.input,xCommonLabel.input, 
                               XAxisLabelSize.input, XAxisLabelFace.input, 
                               XAxisTickSize.input, xAxisBreaks.input,
                               showLegend.input, legendPosition.input ,
                               ARIMAColor.input, GLMColor.input, 
                               GAMColor.input, ProphetColor.input,
                               showBarNum.input, barNumSize.input) {
  
  ########################################
  # Reading in the list of crude metrics #
  ########################################
  crude.metric.input.TP <- crudeMetrics
  
  ####################################
  # Reading in the type of date data #
  ####################################
  dateType.TP <- dateType 
  
  ################################################
  # Reading in the list of variables to be log10 #
  ################################################
  logMetrics <- scaleY.input
  
  ############################
  # Reading in the MSE label #
  ############################
  MSELabel <- MSELabel.input
  
  ############################
  # Reading in the MAE label #
  ############################
  MAELabel <- MAELabel.input
  
  ############################
  # Reading in the WIS label #
  ############################
  WISLabel <- WISLabel.input
  
  ###########################
  # Reading in the PI label #
  ###########################
  PILabel <- PILabel.input
  
  ####################################
  # Reading in the y-axis label size #
  ####################################
  yLabelSize <- YAxisLabelSize.input
  
  ####################################
  # Reading in the y-axis label face #
  ####################################
  yLabelFace <- YAxisLabelFace.input
  
  ###################################
  # Reading in the y-axis tick size #
  ###################################
  yTickSize <- YAxisTickSize.input
  
  #######################################
  # Reading in the y-axis breaks amount #
  #######################################
  yBreaks <- YAxisBreaks.input
  
  #####################################
  # Reading in the y-axis start point #
  #####################################
  yStart <- YAxisStart.input
  
  ###############################
  # Reading in the x-axis label #
  ###############################
  xAxisLabel <- xAxisLabel.input
  
  ######################################
  # Reading in the common label choice #
  ######################################
  commonXAxis <- xCommonLabel.input
  
  ####################################
  # Reading in the x-axis label size #
  ####################################
  xLabelSize <- XAxisLabelSize.input
  
  ####################################
  # Reading in the x-axis label face #
  ####################################
  xLabelFace <- XAxisLabelFace.input
  
  ###################################
  # Reading in the x-axis tick size #
  ###################################
  xAxisTickSize <- XAxisTickSize.input
  
  ######################################
  # Reading in the x-axis breaks value #
  ######################################
  xDatebreaks <- xAxisBreaks.input
  
  #####################################
  # Reading in the show legend choice #
  #####################################
  showLegend <- showLegend.input
  
  #########################################
  # Reading in the legend position choice #
  #########################################
  legendPos <- legendPosition.input
  
  ##################################
  # Reading in the color for ARIMA #
  ##################################
  ARIMAColor <- ARIMAColor.input
  
  ################################
  # Reading in the color for GLM #
  ################################
  GLMColor <- GLMColor.input
  
  ################################
  # Reading in the color for GAM #
  ################################
  GAMColor <- GAMColor.input
  
  ####################################
  # Reading in the color for Prophet #
  ####################################
  ProphetColor <- ProphetColor.input
  
  ######################################
  # Reading in the show numbers choice #
  ######################################
  showNumber <- showBarNum.input
  
  ###########################################
  # Reading in the size for the bar numbers #
  ###########################################
  barSize <- barNumSize.input
  
  #################################
  # Creating the list for figures #
  #################################
  figureFinalList <- list()
  
  ##################################
  # Creating the `not-in` function #
  ##################################
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  
#------------------------------------------------------------------------------#
# Breaking loop if does not contain needed metrics -----------------------------
#------------------------------------------------------------------------------#
# About: This section breaks the function if MSE, MAE, WIS, or PI is not       #
# available.                                                                   #
#------------------------------------------------------------------------------#
  
  if(all(c("MSE", "MAE", "WIS", "PI") %!in% colnames(crude.metric.input.TP))){
    
    print("worked")
    return("ERROR")
    
  }
  
  
#------------------------------------------------------------------------------#
# Creating the face-type variable (Y) ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the variable hosting the 'face' specification    #
# for the y-axis label. While the user has all available options, this code    #
# fixes the name to match the argument needed for ggplot.                      #
#------------------------------------------------------------------------------#
  
  YFaceLabelFinal <- switch(yLabelFace,
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
  
  XFaceLabelFinal <- switch(xLabelFace,
                            "Plain" = "plain",
                            "Bold" = "bold",
                            "Italic" = "italic", 
                            "Bold & Italic" = "bold.italic") 
  
#------------------------------------------------------------------------------#
# Preparing the legend ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if the legend should be shown or not on the   #
# the figures. The default is for the legend to be shown. Additionally, it     #
# reformat the location options to match that needed for 'patchwork'.          #
#------------------------------------------------------------------------------#
  
  ##################################################
  # Creating the formatted legend position options #
  ##################################################
  correctLeg <- switch (legendPos,
                        "Top" = "top",
                        "Bottom" = "bottom",
                        "Left" = "left",
                        "Right" = "right"
  )

  ##########################
  # Show the figure legend #
  ##########################
  if(showLegend | is.null(showLegend)){
    
    legendPosition <- correctLeg
    
  #################################
  # Do not show the figure legend #
  #################################
  }else{
    
    legendPosition <- "none"
    
  }
  

#------------------------------------------------------------------------------#
# Preparing for the figure loop ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares for the figure loop. It creates the vectors of  #
# unique values to loop through (i.e., location, calibration). Additionally,   #
# it cleans up the dates in the data and creates the indicator to determine    #
# which figure to produce.                                                     #
#------------------------------------------------------------------------------#
  
  #####################
  # List of locations #
  #####################
  locationUnique <- unique(crude.metric.input.TP$Location)
  
  ######################################
  # List of calibration period lengths #
  ######################################
  calibrationUnique <- unique(crude.metric.input.TP$Calibration)
  
  #######################################
  # Cleaning up the dates: Week and Day #
  #######################################
  if(dateType.TP %in% c("week", "day")){
    
    # Fixing the date format
    data <- crude.metric.input.TP %>%
      dplyr::mutate(Date = anytime::anydate(Date))
    
  ##############################################
  # Cleaning up the dates: Year and Time Index #
  ##############################################
  }else{
    
    # Fixing the date format
    data <- crude.metric.input.TP %>%
      dplyr::mutate(Date = as.numeric(Date))
    
  }
  
  #################################
  # Creating the figure indicator #
  #################################
  
  # Length of forecast dates
  forecastDatesLengths <- length(unique(crude.metric.input.TP$Date))
  
  # Creating the indicator
  if(forecastDatesLengths == 1){
    
    singleGraph <- 1
    
  }else{
    
    singleGraph <- 0
    
  }
  
  #########################################
  # Creating the indicator for list index #
  #########################################
  indexPoint <- length(locationUnique) * length(calibrationUnique)
  
#------------------------------------------------------------------------------#
# Creating the loop to produce figure(s) ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each location-calibration period length    #
# combination to produce a list of metrics figures. If only one forecast date  #
# is selected, a bar-chart will be created. If multiple forecast dates are     #
# selected, a line plot will be created.                                       #
#------------------------------------------------------------------------------#
  
  ##############################
  # Starting the location loop #
  ##############################
  for(i in 1:length(locationUnique)){
    
    ########################################
    # Starting the calibration period loop #
    ########################################
    for(c in 1:length(calibrationUnique)){
      
      ####################
      # Indexed data set #
      ####################
      indexedData <- data %>%
        dplyr::filter(Location == locationUnique[i], # Filtering locations
                      Calibration == calibrationUnique[c]) %>% # Filtering calibration 
        dplyr::select(Location, Model, Date, Calibration, MSE, MAE, PI, WIS) %>% # Selecting needed rows 
        na.omit() # Removing non-needed rows
      
      
#------------------------------------------------------------------------------#
# Creating the log-10 values ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if the log-10 or un-transformed metrics       #
# should be plotted. The users can select which values they wish to show in    #
# in log-10 form.                                                              #
#------------------------------------------------------------------------------#
      
      ##################################
      # Calculating the log-10 metrics #
      ##################################
      indexedData <- indexedData %>%
        dplyr::mutate(Log10.MSE = log10(MSE + 1),
                      Log10.MAE = log10(MAE + 1),
                      Log10.WIS = log10(WIS + 1),
                      Log10.PI = log10(PI + 1))
      
      ###############################################
      # Determining which variables to proceed with #
      ###############################################
      if(is.null(logMetrics)){
        
      finalData <- data.frame(Location = c(indexedData$Location),
                              Model = c(indexedData$Model),
                              Date = c(indexedData$Date),
                              Calibration = c(indexedData$Calibration),
                              MSE = indexedData$MSE,
                              MAE = indexedData$MAE,
                              WIS = indexedData$WIS,
                              PI = indexedData$PI)
      }else{
        
        finalData <- data.frame(Location = c(indexedData$Location),
                                Model = c(indexedData$Model),
                                Date = c(indexedData$Date),
                                Calibration = c(indexedData$Calibration),
                                MSE = ifelse("MSE" %in% c(logMetrics), indexedData$Log10.MSE, indexedData$MSE),
                                MAE = ifelse("MAE" %in% c(logMetrics), indexedData$Log10.MAE, indexedData$MAE),
                                WIS = ifelse("WIS" %in% c(logMetrics), indexedData$Log10.WIS, indexedData$WIS),
                                PI = ifelse("PI" %in% c(logMetrics), indexedData$Log10.PI, indexedData$PI))
        
      }
      
#------------------------------------------------------------------------------#
# Determining the starting point for the y-axis --------------------------------
#------------------------------------------------------------------------------#
# About: This section determines whether the start the y-axis at zero or the   #
# minimum value of the data set. It creates an indicator which is used         #
# in a later section.                                                          #
#------------------------------------------------------------------------------#
      
      ########################################################
      # Runs if the user selects to start the y-axis at zero #
      ########################################################
      if(yStart == "Zero" || is.null(yStart)){
        
        # Start value: MSE
        start.MSE <- 0
        
        # Start value: MAE
        start.MAE <- 0
        
        # Start value: WIS
        start.WIS <- 0
        
        # Start value: PI
        start.PI <- 0
        
      ################################################################
      # Runs if the user does not select to start the y-axis at zero #
      ################################################################
      }else{
        
        # Start value: MSE
        start.MSE <- min(finalData$MSE, na.rm = T)
        
        # Start value: MAE
        start.MAE <- min(finalData$MAE, na.rm = T)
        
        # Start value: WIS
        start.WIS <- min(finalData$WIS, na.rm = T)
        
        # Start value: PI
        start.PI <- min(finalData$PI, na.rm = T)
        
      } 
      
#------------------------------------------------------------------------------#
# Adjusting the y-axis based on the user settings ------------------------------
#------------------------------------------------------------------------------#
# About: This section adjusts the y-axis based upon the user settings. This    #
# includes but is not limited to adjusting the scale of the axis, the start    #
# point, and the number of axis breaks. The setting apply depend on the        #
# figure used.                                                                 #
#------------------------------------------------------------------------------#
      
      ##############################################################
      # Adjusting the y-axis - determining the max y value for MSE #
      ##############################################################
      maxValue.MSE <- max(finalData$MSE, na.rm = T)
      
      ################################################
      # Determining the breaks in the y-axis for MSE #
      ################################################
      
      # Break calculation
      breakCalculation.MSE <- maxValue.MSE/yBreaks
      
      # Determining the number of breaks
      breaks.MSE <- case_when(
        
        breakCalculation.MSE > 1 ~ floor(breakCalculation.MSE),
        breakCalculation.MSE <= 1 ~ round(breakCalculation.MSE, 3)
        
      )
      
      ##############################################################
      # Adjusting the y-axis - determining the max y value for MAE #
      ##############################################################
      maxValue.MAE <- max(finalData$MAE, na.rm = T)
      
      ################################################
      # Determining the breaks in the y-axis for MAE #
      ################################################
      
      # Break calculation
      breakCalculation.MAE <- maxValue.MAE/yBreaks
      
      # Determining the number of breaks
      breaks.MAE <- case_when(
        
        breakCalculation.MAE > 1 ~ floor(breakCalculation.MAE),
        breakCalculation.MAE <= 1 ~ round(breakCalculation.MAE, 3), 
        
      )
      
      ##############################################################
      # Adjusting the y-axis - determining the max y value for WIS #
      ##############################################################
      maxValue.WIS <- max(finalData$WIS, na.rm = T)
      
      ################################################
      # Determining the breaks in the y-axis for WIS #
      ################################################
      
      # Break calculation
      breakCalculation.WIS <- maxValue.WIS/yBreaks
      
      # Determining the number of breaks
      breaks.WIS <- case_when(
        
        breakCalculation.WIS > 1 ~ floor(breakCalculation.WIS),
        breakCalculation.WIS <= 1 ~ round(breakCalculation.WIS, 3)
        
      )
      
      #############################################################
      # Adjusting the y-axis - determining the max y value for PI #
      #############################################################
      maxValue.PI <- max(finalData$PI, na.rm = T)
      
      ###############################################
      # Determining the breaks in the y-axis for PI #
      ###############################################
      
      # Break calculation
      breakCalculation.PI <- maxValue.PI/yBreaks
      
      # Determining the number of breaks
      breaks.PI <- case_when(
        
        breakCalculation.PI > 1 ~ floor(breakCalculation.PI),
        breakCalculation.PI > 0 ~ round(breakCalculation.PI, 3),
        breakCalculation.PI == 0 ~ round(10, 2)
        
      )
      
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
      if(dateType.TP %in% c("week")){
        
        # Formatting the dates 
        finalData <- finalData %>%
          mutate(dates = anytime::anydate(Date)) 
        
        ##################################################
        # Creating the date breaks: Nothing is Specified #
        ##################################################
        if(xDatebreaks < 1 || is.na(xDatebreaks) || is.null(xDatebreaks)){
          
          breaksLabel <- paste0(1, " week")
          
        ###############################################################
        # Creating the date breaks: Unique dates breaks are specified #
        ###############################################################
        }else{
          
          breaksLabel <- paste0(xDatebreaks, " weeks")
          
        }
        
        ############################################
        # Creating the statement for X-Axis breaks #
        ############################################
        xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(finalData$dates)), max(anydate(finalData$dates)), by = breaksLabel))  # X-axis breaks
        
      ###################################################
      # Cleaning up the dates in the X-Axis: Daily data #
      ###################################################
      }else if(dateType.TP == "day"){
        
        # Formatting the dates 
        finalData <- finalData %>%
          mutate(dates = anytime::anydate(Date)) 
        
        ##################################################
        # Creating the date breaks: Nothing is Specified #
        ##################################################
        if(xDatebreaks < 1 || is.na(xDatebreaks) || is.null(xDatebreaks)){
          
          breaksLabel <- paste0(1, " day")
          
        ###############################################################
        # Creating the date breaks: Unique dates breaks are specified #
        ###############################################################
        }else{
          
          breaksLabel <- paste0(xDatebreaks, " days")
          
        }
        
        ############################################
        # Creating the statement for X-Axis breaks #
        ############################################
        xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(finalData$dates)), max(anydate(finalData$dates)), by = breaksLabel))  # X-axis breaks
        
      ################################################################
      # Cleaning up the dates in the X-Axis: Year or Time Index data #
      ################################################################
      }else{
        
        # Formatting the dates 
        finalData <- finalData %>%
          mutate(dates = as.numeric(Date)) 

        ##################################################
        # Creating the date breaks: Nothing is Specified #
        ##################################################
        if(xDatebreaks < 1 || is.na(xDatebreaks) || is.null(xDatebreaks)){
          
          breaksLabel <- 1
          
        ###############################################################
        # Creating the date breaks: Unique dates breaks are specified #
        ###############################################################
        }else{
          
          breaksLabel <- as.numeric(xDatebreaks)
          
        }
        
        ############################################
        # Creating the statement for X-Axis breaks #
        ############################################
        xAxisBreaks <- scale_x_continuous(breaks = seq(min(finalData$dates, na.rm = T), max(finalData$dates, na.rm = T), by = breaksLabel))  # X-axis breaks
        
      } # End of statements formatting the X-Axis
      
#------------------------------------------------------------------------------#
# Determining if the numbers should be shown on top of the bars ----------------
#------------------------------------------------------------------------------#
# About: This section determines whether the numbers should be shown on the    #
# top of the bar-chart bars. Additionally, it determines their size and rounds #
# each value to two decimal points.                                            #
#------------------------------------------------------------------------------#
      
      ###########################
      # Showing the bar-numbers #
      ###########################
      if(showNumber || is.null(showNumber)){
        
        # MSE
        numTextMSE <- geom_text(aes(label = round(MSE, 2)), 
                             position = position_dodge(width = 0.9), 
                             vjust = -0.5,
                             size = barSize) 
        
        # MAE
        numTextMAE <- geom_text(aes(label = round(MAE, 2)), 
                                position = position_dodge(width = 0.9), 
                                vjust = -0.5,
                                size = barSize) 
        
        # WIS
        numTextWIS <- geom_text(aes(label = round(WIS, 2)), 
                                position = position_dodge(width = 0.9), 
                                vjust = -0.5,
                                size = barSize) 
        
        # PI
        numTextPI <- geom_text(aes(label = round(PI, 2)), 
                                position = position_dodge(width = 0.9), 
                                vjust = -0.5,
                                size = barSize) 
        
      ###############################
      # Not showing the bar-numbers #
      ###############################
      }else{
        
        # MSE
        numTextMSE <- NULL
        
        # MAE
        numTextMAE <- NULL
        
        # WIS
        numTextWIS <- NULL
        
        # PI
        numTextPI <- NULL
        
      }
      
#------------------------------------------------------------------------------#
# Plotting the figures with the crude metrics: Single plot ---------------------
#------------------------------------------------------------------------------#
# About: This section plots the bar graph when only a single date is           #
# selected. It also creates the title that will be assigned to the figure in   #
# the main dashboard.                                                          #
#------------------------------------------------------------------------------#
      
      ################################################
      # Running if the single-graph indicator is one #
      ################################################
      if(singleGraph == 1){
        
        ###############################
        # Creating the bar-chart: MSE #
        ###############################
        MSEFig <- ggplot(data = finalData, aes(x = Model, y = MSE, fill = Model)) +
          geom_col(position = position_dodge()) +
          scale_y_continuous(limits = c(0, maxValue.MSE), breaks = seq(0, maxValue.MSE + breaks.MSE, by = breaks.MSE), expand = expansion(mult = c(0, 0.2))) + 
          scale_fill_manual(values = c("ARIMA" = ARIMAColor, "GLM" = GLMColor, "GAM" = GAMColor, "Prophet" = ProphetColor))  +
          labs(y = MSELabel,
               x = "") + 
          numTextMSE + 
          theme_minimal() +
          theme(axis.title.y = element_text(size = yLabelSize, face = YFaceLabelFinal),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(size = xLabelSize, face = XFaceLabelFinal), 
                axis.text.x = element_text(size = xAxisTickSize), 
                legend.position = legendPosition)

        ###############################
        # Creating the bar-chart: MAE #
        ###############################
        MAEFig <- ggplot(data = finalData, aes(x = Model, y = MAE, fill = Model)) +
          geom_col(position = position_dodge()) +
          scale_y_continuous(limits = c(0, maxValue.MAE), breaks = seq(0, maxValue.MAE + breaks.MAE, by = breaks.MAE), expand = expansion(mult = c(0, 0.2))) +
          scale_fill_manual(values = c("ARIMA" = ARIMAColor, "GLM" = GLMColor, "GAM" = GAMColor, "Prophet" = ProphetColor))  +
          labs(y = MAELabel,
               x = "") +
          numTextMAE + 
          theme_minimal() +
          theme(axis.title.y = element_text(size = yLabelSize, face = YFaceLabelFinal),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(size = xLabelSize, face = XFaceLabelFinal), 
                axis.text.x = element_text(size = xAxisTickSize), 
                legend.position = legendPosition)

        ###############################
        # Creating the bar-chart: WIS #
        ###############################
        WISFig <- ggplot(data = finalData, aes(x = Model, y = WIS, fill = Model)) +
          geom_col(position = position_dodge()) +
          scale_y_continuous(limits = c(0, maxValue.WIS), breaks = seq(0, maxValue.WIS + breaks.WIS, by = breaks.WIS), expand = expansion(mult = c(0, 0.2))) + 
          scale_fill_manual(values = c("ARIMA" = ARIMAColor, "GLM" = GLMColor, "GAM" = GAMColor, "Prophet" = ProphetColor))  +
          labs(y = WISLabel,
               x = xAxisLabel) + 
          numTextWIS + 
          theme_minimal() +
          theme(axis.title.y = element_text(size = yLabelSize, face = YFaceLabelFinal),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(size = xLabelSize, face = XFaceLabelFinal), 
                axis.text.x = element_text(size = xAxisTickSize), 
                legend.position = legendPosition)

        ##############################
        # Creating the bar-chart: PI #
        ##############################
        PIFig <- ggplot(data = finalData, aes(x = Model, y = PI, fill = Model)) +
          geom_col(position = position_dodge()) +
          scale_y_continuous(limits = c(0,  maxValue.PI), breaks = seq(0, maxValue.PI, by = breaks.PI), expand = expansion(mult = c(0, 0.2))) +
          scale_fill_manual(values = c("ARIMA" = ARIMAColor, "GLM" = GLMColor, "GAM" = GAMColor, "Prophet" = ProphetColor))  +
          labs(y = PILabel,
               x = xAxisLabel) + 
          numTextPI + 
          theme_minimal() +
          theme(axis.title.y = element_text(size = yLabelSize, face = YFaceLabelFinal),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(size = xLabelSize, face = XFaceLabelFinal), 
                axis.text.x = element_text(size = xAxisTickSize), 
                legend.position = legendPosition)
        
        #############################
        # Creating the figure title #
        #############################
        titleFigure <- paste0("Crude-Metrics-", locationUnique[i], "-Calibration-", calibrationUnique[c], "-", max(finalData$Date))
        
        }else{
#------------------------------------------------------------------------------#
# Plotting the figures with the crude metrics: Time-series plot ----------------
#------------------------------------------------------------------------------#
# About: This section plots the time series figure when multiple forecast      #
# dates are selected. It also creates the title that will be assigned to the   #
# figure in the main dashboard.                                                #                                                          
#------------------------------------------------------------------------------#
          
          ######################################
          # Creating the time series plot: MSE #
          ######################################
          MSEFig <- ggplot(data = finalData, aes(x = dates, y = MSE, color = Model, linetype = Model)) +
            geom_line() +
            scale_y_continuous(limits = c(start.MSE, maxValue.MSE), breaks = seq(start.MSE, maxValue.MSE + breaks.MSE, by = breaks.MSE)) + 
            scale_color_manual(values = c("ARIMA" = ARIMAColor, "GLM" = GLMColor, "GAM" = GAMColor, "Prophet" = ProphetColor))  +
            xAxisBreaks +
            labs(y = MSELabel,
                 x = xAxisLabel) + 
            theme_minimal() +
            theme(axis.title.y = element_text(size = yLabelSize, face = YFaceLabelFinal),
                  axis.text.y = element_text(size = yTickSize),
                  axis.title.x = element_text(size = xLabelSize, face = XFaceLabelFinal), 
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = xAxisTickSize),
                  legend.position = legendPosition)
          
          ######################################
          # Creating the time series plot: MAE #
          ######################################
          MAEFig <- ggplot(data = finalData, aes(x = dates, y = MAE, color = Model, linetype = Model)) +
            geom_line() +
            scale_y_continuous(limits = c(start.MAE, maxValue.MAE), breaks = seq(start.MAE, maxValue.MAE + breaks.MAE, by = breaks.MAE)) + 
            scale_color_manual(values = c("ARIMA" = ARIMAColor, "GLM" = GLMColor, "GAM" = GAMColor, "Prophet" = ProphetColor))  +
            xAxisBreaks + 
            labs(y = MAELabel,
                 x = xAxisLabel) + 
            theme_minimal() +
            theme(axis.title.y = element_text(size = yLabelSize, face = YFaceLabelFinal),
                  axis.text.y = element_text(size = yTickSize),
                  axis.title.x = element_text(size = xLabelSize, face = XFaceLabelFinal), 
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = xAxisTickSize),
                  legend.position = legendPosition)
          
          ######################################
          # Creating the time series plot: WIS #
          ######################################
          WISFig <- ggplot(data = finalData, aes(x = dates, y = WIS, color = Model, linetype = Model)) +
            geom_line() +
            scale_y_continuous(limits = c(start.WIS, maxValue.WIS), breaks = seq(start.WIS, maxValue.WIS + breaks.WIS, by = breaks.WIS))  + 
            scale_color_manual(values = c("ARIMA" = ARIMAColor, "GLM" = GLMColor, "GAM" = GAMColor, "Prophet" = ProphetColor))  +
            xAxisBreaks + 
            labs(y = WISLabel, 
                 x = "") + 
            theme_minimal() +
            theme(axis.title.y = element_text(size = yLabelSize, face = YFaceLabelFinal),
                  axis.text.y = element_text(size = yTickSize),
                  axis.title.x = element_text(size = xLabelSize, face = XFaceLabelFinal), 
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = xAxisTickSize),
                  legend.position = legendPosition)
          
          #####################################
          # Creating the time series plot: PI #
          #####################################
          PIFig <- ggplot(data = finalData, aes(x = dates, y = PI, color = Model, linetype = Model)) +
            geom_line() +
            scale_y_continuous(limits = c(start.PI, maxValue.PI), breaks = seq(start.PI,  maxValue.PI, by = breaks.PI)) +
            scale_color_manual(values = c("ARIMA" = ARIMAColor, "GLM" = GLMColor, "GAM" = GAMColor, "Prophet" = ProphetColor))  +
            xAxisBreaks + 
            labs(y = PILabel,
                 x = "") + 
            theme_minimal() +
            theme(axis.title.y = element_text(size = yLabelSize, face = YFaceLabelFinal),
                  axis.text.y = element_text(size = yTickSize),
                  axis.title.x = element_text(size = xLabelSize, face = XFaceLabelFinal), 
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = xAxisTickSize),
                  legend.position = legendPosition)
          
          #############################
          # Creating the figure title #
          #############################
          titleFigure <- paste0("Crude-Metrics-", locationUnique[i], "-Calibration-", calibrationUnique[c])
          
        }
      
#------------------------------------------------------------------------------#
# Creating the final plot ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the final plot of four graphs that is saved in   #
# the list of calibration-location specific figures. Additionally, it adds the #
# titles which will show in the main dashboard.                                #
#------------------------------------------------------------------------------#
      
      ##############################################
      # Combining the figures: Common X-Axis Label #
      ##############################################
      if(commonXAxis){
        
        combined_plot <- (MSEFig + MAEFig + WISFig + PIFig) +
          plot_layout(guides = 'collect') &
          theme(
            legend.position = legendPosition) +
          plot_layout(axis_titles = "collect")
        
        #################################################
        # Combining the figures: No common X-Axis Label #
        #################################################
        }else{
          
          
          combined_plot <- (MSEFig + MAEFig + WISFig + PIFig) +
            plot_layout(guides = 'collect') &
            theme(
              legend.position = legendPosition)
  
        }

      
#------------------------------------------------------------------------------#
# Saving the figure to the list ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section saves the combined plot and associated title to the      #
# list that will be outputted to the main dashboard.                           #
#------------------------------------------------------------------------------#
      
      #################################
      # Saving the figure to the list #
      #################################
      figureFinalList[[indexPoint]] <- combined_plot
    
      ####################
      # Saving the title #
      ####################
      names(figureFinalList)[indexPoint] <- titleFigure
      
      #######################
      # Adjusting the index #
      #######################
      indexPoint <- indexPoint - 1
      
    } # End of calibration loop 
    
  } # End of location loop 
  
  #################################
  # Returning the list of figures #
  #################################
  return(figureFinalList)
   
}



