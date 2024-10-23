#------------------------------------------------------------------------------#
#                                                                              #
#                         Tile Panels for Average Metrics                      #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function plots the average metrics for both model fit and forecast      #
# performance as a tile plot. The code is receptive to missing data, thus      #
# if a certain metric can not be calculated the code will still execute. The   #
# users have many customization options, including the color of the tiles, the #
# labels, and other feature related to the legend.                             #
#------------------------------------------------------------------------------#
#                         Author: Amanda Bleichrodt                            #                                  
#------------------------------------------------------------------------------#
AverageMetricsPanel <- function(avgMetrics.input, dateType.input, scaleY.input,
                                lowColor.input, highColor.input, outlineColor.input,
                                textColor.input, showText.input, legendBreaks.input,
                                textSize.input, legendLabelSize.input,
                                legendTickSize.input, showLegend.input,
                                legendPosition.input, yAxisLabel.input,
                                yAxisLabelFace.input, yAxisLabelSize.input,
                                yAxisTickSize.input, xAxisLabel.input,
                                xAxisLabelFace.input, xAxisLabelSize.input,
                                xAxisTickSize.input){
  
  ########################################
  # Reading in the list of crude metrics #
  ########################################
  crude.metric.input.AM <- avgMetrics.input
  
  ####################################
  # Reading in the type of date data #
  ####################################
  dateType.AM <- dateType.input
  
  ################################################
  # Reading in the list of variables to be log10 #
  ################################################
  logMetrics <- scaleY.input
  
  #############
  # Low color #
  #############
  lowColor <- lowColor.input
  
  ##############
  # High color #
  ##############
  highColor <- highColor.input
  
  #################
  # Outline color #
  #################
  outlineColor <- outlineColor.input
  
  #################
  # Legend breaks #
  #################
  legendBreaks <- legendBreaks.input
  
  #############
  # Show text #
  #############
  showText <- showText.input
  
  ##############
  # Text color #
  ##############
  textColor <- textColor.input
  
  #############
  # Text Size #
  #############
  textSize <- textSize.input
  
  #####################
  # Legend label size #
  #####################
  legendLabelSize <- legendLabelSize.input
  
  ####################
  # Legend tick size #
  ####################
  legendTickSize <- legendTickSize.input
  
  ###################
  # Show the legend #
  ###################
  showLegend <- showLegend.input
  
  ###################
  # Legend position #
  ###################
  legPos <- legendPosition.input
  
  ################
  # Y-Axis Label #
  ################
  yLabel <- yAxisLabel.input
  
  #####################
  # Y-Axis Label Face #
  #####################
  yLabelFace <- yAxisLabelFace.input
  
  #####################
  # Y-Axis Label Size #
  #####################
  yLabelSize <- yAxisLabelSize.input
  
  ####################
  # Y-Axis Tick Size #
  ####################
  yTickSize <- yAxisTickSize.input
  
  ################
  # X-Axis Label #
  ################
  xLabel <- xAxisLabel.input
  
  #####################
  # X-Axis Label Face #
  #####################
  xLabelFace <- xAxisLabelFace.input
  
  #####################
  # X-Axis Label Size #
  #####################
  xLabelSize <- xAxisLabelSize.input
  
  ####################
  # X-Axis Tick Size #
  ####################
  xTickSize <- xAxisTickSize.input
  
  #################################
  # Creating the list for figures #
  #################################
  figureFinalList <- list()
  
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
  
  xFaceLabelFinal <- switch(xLabelFace,
                            "Plain" = "plain",
                            "Bold" = "bold",
                            "Italic" = "italic", 
                            "Bold & Italic" = "bold.italic") 
  
#------------------------------------------------------------------------------#
# Creating the legend indicator ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section re-formats the options for the legend position. The      #
# ggplot theme element requires lower case, therefore this section takes the   #
# upper case entry from the user and shifts it to lower case. Additionally,    #
# it lets the code know if the legend should be shown or not.                  #
#------------------------------------------------------------------------------#
  
  ##################################################
  # Creating the formatted legend position options #
  ##################################################
  correctLeg <- switch (legPos,
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
# Looping through calibration period loops -------------------------------------
#------------------------------------------------------------------------------#
# About: This section starts the loop that goes through unique calibration     #
# period lengths, and subsets the data accordingly.                            #
#------------------------------------------------------------------------------#
  
  # List of unique calibration period lengths 
  listCalibration <- c(unique(crude.metric.input.AM$Calibration))
  
  # Index for figure list
  j = 1
  
  #####################
  # Starting the loop #
  #####################
  for(i in 1:length(listCalibration)){
    
    ########################
    # Sub-setting the data #
    ########################
    dataFigure <- crude.metric.input.AM %>%
      dplyr::filter(Calibration == listCalibration[i])
    
    # Removing columns with NAs
    dataFigure <- dataFigure[, !apply(dataFigure, 2, function(x) all(is.na(x)))]
    
    #############################
    # List of available metrics #
    #############################
    availableMetrics <- c(unique(colnames(dataFigure)))
    

#------------------------------------------------------------------------------#
# Plotting the Mean Squared Error ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the MSE as a tile plot, taking into consideration  #
# any user-specifications. Additionally, it removes any missing rows from the  #
# data to ensure that the figure shows only available MSE metrics for either   #
# the average model fit or forecast performance.                               #
#------------------------------------------------------------------------------#
  
    ##########################################
    # Running if the MSE metric is available #
    ##########################################
    if("MSE" %in% c(availableMetrics)){
      
      #############################
      # Removing the rows with NA #
      #############################
      MSE.data <- dataFigure %>%
        dplyr::select(Location, Model, Calibration, MSE) %>%
        na.omit()
      
      ###########################################################
      # Determining if the log-10 or normal MSE should be shown #
      ###########################################################
      if("MSE" %in% c(logMetrics) & !is.null(logMetrics)){
        
        # Creating the data 
        MSE.data <- MSE.data %>%
          dplyr::mutate(finalMSE = log10(MSE + 1))
        
        # Legend label 
        legendLabel <- "MSE (Log-10)"
        
      }else{
        
        # Creating the data
        MSE.data <- MSE.data %>%
          dplyr::mutate(finalMSE = MSE)
        
        # Legend label
        legendLabel <- "MSE"
        
      }
      
      ##########################
      # Handling legend breaks #
      ##########################
      if(nrow(MSE.data) == 1){
        
        # Setting the legend break to 1
        legendBreaks.MSE <- 1
        
      #####################################
      # Keeping the default legend breaks #
      #####################################
      }else{
        
        # Setting the legend break to 1
        legendBreaks.MSE <- legendBreaks
        
      }
      
      ############################
      # TryCatch for color issue #
      ############################
      tryCatch({
      
      ##############################################
      # Plotting the average figure: Show the text #
      ##############################################
      if(!is.null(showText) & showText){
        
        MSE.Figure <- ggplot(data = MSE.data, aes(x = Model, y = fct_rev(Location), fill = finalMSE)) +
          geom_tile(color = outlineColor) +
          geom_text(aes(label = round(finalMSE, 2)), color = textColor, size = textSize) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(MSE.data$finalMSE), max(MSE.data$finalMSE), length.out = legendBreaks.MSE),
                              labels = round(seq(min(MSE.data$finalMSE), max(MSE.data$finalMSE), length.out = legendBreaks.MSE), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      #############################################
      # Plotting the average figure: Show No text #
      #############################################
      }else{
        
        MSE.Figure <- ggplot(data = MSE.data, aes(x = Model, y = fct_rev(Location), fill = finalMSE)) +
          geom_tile(color = outlineColor) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(MSE.data$finalMSE), max(MSE.data$finalMSE), length.out = legendBreaks.MSE),
                              labels = round(seq(min(MSE.data$finalMSE), max(MSE.data$finalMSE), length.out = legendBreaks.MSE), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      }
        
      #################################
      # Adding the figure to the list #
      #################################
      figureFinalList[[j]] <- MSE.Figure
      
      # Adding the title
      names(figureFinalList)[j] <- paste0("MSE-Calibration-", listCalibration[[i]]) 
      
      # Adding one to the index
      j <- j + 1
      
      
      #################################################
      # Returning nothing if the colors have an issue #
      #################################################
      }, error = function(e){
        
        MSE.Figure <- NULL
        
      })
      
    ########################
    # Null holder for list #
    ########################
    }else{
      
      MSE.Figure <- NULL
      
    }
  
#------------------------------------------------------------------------------#
# Plotting the Mean Absolute Error ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the MAE as a tile plot, taking into consideration  #
# any user-specifications. Additionally, it removes any missing rows from the  #
# data to ensure that the figure shows only available MAE metrics for either   #
# the average model fit or forecast performance.                               #
#------------------------------------------------------------------------------#
  
    ##########################################
    # Running if the MAE metric is available #
    ##########################################
    if("MAE" %in% c(availableMetrics)){
      
      #############################
      # Removing the rows with NA #
      #############################
      MAE.data <- dataFigure %>%
        dplyr::select(Location, Model, Calibration, MAE) %>%
        na.omit()
      
      ###########################################################
      # Determining if the log-10 or normal MAE should be shown #
      ###########################################################
      if("MAE" %in% c(logMetrics) & !is.null(logMetrics)){
        
        # Creating the data
        MAE.data <- MAE.data %>%
          dplyr::mutate(finalMAE = log10(MAE + 1))
        
        # Legend label 
        legendLabel <- "MAE (Log-10)"
        
      }else{
        
        # Creating the data
        MAE.data <- MAE.data %>%
          dplyr::mutate(finalMAE = MAE)
        
        # Legend label 
        legendLabel <- "MAE"
        
      }
      
      ##########################
      # Handling legend breaks #
      ##########################
      if(nrow(MAE.data) == 1){
        
        # Setting the legend break to 1
        legendBreaks.MAE <- 1
        
      #####################################
      # Keeping the default legend breaks #
      #####################################
      }else{
        
        # Setting the legend break to 1
        legendBreaks.MAE <- legendBreaks
        
      }
      
      ############################
      # TryCatch for color issue #
      ############################
      tryCatch({
      
      ##############################################
      # Plotting the average figure: Show the text #
      ##############################################
      if(!is.null(showText) & showText){
        
        MAE.Figure <- ggplot(data = MAE.data, aes(x = Model, y = fct_rev(Location), fill = finalMAE)) +
          geom_tile(color = outlineColor) +
          geom_text(aes(label = round(finalMAE, 2)), color = textColor, size = textSize) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(MAE.data$finalMAE), max(MAE.data$finalMAE), length.out = legendBreaks.MAE),
                              labels = round(seq(min(MAE.data$finalMAE), max(MAE.data$finalMAE), length.out = legendBreaks.MAE), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      #############################################
      # Plotting the average figure: Show No text #
      #############################################
      }else{
        
        MAE.Figure <- ggplot(data = MAE.data, aes(x = Model, y = fct_rev(Location), fill = finalMAE)) +
          geom_tile(color = outlineColor) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(MAE.data$finalMAE), max(MAE.data$finalMAE), length.out = legendBreaks.MAE),
                              labels = round(seq(min(MAE.data$finalMAE), max(MAE.data$finalMAE), length.out = legendBreaks.MAE), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      }
        

      #################################
      # Adding the figure to the list #
      #################################
      figureFinalList[[j]] <- MAE.Figure
      
      # Adding the title
      names(figureFinalList)[j] <- paste0("MAE-Calibration-", listCalibration[[i]]) 
      
      # Adding one to the index
      j <- j + 1
      
      #################################################
      # Returning nothing if the colors have an issue #
      #################################################
      }, error = function(e){
        
        MAE.Figure <- NULL
        
      })
      
      
    ########################
    # Null holder for list #
    ########################
    }else{
      
      MAE.Figure <- NULL
    
    }

#------------------------------------------------------------------------------#
# Plotting the Prediction Interval Coverage ------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the PI coverage as a tile plot, taking into        #
# consideration any user-specifications. Additionally, it removes any missing  #
# rows from the data to ensure that the figure shows only available PI metrics #
# for either the average model fit or forecast performance.                    #
#------------------------------------------------------------------------------#
  
    #########################################
    # Running if the PI metric is available #
    #########################################
    if("PI" %in% c(availableMetrics)){
      
      #############################
      # Removing the rows with NA #
      #############################
      PI.data <- dataFigure %>%
        dplyr::select(Location, Model, Calibration, PI) %>%
        na.omit()
      
      ##########################################################
      # Determining if the log-10 or normal PI should be shown #
      ##########################################################
      if("PI" %in% c(logMetrics) & !is.null(logMetrics)){
        
        # Creating the data 
        PI.data <- PI.data %>%
          dplyr::mutate(finalPI = log10(PI + 1))
        
        # Legend label 
        legendLabel <- "PI Coverage (Log-10)"
        
      }else{
        
        # Creating the data
        PI.data <- PI.data %>%
          dplyr::mutate(finalPI = PI)
        
        # Legend label 
        legendLabel <- "PI Coverage"
        
      }
      
      ##########################
      # Handling legend breaks #
      ##########################
      if(nrow(PI.data) == 1){
        
        # Setting the legend break to 1
        legendBreaks.PI <- 1
        
      #####################################
      # Keeping the default legend breaks #
      #####################################
      }else{
        
        # Setting the legend break to 1
        legendBreaks.PI <- legendBreaks
        
      }
      
      ############################
      # TryCatch for color issue #
      ############################
      tryCatch({
      
      ##############################################
      # Plotting the average figure: Show the text #
      ##############################################
      if(!is.null(showText) & showText){
        
        PI.Figure <- ggplot(data = PI.data, aes(x = Model, y = fct_rev(Location), fill = finalPI)) +
          geom_tile(color = outlineColor) +
          geom_text(aes(label = round(finalPI, 2)), color = textColor, size = textSize)  +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(PI.data$finalPI), max(PI.data$finalPI), length.out = legendBreaks.PI),
                              labels = round(seq(min(PI.data$finalPI), max(PI.data$finalPI), length.out = legendBreaks.PI), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      #############################################
      # Plotting the average figure: Show No text #
      #############################################
      }else{
        
        PI.Figure <- ggplot(data = PI.data, aes(x = Model, y = fct_rev(Location), fill = finalPI)) +
          geom_tile(color = outlineColor) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(PI.data$finalPI), max(PI.data$finalPI), length.out = legendBreaks.PI),
                              labels = round(seq(min(PI.data$finalPI), max(PI.data$finalPI), length.out = legendBreaks.PI), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      }
      

      #################################
      # Adding the figure to the list #
      #################################
      figureFinalList[[j]] <- PI.Figure
      
      # Adding the title
      names(figureFinalList)[j] <- paste0("PI-Calibration-", listCalibration[[i]]) 
    
      # Adding one to the index
      j <- j + 1
      
      #################################################
      # Returning nothing if the colors have an issue #
      #################################################
      }, error = function(e){
        
        PI.Figure <- NULL
        
      })
      
      
    ########################
    # Null holder for list #
    ########################
    }else{
      
      PI.Figure <- NULL
      
    }
      
#------------------------------------------------------------------------------#
# Plotting the Weighted Interval Score -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the WIS as a tile plot, taking into consideration  #
# any user-specifications. Additionally, it removes any missing rows from the  #
# data to ensure that the figure shows only available WIS metrics for either   #
# the average model fit or forecast performance.                               #
#------------------------------------------------------------------------------#
  
    ##########################################
    # Running if the WIS metric is available #
    ##########################################
    if("WIS" %in% c(availableMetrics)){
      
      #############################
      # Removing the rows with NA #
      #############################
      WIS.data <- dataFigure %>%
        dplyr::select(Location, Model, Calibration, WIS) %>%
        na.omit()
      
      ###########################################################
      # Determining if the log-10 or normal WIS should be shown #
      ###########################################################
      if("WIS" %in% c(logMetrics) & !is.null(logMetrics)){
        
        # Creating the data 
        WIS.data <- WIS.data %>%
          dplyr::mutate(finalWIS = log10(WIS + 1))
        
        # Legend label 
        legendLabel <- "WIS (Log-10)"
        
      }else{
        
        # Creating the data
        WIS.data <- WIS.data %>%
          dplyr::mutate(finalWIS = WIS)
        
        # Legend label 
        legendLabel <- "WIS"
        
      }
      
      ##########################
      # Handling legend breaks #
      ##########################
      if(nrow(WIS.data) == 1){
        
        # Setting the legend break to 1
        legendBreaks.WIS <- 1
        
      #####################################
      # Keeping the default legend breaks #
      #####################################
      }else{
        
        # Setting the legend break to 1
        legendBreaks.WIS <- legendBreaks
        
      }
      
      ############################
      # TryCatch for color issue #
      ############################
      tryCatch({
        
      ##############################################
      # Plotting the average figure: Show the text #
      ##############################################
      if(!is.null(showText) & showText){
        
        WIS.Figure <- ggplot(data = WIS.data, aes(x = Model, y = fct_rev(Location), fill = finalWIS)) +
          geom_tile(color = outlineColor) +
          geom_text(aes(label = round(finalWIS, 2)), color = textColor, size = textSize)  +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(WIS.data$finalWIS), max(WIS.data$finalWIS), length.out = legendBreaks.WIS),
                              labels = round(seq(min(WIS.data$finalWIS), max(WIS.data$finalWIS), length.out = legendBreaks.WIS), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      #############################################
      # Plotting the average figure: Show No text #
      #############################################
      }else{
        
        WIS.Figure <- ggplot(data = WIS.data, aes(x = Model, y = fct_rev(Location), fill = finalWIS)) +
          geom_tile(color = outlineColor) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(WIS.data$finalWIS), max(WIS.data$finalWIS), length.out = legendBreaks.WIS),
                              labels = round(seq(min(WIS.data$finalWIS), max(WIS.data$finalWIS), length.out = legendBreaks.WIS), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      }

      #################################
      # Adding the figure to the list #
      #################################
      figureFinalList[[j]] <- WIS.Figure
      
      # Adding the title
      names(figureFinalList)[j] <- paste0("WIS-Calibration-", listCalibration[[i]]) 
      
      # Adding one to the index
      j <- j + 1
      
      
      #################################################
      # Returning nothing if the colors have an issue #
      #################################################
      }, error = function(e){
        
        WIS.Figure <- NULL
        
      })
      
      
    ########################
    # Null holder for list #
    ########################
    }else{
      
      WIS.Figure <- NULL
      
    }
  
#------------------------------------------------------------------------------#
# Plotting the AICc ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the AICs as a tile plot, taking into consideration #
# any user-specifications. Additionally, it removes any missing rows from the  #
# data to ensure that the figure shows only available AICc metrics for either  #
# the average model fit or forecast performance.                               #
#------------------------------------------------------------------------------#
  
    ###########################################
    # Running if the AICc metric is available #
    ###########################################
    if("AICc" %in% c(availableMetrics)){
      
      #############################
      # Removing the rows with NA #
      #############################
      AICc.data <- dataFigure %>%
        dplyr::select(Location, Model, Calibration, AICc) %>%
        na.omit()
      
      ############################################################
      # Determining if the log-10 or normal AICc should be shown #
      ############################################################
      if("AICc" %in% c(logMetrics) & !is.null(logMetrics)){
        
        # Creating the data 
        AICc.data <- AICc.data %>%
          dplyr::mutate(finalAICc = log10(AICc + 1))
        
        # Legend label 
        legendLabel <- "AICc (Log-10)"
        
      }else{
        
        # Creating the data
        AICc.data <- AICc.data %>%
          dplyr::mutate(finalAICc = AICc)
        
        # Legend label
        legendLabel <- "AICc"
        
      }
      
      ############################
      # TryCatch for color issue #
      ############################
      tryCatch({
      
      ##############################################
      # Plotting the average figure: Show the text #
      ##############################################
      if(!is.null(showText) & showText){
        
        AICc.Figure <- ggplot(data = AICc.data, aes(x = Model, y = fct_rev(Location), fill = finalAICc)) +
          geom_tile(color = outlineColor) +
          geom_text(aes(label = round(finalAICc, 2)), color = textColor, size = textSize) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(AICc.data$finalAICc), max(AICc.data$finalAICc), length.out = legendBreaks),
                              labels = round(seq(min(AICc.data$finalAICc), max(AICc.data$finalAICc), length.out = legendBreaks), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      ##############################################
      # Plotting the average figure: Show No text #
      ##############################################
      }else{
        
        AICc.Figure <- ggplot(data = AICc.data, aes(x = Model, y = fct_rev(Location), fill = finalAICc)) +
          geom_tile(color = outlineColor) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(AICc.data$finalAICc), max(AICc.data$finalAICc), length.out = legendBreaks),
                              labels = round(seq(min(AICc.data$finalAICc), max(AICc.data$finalAICc), length.out = legendBreaks), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      }
        
      #################################
      # Adding the figure to the list #
      #################################
      figureFinalList[[j]] <- AICc.Figure
      
      # Adding the title
      names(figureFinalList)[j] <- paste0("AICc-Calibration-", listCalibration[[i]]) 
      
      # Adding one to the index
      j <- j + 1
      
      #################################################
      # Returning nothing if the colors have an issue #
      #################################################
      }, error = function(e){
        
        AICc.Figure <- NULL
        
      })
      
    ########################
    # Null holder for list #
    ########################
    }else{
      
      AICc.Figure <- NULL

    }
  
#------------------------------------------------------------------------------#
# Plotting the AIC -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the AIC as a tile plot, taking into consideration  #
# any user-specifications. Additionally, it removes any missing rows from the  #
# data to ensure that the figure shows only available AIC metrics for either   #
# the average model fit or forecast performance.                               #
#------------------------------------------------------------------------------#
  
    ##########################################
    # Running if the AIC metric is available #
    ##########################################
    if("AIC" %in% c(availableMetrics)){
      
      #############################
      # Removing the rows with NA #
      #############################
      AIC.data <- dataFigure %>%
        dplyr::select(Location, Model, Calibration, AIC) %>%
        na.omit()
      
      ###########################################################
      # Determining if the log-10 or normal AIC should be shown #
      ###########################################################
      if("AIC" %in% c(logMetrics) & !is.null(logMetrics)){
        
        # Creating the data 
        AIC.data <- AIC.data %>%
          dplyr::mutate(finalAIC = log10(AIC + 1))
        
        # Legend label 
        legendLabel <- "AIC (Log-10)"
        
      }else{
        
        # Creating the data
        AIC.data <- AIC.data %>%
          dplyr::mutate(finalAIC = AIC)
        
        # Legend label 
        legendLabel <- "AIC"
        
      }
      
      ############################
      # TryCatch for color issue #
      ############################
      tryCatch({
        
      ##############################################
      # Plotting the average figure: Show the text #
      ##############################################
      if(!is.null(showText) & showText){
        
        AIC.Figure <- ggplot(data = AIC.data, aes(x = Model, y = fct_rev(Location), fill = finalAIC)) +
          geom_tile(color = outlineColor) +
          geom_text(aes(label = round(finalAIC, 2)), color = textColor, size = textSize)  +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(AIC.data$finalAIC), max(AIC.data$finalAIC), length.out = legendBreaks),
                              labels = round(seq(min(AIC.data$finalAIC), max(AIC.data$finalAIC), length.out = legendBreaks), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      ##############################################
      # Plotting the average figure: Show No text #
      ##############################################
      }else{
        
        AIC.Figure <- ggplot(data = AIC.data, aes(x = Model, y = fct_rev(Location), fill = finalAIC)) +
          geom_tile(color = outlineColor) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(AIC.data$finalAIC), max(AIC.data$finalAIC), length.out = legendBreaks),
                              labels = round(seq(min(AIC.data$finalAIC), max(AIC.data$finalAIC), length.out = legendBreaks), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))

      }

      #################################
      # Adding the figure to the list #
      #################################
      figureFinalList[[j]] <- AIC.Figure
      
      # Adding the title
      names(figureFinalList)[j] <- paste0("AIC-Calibration-", listCalibration[[i]]) 
      
      # Adding one to the index
      j <- j + 1
      
      #################################################
      # Returning nothing if the colors have an issue #
      #################################################
      }, error = function(e){
        
        AIC.Figure <- NULL
        
      })
      
    ########################
    # Null holder for list #
    ########################
    }else{
      
      AIC.Figure <- NULL
      
    }
  
#------------------------------------------------------------------------------#
# Plotting the BIC -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the BIC as a tile plot, taking into consideration  #
# any user-specifications. Additionally, it removes any missing rows from the  #
# data to ensure that the figure shows only available BIC metrics for either   #
# the average model fit or forecast performance.                               #
#------------------------------------------------------------------------------#
  
    ##########################################
    # Running if the BIC metric is available #
    ##########################################
    if("BIC" %in% c(availableMetrics)){
      
      #############################
      # Removing the rows with NA #
      #############################
      BIC.data <- dataFigure %>%
        dplyr::select(Location, Model, Calibration, BIC) %>%
        na.omit()
      
      ###########################################################
      # Determining if the log-10 or normal BIC should be shown #
      ###########################################################
      if("BIC" %in% c(logMetrics) & !is.null(logMetrics)){
        
        # Creating the data 
        BIC.data <- BIC.data %>%
          dplyr::mutate(finalBIC = log10(BIC + 1))
        
        # Legend label 
        legendLabel <- "BIC (Log-10)"
        
      }else{
        
        # Creating the data
        BIC.data <- BIC.data %>%
          dplyr::mutate(finalBIC = BIC)
        
        # Legend label 
        legendLabel <- "BIC"
        
      }
      
      ############################
      # TryCatch for color issue #
      ############################
      tryCatch({
        
      ##############################################
      # Plotting the average figure: Show the text #
      ##############################################
      if(!is.null(showText) & showText){
        
        BIC.Figure <- ggplot(data = BIC.data, aes(x = Model, y = fct_rev(Location), fill = finalBIC)) +
          geom_tile(color = outlineColor) +
          geom_text(aes(label = round(finalBIC, 2)), color = textColor, size = textSize) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(BIC.data$finalBIC), max(BIC.data$finalBIC), length.out = legendBreaks),
                              labels = round(seq(min(BIC.data$finalBIC), max(BIC.data$finalBIC), length.out = legendBreaks), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      ##############################################
      # Plotting the average figure: Show No text #
      ##############################################
      }else{
        
        BIC.Figure <- ggplot(data = BIC.data, aes(x = Model, y = fct_rev(Location), fill = finalBIC)) +
          geom_tile(color = outlineColor) +
          scale_fill_gradient(low = lowColor, high = highColor, 
                              breaks = seq(min(BIC.data$finalBIC), max(BIC.data$finalBIC), length.out = legendBreaks),
                              labels = round(seq(min(BIC.data$finalBIC), max(BIC.data$finalBIC), length.out = legendBreaks), 2)) +
          labs(fill = legendLabel,
               y = yLabel,
               x = xLabel) +
          theme_classic() +
          theme(legend.title = element_text(size = legendLabelSize),
                legend.text = element_text(size = legendTickSize),
                legend.position = legendPosition,
                axis.title.y = element_text(face = YFaceLabelFinal, size = yLabelSize),
                axis.text.y = element_text(size = yTickSize),
                axis.title.x = element_text(face = xFaceLabelFinal, size = xLabelSize),
                axis.text.x = element_text(size = xTickSize))
        
      }

      #################################
      # Adding the figure to the list #
      #################################
      figureFinalList[[j]] <- BIC.Figure
      
      # Adding the title
      names(figureFinalList)[j] <- paste0("BIC-Calibration-", listCalibration[[i]]) 

      # Adding one to the index
      j <- j + 1
      
      
      #################################################
      # Returning nothing if the colors have an issue #
      #################################################
      }, error = function(e){
        
        BIC.Figure <- NULL
        
      })
      
      
    ########################
    # Null holder for list #
    ########################
    }else{
      
      BIC.Figure <- NULL
      
    }
    
  } # End of loop going through calibration periods 
    
  ############################
  # Returning the final list #
  ############################
  return(figureFinalList)

}

    
   
