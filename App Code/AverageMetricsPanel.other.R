#------------------------------------------------------------------------------#
#                                                                              #
#                       Tile Panels for Average Metrics                        #
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
AverageMetricsPanel.other <- function(avgMetrics.input, dateType.input, scaleY.input,
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
# Looping through calibration periods and metric type --------------------------
#------------------------------------------------------------------------------#
# About: This section starts the loop that goes through unique calibration     #
# period lengths and metric types and subsets accordingly.                     #
#------------------------------------------------------------------------------#
  
  #############################################
  # List of unique calibration period lengths #
  #############################################
  listCalibration <- c(unique(crude.metric.input.AM$Calibration))
  
  ###############################
  # List of unique metric types #
  ###############################
  listType <- c(unique(crude.metric.input.AM$Type))
  
  # Index for figure list
  j = 1
  
  #################################
  # Starting the calibration loop #
  #################################
  for(i in 1:length(listCalibration)){
    
    ##########################
    # Starting the type loop #
    ##########################
    for(c in 1:length(listType)){
    
      ########################
      # Sub-setting the data #
      ########################
      dataFigure <- crude.metric.input.AM %>%
        dplyr::filter(Calibration == listCalibration[i],
                      Type == listType[c])
      
      ########################
      # Skipping to next row #
      ########################
      if(nrow(dataFigure) == 0){
        
        next
        
      }
      
      # Removing columns with NAs
      dataFigure <-  dataFigure[, !apply(dataFigure, 2, function(x) all(is.na(x)))]
    
    #############################
    # List of available metrics #
    #############################
    availableMetrics <- c(unique(colnames(dataFigure)[-c(1:4)]))
    
#------------------------------------------------------------------------------#
# Plotting the Metric ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the indexed metrics as a tile plot, taking into    #
# consideration any user-specifications. Additionally, it removes any missing  #
# rows from the data to ensure the figure shows only available metrics for     #
# either the average model fit or forecast performance. It creates the figures #
# via a loop.                                                                  #
#------------------------------------------------------------------------------#
      
      #####################
      # Starting the loop #
      #####################
      for(k in 1:length(availableMetrics)){
        
        #############################
        # Removing the rows with NA #
        #############################
        temp.data <- dataFigure %>%
          dplyr::select(Type, Location, Model, Calibration, availableMetrics[[k]]) %>%
          na.omit()
        
        # Renaming the last column
        colnames(temp.data)[5] <- "temp.metric"
        
        ##############################################################
        # Determining if the log-10 or normal metric should be shown #
        ##############################################################
        
        # Determining the metric without `average` before it
        metricPlain <- trimws(str_split(availableMetrics[[k]], "[.]")[[1]][2], which = "left")
        
        ############################
        # Producing the log metric #
        ############################
        if(metricPlain %in% c(logMetrics) & !is.null(logMetrics)){
          
          # Creating the data 
          temp.data <- temp.data %>%
            dplyr::mutate(temp.metricFinal = log10(temp.metric + 1))
          
          # Legend label 
          legendLabel <- paste0(metricPlain, " (Log 10)")
          
        ###########################
        # Using the normal metric #
        ###########################
        }else{
          
          # Creating the data
          temp.data <- temp.data %>%
            dplyr::mutate(temp.metricFinal = temp.metric)
          
          # Legend label
          legendLabel <- metricPlain
          
        }
        
        ####################################
        # Setting the orders of the models #
        ####################################
        important_models <- c("ARIMA", "GAM", "GLM", "Prophet")
        
        # Unique model names 
        all_models <- unique(temp.data$Model)  # Get all unique model names
        
        # Reorder the 'model' factor 
        temp.data$Model <- factor(temp.data$Model, 
                                          levels = c(important_models, setdiff(all_models, important_models)))
        
        ##########################
        # Handling legend breaks #
        ##########################
        if(nrow(temp.data) == 1){
          
          # Setting the legend break to 1
          legendBreaks.temp <- 1
          
          #####################################
          # Keeping the default legend breaks #
          #####################################
        }else{
          
          # Setting the legend break to 1
          legendBreaks.temp <- legendBreaks
          
        }

        ############################
        # TryCatch for color issue #
        ############################
        tryCatch({
          
        ##############################################
        # Plotting the average figure: Show the text #
        ##############################################
        if(!is.null(showText) & showText){
          
          Figure <- ggplot(data = temp.data, aes(x = Model, y = fct_rev(Location), fill = temp.metricFinal)) +
            geom_tile(color = outlineColor) +
            geom_text(aes(label = round(temp.metricFinal, 2)), color = textColor, size = textSize) +
            scale_fill_gradient(low = lowColor, high = highColor, 
                                breaks = seq(min(temp.data$temp.metricFinal), max(temp.data$temp.metricFinal), length.out = legendBreaks.temp),
                                labels = round(seq(min(temp.data$temp.metricFinal), max(temp.data$temp.metricFinal), length.out = legendBreaks.temp), 2)) +
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
          
          Figure <- ggplot(data = temp.data, aes(x = Model, y = fct_rev(Location), fill = temp.metricFinal)) +
            geom_tile(color = outlineColor) +
            scale_fill_gradient(low = lowColor, high = highColor, 
                                breaks = seq(min(temp.data$temp.metricFinal), max(temp.data$temp.metricFinal), length.out = legendBreaks.temp),
                                labels = round(seq(min(temp.data$temp.metricFinal), max(temp.data$temp.metricFinal), length.out = legendBreaks.temp), 2)) +
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
        figureFinalList[[j]] <- Figure
        
        # Adding the title
        names(figureFinalList)[j] <- paste0(availableMetrics[k], "-", listType[c], "-Calibration-", listCalibration[[i]]) 
        
        # Adding one to the index
        j <- j + 1
        
        #################################################
        # Returning nothing if the colors have an issue #
        #################################################
        }, error = function(e){
          
          Figure <- NULL
          
        })
        
      } # End of loop for metrics
      
    } # End of `Type` loop
    
  } # End of `Calibration` loop
        
  ############################
  # Returning the final list #
  ############################
  return(figureFinalList)

}

    
   
