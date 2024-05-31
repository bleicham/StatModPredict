#------------------------------------------------------------------------------#
#                                                                              #
#                          Tile Panels for Metrics                             #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This section creates an average panel plot to show the average metric #
# plot for the given data. The y-axis is the locations, and the x-axis is the  #
# models. In total, there are four figures to one panels, where each panel     #
# corresponds to one metric. The function outputs a figure.                    #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
AverageMetricsPanel <- function(avgMetrics.input, dateType.input, scaleY.input,
                                lowColor.input, highColor.input, outlineColor.input,
                                textColor.input, showText.input){
  
  ########################################
  # Reading in the list of crude metrics #
  ########################################
  crude.metric.input.AM <<- avgMetrics.input
  
  ####################################
  # Reading in the type of date data #
  ####################################
  dateType.AM <<- dateType.input
  
  ################################################
  # Reading in the list of variables to be log10 #
  ################################################
  logMetrics <<- scaleY.input
  
  #############
  # Low color #
  #############
  lowColor <<- lowColor.input
  
  ##############
  # High color #
  ##############
  highColor <<- highColor.input
  
  #################
  # Outline color #
  #################
  outlineColor <<- outlineColor.input
  
  ##############
  # Text color #
  ##############
  textColor <<- textColor.input
  
  #############
  # Show text #
  #############
  showText <- showText.input
  
  ###############################
  # Empty data frame for graphs #
  ###############################
  fullMetrics <- NA
  
  #################################
  # Creating the list for figures #
  #################################
  figureFinalList <- list()
  
#------------------------------------------------------------------------------#
# Determining which metrics should be in log-scale -----------------------------
#------------------------------------------------------------------------------#
# About: This section determine which of the metrics should be presented in    #
# the log-scale.                                                               #
#------------------------------------------------------------------------------#
  
  # Data for figure 
  fullMetrics <- crude.metric.input.AM
  
  #############################################
  # Adjusting the scale for MSE, WIS, and MAE #
  #############################################
  
  ###############################
  # Adjusting the scale for MSE #
  ###############################
  if("MSE" %in% c(logMetrics)){
    
    # Changing MSE to log10 scale
    fullMetrics$logMSE <- log10(fullMetrics$MSE + 1)
    
    # Variable to use
    fullMetrics$MSEmetricToUse <- fullMetrics$logMSE
    
    # Title 
    titleFinalMSE <- " MSE (log10)"
    
  }else{
    
    # Variable to use
    fullMetrics$MSEmetricToUse <- fullMetrics$MSE
    
    # Title 
    titleFinalMSE <- " MSE"
    
  }
  
  ###############################
  # Adjusting the scale for MAE #
  ###############################
  if("MAE" %in% c(logMetrics)){
    
    # Changing MAE to log10 scale
    fullMetrics$logMAE <- log10(fullMetrics$MAE + 1)
    
    # Variable to use
    fullMetrics$MAEmetricToUse <- fullMetrics$logMAE
    
    # Title 
    titleFinalMAE <- " MAE (log10)"
    
  }else{
    
    # Variable to use
    fullMetrics$MAEmetricToUse <- fullMetrics$MAE
    
    # Title
    titleFinalMAE <- " MAE"
    
  }
  
  ###############################
  # Adjusting the scale for WIS #
  ###############################
  if("WIS" %in% c(logMetrics)){
    
    # Changing WIS to log10 scale
    fullMetrics$logWIS <- log10(fullMetrics$WIS + 1)
    
    # Variable to use
    fullMetrics$WISmetricToUse <- fullMetrics$logWIS
    
    # Title
    titleFinalWIS <- " WIS (log10)"
    
  }else{
    
    # Variable to use
    fullMetrics$WISmetricToUse <- fullMetrics$WIS
    
    # Title
    titleFinalWIS <- " WIS"
    
  }
  
  ###########################################
  # Adjusting the scale for 95% PI Coverage #
  ###########################################
  if("95%PI" %in% c(logMetrics)){
    
    # Changing 95%PI to log10 scale
    fullMetrics$log95 <- log10(fullMetrics$`95%PI` + 1)
    
    # Variable to use
    fullMetrics$PImetricToUse <- fullMetrics$log95
    
    # Title
    titleFinalPI <- " 95% PI Coverage (log10)"
    
  }else{
    
    # Variable to use
    fullMetrics$PImetricToUse <- fullMetrics$`95%PI`
    
    # Title
    titleFinalPI <- " 95% PI Coverage"
    
  }
  
#------------------------------------------------------------------------------#
# Determining if the text should be shown --------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if text should be shown in the tiles or if    #
# the legend should be shown.                                                  #
#------------------------------------------------------------------------------#
  
  ######################################
  # If the show text button is clicked #
  ######################################
  if(showText){
    
    #######
    # MSE #
    #######
    
    # Creating the text
    MSETextLegend <- geom_text(data = fullMetrics,aes(x = Location, y = Model,label=round(MSEmetricToUse, 2)), color = textColor) 
  
    #######
    # MAE #
    #######
    
    # Creating the text
    MAETextLegend <- geom_text(data=fullMetrics,aes(x = Location, y = Model,label=round(MAEmetricToUse, 2)), color = textColor) 
    
    #######
    # WIS #
    #######
    
    # Creating the text
    WISTextLegend <- geom_text(data=fullMetrics,aes(x = Location, y = Model,label=round(WISmetricToUse, 2)), color = textColor) 
    
    ###########################
    # 95% Prediction Interval #
    ###########################
    
    # Creating the text
    PITextLegend <- geom_text(data=fullMetrics,aes(x = Location, y = Model,label=round(PImetricToUse, 2)), color = textColor) 
    
    ###################################
    # Determining the legend position #
    ###################################
    legPos <- "none"
  
  ##########################################
  # If the show text button is not clicked #
  ##########################################
  }else{
    
    #######
    # MSE #
    #######
    
    # Creating the empty text
    MSETextLegend <- NULL 
    
    #######
    # MAE #
    #######
    
    # Creating the empty text
    MAETextLegend <- NULL
    
    #######
    # WIS #
    #######
    
    # Creating the empty text
    WISTextLegend <- NULL
    
    ###########################
    # 95% Prediction Interval #
    ###########################
    
    # Creating the empty text
    PITextLegend <- NULL
    
    ###################################
    # Determining the legend position #
    ###################################
    legPos <- "right"
      
    
    }
  
#------------------------------------------------------------------------------#
# Plotting the data ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the MSE, MAE, 95% PI, and WIS as separate plots    #
# and returns a combined plots.                                                #
#------------------------------------------------------------------------------#
    
    ###############################
    # Plotting Mean Squared Error #
    ###############################
    MSEPlot <- ggplot(data = fullMetrics, aes(x = Location, y = Model, fill = MSEmetricToUse)) +
      geom_tile(color = outlineColor) +
      scale_fill_gradient(low = lowColor, high = highColor, n.breaks = 7) +
      MSETextLegend +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("A") ~ .( titleFinalMSE)),
           y = "",
           x = "",
           fill = "")+
      theme(axis.text.x = element_blank(),
            legend.position = legPos)
    
    ################################
    # Plotting Mean Absolute Error #
    ################################
    MAEPlot <- ggplot(data = fullMetrics, aes(x = Location, y = Model, fill = MAEmetricToUse)) +
      geom_tile(color = outlineColor) +
      scale_fill_gradient(low = lowColor, high = highColor, n.breaks = 7) +
      MAETextLegend +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("B")~ .( titleFinalMAE)),
           y = "",
           x = "",
           fill = "")+
    theme(axis.text.x = element_blank(),
          legend.position = legPos)
    
    #####################################
    # Plotting Weighted Interval Scores #
    #####################################
    WISPlot <- ggplot(data = fullMetrics, aes(x = Location, y = Model, fill = WISmetricToUse)) +
      geom_tile(color = outlineColor) +
      scale_fill_gradient(low = lowColor, high = highColor, n.breaks = 7) +
      WISTextLegend +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("C") ~ .( titleFinalWIS)),
           y = "",
           x = "",
           fill = "")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = legPos)
    
    ###################
    # Plotting 95% PI #
    ###################
    PIPlot <- ggplot(data = fullMetrics, aes(x = Location, y = Model, fill = PImetricToUse)) +
      geom_tile(color = outlineColor) +
      scale_fill_gradient(low = lowColor, high = highColor, n.breaks = 7) +
      PITextLegend + 
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("D") ~  .(titleFinalPI)),
           y = "",
           x = "",
           fill = "") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = legPos)
    
    #####################################################
    # Adding the figure for a single location to a list #
    #####################################################
    figureFinalList[[1]] <- MSEPlot + MAEPlot + WISPlot + PIPlot
  
  #################################
  # Returning the list of figures #
  #################################
  
  return(figureFinalList)

}

    
   
