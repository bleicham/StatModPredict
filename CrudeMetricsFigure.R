#------------------------------------------------------------------------------#
#                                                                              #
#                        Timeseries Panels for Metrics                         #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This section plots the crude metrics as time series plots, with the   #
# y-axis being the metric and the x-axis is the dates. Each panel will contain #
# four plots, each which contain a different metric. Each plot will have lines #
# corresponding to each model and each location has its own panel. If their is #
# only one forecast period selected, a single point will be plotted rather     #
# than a line. The function then outputs a list of ggplots that are            #
# transformed to plotlys.                                                      #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
CrudeMetricsFigure <- function(crudeMetrics, dateType, scaleY.input,
                               lowColor.input, highColor.input,
                               outlineColor.input){
  
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

  ###################
  # Data for figure #
  ###################
  fullMetrics <- crude.metric.input.TP
  
  # Fixing names in the read-in data
  names(fullMetrics)[1] <- "Location"
  
  ###############################
  # Adjusting the scale for MSE #
  ###############################
  if("MSE" %in% c(logMetrics)){
    
    # Changing MSE to log10 scale
    fullMetrics$logMSE <- log10(fullMetrics$MSE + 1)
    
    # Variable to use
    fullMetrics$MSEmetricToUse <- fullMetrics$logMSE
    
    # Label for legend 
    labelForLegendMSE <- "Log10"
  
  }else{
    
    # Variable to use
    fullMetrics$MSEmetricToUse <- fullMetrics$MSE
    
    # Label for legend 
    labelForLegendMSE <- NULL
    
  }
  
  ###############################
  # Adjusting the scale for MAE #
  ###############################
  if("MAE" %in% c(logMetrics)){
    
    # Changing MAE to log10 scale
    fullMetrics$logMAE <- log10(fullMetrics$MAE + 1)
    
    # Variable to use
    fullMetrics$MAEmetricToUse <- fullMetrics$logMAE
    
    # Label for legend 
    labelForLegendMAE <- "Log10"
    
  }else{
    
    # Variable to use
    fullMetrics$MAEmetricToUse <- fullMetrics$MAE
    
    # Label for legend 
    labelForLegendMAE <- NULL
    
  }
  
  ###############################
  # Adjusting the scale for WIS #
  ###############################
  if("WIS" %in% c(logMetrics)){
    
    # Changing WIS to log10 scale
    fullMetrics$logWIS <- log10(fullMetrics$WIS + 1)
    
    # Variable to use
    fullMetrics$WISmetricToUse <- fullMetrics$logWIS
    
    # Label for legend 
    labelForLegendWIS <- "Log10"
    
  }else{
    
    # Variable to use
    fullMetrics$WISmetricToUse <- fullMetrics$WIS
    
    # Label for legend 
    labelForLegendWIS <- NULL
    
  }
  
  ###########################################
  # Adjusting the scale for 95% PI Coverage #
  ###########################################
  if("95%PI" %in% c(logMetrics)){
    
    # Changing 95%PI to log10 scale
    fullMetrics$log95 <- log10(fullMetrics$`95%PI` + 1)
    
    # Variable to use
    fullMetrics$PImetricToUse <- fullMetrics$log95
    
    # Label for legend 
    labelForLegend95 <- "Log10"
    
  }else{
    
    # Variable to use
    fullMetrics$PImetricToUse <- fullMetrics$`95%PI`
    
    # Label for legend 
    labelForLegend95 <- NULL
    
  }
  

#------------------------------------------------------------------------------#
# Plotting the data ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares for the loop that will create each panel, and   #
# determines the the y-breaks and x-breaks for the figures. Additionally, it   #
# creates a vector to determine how many unique forecast periods are included  #
# in the data set.                                                             #
#------------------------------------------------------------------------------#
  
  #############################
  # Looping through locations #
  #############################
  for(i in 1:length(unique(fullMetrics$Location))){
    
    # Creating a list of unique locations
    uniqueLocations <- c(unique(fullMetrics$Location))
    
    ###################################
    # Filtering the data for graphing #
    ###################################
    dataForGraph <- fullMetrics %>%
      dplyr::filter(Location == uniqueLocations[i])
    
    ###############################
    # Plotting Mean Squared Error #
    ###############################
    MSEPlot <- ggplot(data = dataForGraph, aes(x = Date, y = Model, fill = MSEmetricToUse)) +
      geom_tile(color = outlineColor) +
      scale_fill_gradient(low = lowColor, high = highColor, n.breaks = 7) +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("A")~"  MSE"),
           y = "",
           x = "",
           fill = labelForLegendMSE) +
      theme(axis.text.x = element_blank())
    
    ################################
    # Plotting Mean Absolute Error #
    ################################
    MAEPlot <- ggplot(data = dataForGraph, aes(x = Date, y = Model, fill = MAEmetricToUse)) +
      geom_tile(color = outlineColor) +
      scale_fill_gradient(low = lowColor, high = highColor, n.breaks = 7) +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("B")~"  MAE"),
           y = "",
           x = "",
           fill = labelForLegendMAE) +
      theme(axis.text.x = element_blank())
  
    #####################################
    # Plotting Weighted Interval Scores #
    #####################################
    WISPlot <- ggplot(data = dataForGraph, aes(x = Date, y = Model, fill = WISmetricToUse)) +
      geom_tile(color = outlineColor) +
      scale_fill_gradient(low = lowColor, high = highColor, n.breaks = 7) +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("C")~"  WIS"),
           y = "",
           x = "",
           fill = labelForLegendWIS)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ###################
    # Plotting 95% PI #
    ###################
    PIPlot <- ggplot(data = dataForGraph, aes(x = Date, y = Model, fill = PImetricToUse)) +
      geom_tile(color = outlineColor) +
      scale_fill_gradient(low = lowColor, high = highColor, n.breaks = 7) +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("D")~"  95% PI Coverage"),
           y = "",
           x = "",
           fill = labelForLegend95) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    #####################################################
    # Adding the figure for a single location to a list #
    #####################################################
    figureFinalList[[i]] <- MSEPlot + MAEPlot + WISPlot + PIPlot
    
    # Adding the location name
    names(figureFinalList)[i] <- uniqueLocations[i]
    
  } # End of location loop
  
  #################################
  # Returning the list of figures #
  #################################
  
  return(figureFinalList)
  
}



