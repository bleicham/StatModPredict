#------------------------------------------------------------------------------#
#                                                                              #
#                        Timeseries Panels for Metrics                         #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This section creates an average panel plot to show the average metric #
# plot for the given data. The y-axis is the locations, and the x-axis is the  #
# models. In total, there are four figures to one panels, where each panel     #
# corresponds to one metric. The function outputs a figure.                    #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
AverageMetricsPanel <- function(avgMetrics.input, dateType.input){
  
  ########################################
  # Reading in the list of crude metrics #
  ########################################
  crude.metric.input.AM <<- avgMetrics.input
  
  ####################################
  # Reading in the type of date data #
  ####################################
  dateType.AM <<- dateType.input
  
  ###############################
  # Empty data frame for graphs #
  ###############################
  fullMetrics <- NA
  
  #################################
  # Creating the list for figures #
  #################################
  figureFinalList <- list()
  
#------------------------------------------------------------------------------#
# Creating the main data frame used in plotting --------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares for plotting of the average metrics. It also    #
# takes the 'log10' of MSE, MAE, and WIS for plotting.                         #
#------------------------------------------------------------------------------#
  
  # Data for figure 
  fullMetrics <- crude.metric.input.AM
  
  #############################################
  # Adjusting the scale for MSE, WIS, and MAE #
  #############################################
  
  # Changing MSE to log10 scale
  fullMetrics$logMSE <- log10(fullMetrics$avgMSE + 1)
  
  # Changing MAE to log10 scale
  fullMetrics$logMAE <- log10(fullMetrics$avgMAE + 1)
  
  # Changing WIS to log10 scale
  fullMetrics$logWIS <- log10(fullMetrics$avgWIS + 1)
  
#------------------------------------------------------------------------------#
# Plotting the data ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the MSE, MAE, 95% PI, and WIS as separate plots    #
# and returns a combined plots.                                                #
#------------------------------------------------------------------------------#
    
    ###############################
    # Plotting Mean Squared Error #
    ###############################
    MSEPlot <- ggplot(data = fullMetrics, aes(x = Location, y = Model, fill = logMSE)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "#59788E") +
      geom_text(data=fullMetrics,aes(x = Location, y = Model,label=round(logMSE, 2))) +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("A")~"  MSE (log10)"),
           y = "",
           x = "") +
      theme(axis.text.x = element_blank(),
            legend.position = "none")
    
    ################################
    # Plotting Mean Absolute Error #
    ################################
    MAEPlot <- ggplot(data = fullMetrics, aes(x = Location, y = Model, fill = logMAE)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "#59788E") +
      geom_text(data=fullMetrics,aes(x = Location, y = Model,label=round(logMAE, 2))) +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("B")~"  MAE (log10)"),
           y = "",
           x = "") +
    theme(axis.text.x = element_blank(),
          legend.position = "none")
    
    #####################################
    # Plotting Weighted Interval Scores #
    #####################################
    WISPlot <- ggplot(data = fullMetrics, aes(x = Location, y = Model, fill = logWIS)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "#59788E") +
      geom_text(data=fullMetrics,aes(x = Location, y = Model,label=round(logWIS, 2))) +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("C")~"  WIS (log10)"),
           y = "",
           x = "") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")
    
    ###################
    # Plotting 95% PI #
    ###################
    PIPlot <- ggplot(data = fullMetrics, aes(x = Location, y = Model, fill = avgPI)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "#59788E") +
      geom_text(data=fullMetrics,aes(x = Location, y = Model,label=round(avgPI, 2))) +
      theme_bw() + 
      coord_fixed() +
      labs(title = bquote(bold("D")~"  95% PI Coverage"),
           y = "",
           x = "",
           fill = "") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "none")
    
    #####################################################
    # Adding the figure for a single location to a list #
    #####################################################
    figureFinalList[[1]] <- MSEPlot + MAEPlot + WISPlot + PIPlot
  
  #################################
  # Returning the list of figures #
  #################################
  
  return(figureFinalList)

}

    
   
