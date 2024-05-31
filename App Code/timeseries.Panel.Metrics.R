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
timeseries.Panel.Metrics <- function(crudeMetrics, dateType){
  
  ########################################
  # Reading in the list of crude metrics #
  ########################################
  crude.metric.input.TP <<- crudeMetrics
  
  ####################################
  # Reading in the type of date data #
  ####################################
  dateType.TP <<- dateType 
  
  ###############################
  # Empty data frame for graphs #
  ###############################
  fullMetrics <- NA
  
  #################################
  # Creating the list for figures #
  #################################
  figureList <- list()
  
#------------------------------------------------------------------------------#
# Creating the main data frame used in plotting --------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each of the crude metrics, pulls           #
# information from each file and then combines the data into a data frame for  #
# plotting.                                                                    #
#------------------------------------------------------------------------------#

  # Data for figure 
  fullMetrics <<- crude.metric.input.TP
  
  # Fixing names in the read-in data
  names(fullMetrics)[1] <- "Location"
  
  #############################################
  # Adjusting the scale for MSE, WIS, and MAE #
  #############################################
  
  # Changing MSE to log10 scale
  fullMetrics$logMSE <- log10(fullMetrics$MSE + 1)
  
  # Changing MAE to log10 scale
  fullMetrics$logMAE <- log10(fullMetrics$MAE + 1)
  
  # Changing WIS to log10 scale
  fullMetrics$logWIS <- log10(fullMetrics$WIS + 1)
  
#------------------------------------------------------------------------------#
# Plotting the data ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares for the loop that will create each panel, and   #
# determines the the y-breaks and x-breaks for the figures. Additionally, it   #
# creates a vector to determine how many unique forecast periods are included  #
# in the data set.                                                             #
#------------------------------------------------------------------------------#
  

  MSEPlot <- ggplot(data = fullMetrics, aes(x = Date, y = Location, fill = logMSE)) +
    geom_tile() +
    scale_fill_gradient2() +
    theme_bw() + 
    coord_fixed() +
    labs(title = bquote(bold("A")~"  Mean Squared Error (MSE)"),
         y = "",
         x = "")
  
  MAEPlot <- ggplot(data = fullMetrics, aes(x = Date, y = Location, fill = logMAE)) +
    geom_tile() +
    scale_fill_gradient2() +
    theme_bw() + 
    coord_fixed() +
    labs(title = bquote(bold("B")~"  Mean Absolute Error (MAE)"),
         y = "",
         x = "Transmission")
  
  WISPlot <- ggplot(data = fullMetrics, aes(x = Date, y = Location, fill = logWIS)) +
    geom_tile() +
    scale_fill_gradient2() +
    theme_bw() + 
    coord_fixed() +
    labs(title = bquote(bold("C")~"  Weighted Interval Scores (WIS)"),
         y = "",
         x = "")
  
  PIPlot <- ggplot(data = fullMetrics, aes(x = Date, y = Location, fill = PI)) +
    geom_tile() +
    scale_fill_gradient2() +
    theme_bw() + 
    coord_fixed() +
    labs(title = bquote(bold("D")~"  95% Prediction Interval Coverage (95% PI)"),
         y = "",
         x = "")
  
  
  
  figureFinalList <- list(MSEPlot, MAEPlot, WISPlot, PIPlot)
  
  return(figureFinalList)
  
}

