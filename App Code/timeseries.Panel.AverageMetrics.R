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
timeseries.Panel.AverageMetrics <- function(avgMetrics.input, dateType.input){
  
  ########################################
  # Reading in the list of crude metrics #
  ########################################
  avg.metric.input.TP <- avgMetrics.input
  
  ####################################
  # Reading in the type of date data #
  ####################################
  dateType.TP <- dateType.input
  
  ###############################
  # Empty data frame for graphs #
  ###############################
  fullMetrics <- NA
  
#------------------------------------------------------------------------------#
# Creating the main data frame used in plotting --------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each of the average metrics, pulls         #
# information from each file and then combines the data into a data frame for  #
# plotting.                                                                    #
#------------------------------------------------------------------------------#
  
  for(i in 1:length(avg.metric.input.TP)){
    
    ########################
    # Indexed crude metric #
    ########################
    indexedMetric <- avg.metric.input.TP[[i]]
    
    # Pulling the name of the crude metric 
    indexedMetricName <- names(avg.metric.input.TP[i])
    
    ###############################################################
    # Adding the information as columns to the metrics data frame #
    ###############################################################
    individualMetricFinal <- indexedMetric %>%
      dplyr::mutate(location = indexedMetricName) # Location
    
    ##########################################################
    # Adding the individual data file to the full data frame #
    ##########################################################
    fullMetrics <- rbind(fullMetrics, as.data.frame(individualMetricFinal))
    
  }
  
  ##########################################################
  # Removing the first row of the 'fullMetrics' data frame #
  ##########################################################
  wideData <- fullMetrics[-1,]
  
  # Wide to long data 
  graphData <- pivot_longer(wideData, !c(Model, location), values_to = "Value", names_to = "Metric")
  
#------------------------------------------------------------------------------#
# Plotting the data ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the average forecast panel by creating each of   #
# the metric figures, and then combining them together. It then outputs one    #
# figure.                                                                      #
#------------------------------------------------------------------------------#
  
  ###########################
  # Renaming the graph data #
  ###########################
  finalGraph <- graphData
  
  #########################################
  # Plotting 95% PI - Setting up the data #
  #########################################
  PI <- finalGraph %>%
    dplyr::filter(Metric == "Avg. 95% PI")
  
  ###############
  # Plot 95% PI #
  ###############
  PIPlot <- ggplot(PI, aes(x = Model, y = location, fill = Value)) +
    geom_tile(color = "black") +
    scale_fill_gradient(low = "blue", 
                        high = "red",
                        breaks = seq(min(PI$Value), max(PI$Value), length.out = 5),
                        labels = scales::number_format(accuracy = 0.01)) +
    coord_equal() +
    theme_classic() +
    labs(y = "",
         x = "",
         fill = "95% PI") +
    theme(legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ######################################
  # Plotting WIS - Setting up the data #
  ######################################
  WIS <- finalGraph %>%
    dplyr::filter(Metric == "Avg. WIS")
  
  ############
  # Plot WIS #
  ############
  WISPlot <- ggplot(WIS, aes(x = Model, y = location, fill = Value)) +
    geom_tile(color = "black") +
    scale_fill_gradient(low = "blue",
                        high = "red", 
                        breaks = seq(min(WIS$Value), max(WIS$Value), length.out = 5),
                        labels = scales::trans_format("log10", scales::math_format(.x, scales::number_format(accuracy = 0.01)))) +
    coord_equal() +
    theme_classic() +
    labs(y = "",
         x = "",
         fill = "WIS (log10)") +
    theme(legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ######################################
  # Plotting MSE - Setting up the data #
  ######################################
  MSE <- finalGraph %>%
    dplyr::filter(Metric == "Avg. MSE")
  
  
  ############
  # Plot MSE #
  ############
  MSEPlot <- ggplot(MSE, aes(x = Model, y = location, fill = Value)) +
    geom_tile(color = "black") +
    scale_fill_gradient(low = "blue", 
                        high = "red",
                        breaks = seq(min(MSE$Value), max(MSE$Value), length.out = 5),
                        labels = scales::trans_format("log10", scales::math_format(.x, scales::number_format(accuracy = 0.01)))) +
    coord_equal() +
    theme_classic() +
    labs(y = "",
         x = "",
         fill = "MSE (log(10))") +
    theme(legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ######################################
  # Plotting MAE - Setting up the data #
  ######################################
  MAE <- finalGraph %>%
    dplyr::filter(Metric == "Avg. MAE")
  
  ############
  # Plot MSE #
  ############
  MAEPlot <- ggplot(MAE, aes(x = Model, y = location, fill = Value)) +
    geom_tile(color = "black") +
    scale_fill_gradient(low = "blue",
                        high = "red", 
                        breaks = seq(min(MAE$Value), max(MAE$Value), length.out = 5),
                        labels = scales::trans_format("log10", scales::math_format(.x, scales::number_format(accuracy = 0.01)))) +
    coord_equal() +
    theme_classic() +
    labs(y = "",
         x = "",
         fill = "MAE (log(10))") +
    theme(legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ###################
  # Combining Plots #
  ###################
  metricsTimeSeries <- ggarrange(MSEPlot, MAEPlot, WISPlot, PIPlot, nrow = 2, ncol = 2) +
    theme(panel.background = element_rect(fill = "white", color = "white"))

return(metricsTimeSeries)

}

    
   
