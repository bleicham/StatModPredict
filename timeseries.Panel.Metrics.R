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
  crude.metric.input.TP <- crudeMetrics
  
  ####################################
  # Reading in the type of date data #
  ####################################
  dateType.TP <- dateType 
  
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
  for(i in 1:length(crude.metric.input.TP)){
    
    ########################
    # Indexed crude metric #
    ########################
    indexedMetric <- crude.metric.input.TP[[i]]
    
    # Pulling the name of the crude metric 
    indexedMetricName <- names(crude.metric.input.TP[i])
    
    ########################################################
    # Determining the model, forecast period, and location #
    ########################################################
    
    # Model type 
    model.Figure <- strsplit(indexedMetricName, "[-]")[[1]][1]
    
    # Sub-setting location/group name 
    locationGroupName <- strsplit(indexedMetricName, "[-]")[[1]][2]
    
    # Forecast period for weekly or daily data 
    if(dateType.TP %in% c("week", "day")){
      
      # Determining the forecast period from the name
      forecastPeriod <- anytime::anydate(substring(indexedMetricName, regexpr("-", indexedMetricName) + (nchar(locationGroupName) + 2)))
      
      # Forecast period for yearly or time index data 
    }else{
      
      # Determining the forecast period from the name
      forecastPeriod <- as.numeric(strsplit(indexedMetricName, "[-]")[[1]][3])
      
    }
    
    ###############################################################
    # Adding the information as columns to the metrics data frame #
    ###############################################################
    individualMetricFinal <- indexedMetric %>%
      dplyr::mutate(model = model.Figure, # Model Name
                    location = locationGroupName, # Location 
                    forecast_Period = forecastPeriod) # Forecast Period
    
    ##########################################################
    # Adding the individual data file to the full data frame #
    ##########################################################
    fullMetrics <- rbind(fullMetrics, individualMetricFinal)
    
  }
  
  ##########################################################
  # Removing the first row of the 'fullMetrics' data frame #
  ##########################################################
  wideData <- fullMetrics[-1,]
  
  # Wide to long data 
  graphData <- pivot_longer(wideData, !c(model, location, forecast_Period), values_to = "Value", names_to = "Metric")
  
#------------------------------------------------------------------------------#
# Plotting the data ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares for the loop that will create each panel, and   #
# determines the the y-breaks and x-breaks for the figures. Additionally, it   #
# creates a vector to determine how many unique forecast periods are included  #
# in the data set.                                                             #
#------------------------------------------------------------------------------#
  
  ################################
  # Creating a list of locations #
  ################################
  uniqueLocations <- c(unique(graphData$location))
  
  #############################
  # Looping through locations #
  #############################
  for(j in 1:length(uniqueLocations)){
    
    # Indexed location
    locationIndex <- uniqueLocations[j]
    
    # Filtering the graph data
    finalGraph <- graphData %>%
      dplyr::filter(location == locationIndex)
    
    
    ###############################################
    # Handling dates in the files - weeks or days #
    ###############################################
    if(dateType.TP %in% c("week", "day")){
      
      # X-axis breaks 
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(finalGraph$forecast_Period)), max(anydate(finalGraph$forecast_Period)), by = 7))  # X-axis breaks
      
      #####################################################
      # Handling dates in the files - years or time index #
      #####################################################
    }else{
      
      # X-axis breaks
      xAxisBreaks <- scale_x_continuous(breaks = seq(min(finalGraph$forecast_Period), max(finalGraph$forecast_Period), by = 1))  # X-axis breaks
      
    }

      #########################################
      # Plotting 95% PI - Setting up the data #
      #########################################
      PI <- finalGraph %>%
        dplyr::filter(Metric == "95%PI")
      
      ###############
      # Plot 95% PI #
      ###############
      PIPlot <- ggplot(PI, aes(x = as.factor(forecast_Period), y = model, fill = Value)) +
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
        dplyr::filter(Metric == "WIS")
      
      ############
      # Plot WIS #
      ############
      WISPlot <- ggplot(WIS, aes(x = as.factor(forecast_Period), y = model, fill = Value)) +
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
        dplyr::filter(Metric == "MSE")
      
      
      ############
      # Plot MSE #
      ############
      MSEPlot <- ggplot(MSE, aes(x = as.factor(forecast_Period), y = model, fill = Value)) +
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
        dplyr::filter(Metric == "MAE")
      
    ############
    # Plot MSE #
    ############
    MAEPlot <- ggplot(MAE, aes(x = as.factor(forecast_Period), y = model, fill = Value)) +
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
  
    
    #################################
    # Adding the figure to the list #
    #################################
    figureList[[j]] <- metricsTimeSeries
    
    # Adding the name to the list
    names(figureList)[j] <- locationIndex
    
  } # End of loop creating plots
  
  return(figureList)
  
}

