#------------------------------------------------------------------------------#
#                                                                              #
#               Calculating the Average Metrics - Model Comparison             #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function calculates the average performance metric across forecast      #
# period dates for all metrics, with the except of some for the ARIMA model.   #
# Each unique combination of location, calibration period length, type, and    #
# model has their own value.                                                   #
#------------------------------------------------------------------------------#
#                        Author: Amanda Bleichrodt                             #
#------------------------------------------------------------------------------#
average.compare.metrics <- function(metrics.input){
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to be used throughout the    #
# the rest of the code.                                                        #
#------------------------------------------------------------------------------#
  
  ######################
  # Crude metrics list #
  ######################
  crudeMetrics <- metrics.input
  
#------------------------------------------------------------------------------#
# Cleaning up the data ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section cleans up the data prior to calculating the average      #
# metrics. It removes any variables that are not needed.                       #
#------------------------------------------------------------------------------#
  
  ###############################
  # List of variables to remove #
  ###############################
  removeVars <- c("Non-Seasonal-Specification", "Seasonal-Specification", "Intercept", "Q", "df", "p-Value")
  
  ##########################
  # Removing the variables #
  ##########################
  cleanedData <- crudeMetrics %>%
    dplyr::select(-any_of(removeVars))
  
#------------------------------------------------------------------------------#
# Setting up for calculating the average value ---------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares all of the information needed in order to       #
# calculate the average metrics for each available crude metric.               #
#------------------------------------------------------------------------------#
  
  ################################
  # Creating an empty data frame #
  ################################
  averageMetrics <- data.frame(Type = cleanedData$Type,
                               Location = cleanedData$Location,
                               Model = cleanedData$Model,
                               Calibration = cleanedData$Calibration)
                               
  
#------------------------------------------------------------------------------#
# Calculating the average metrics ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the average metric for each crude metric      #
# available. The average is calculated of forecast period dates, holding the   #
# type of metric, location, model, and calibration period length steady.       #
#------------------------------------------------------------------------------#
  for(i in 6:ncol(cleanedData)){
    
    #################
    # Temp data set #
    #################
    temp.data <- data.frame(Type = cleanedData$Type,
                            Location = cleanedData$Location,
                            Model = cleanedData$Model,
                            Calibration = cleanedData$Calibration,
                            Metric = cleanedData[[i]])
    
    ###############################
    # Calculating the average MSE #
    ###############################
    average.temp <- temp.data %>%
      dplyr::group_by(Type, Location, Model, Calibration) %>% # Group-by variables
      dplyr::mutate(avgMetric = round(mean(Metric, na.rm = T), 2)) %>%
      dplyr::ungroup() %>% # Ungroup
      dplyr::distinct(Type, Location, Model, Calibration, .keep_all = T) %>% # Keeping unique rows
      dplyr::select(-Metric) # Removing the crude metric 
    
    #################################
    # Adding to the full data frame #
    #################################
    averageMetrics <- merge(averageMetrics, average.temp,  by = c("Type", "Location", "Model", "Calibration"), all = T) 
    
    # Adding the name
    colnames(averageMetrics)[i-1] <- paste0("Avg. ", names(cleanedData)[i])
    
  }
  
#------------------------------------------------------------------------------#
# Cleaning up the final average data set ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section cleans up the average data set. It removes duplicate     #
# rows, and the last column.                                                   #
#------------------------------------------------------------------------------#
  
  ################
  # Cleaned data #
  ################
  cleanedAvg <- averageMetrics %>%
    dplyr::distinct(Type, Location, Model, Calibration, .keep_all = T)
  
  ######################
  # Returning the data #
  ######################
  return(cleanedAvg)
                           
}