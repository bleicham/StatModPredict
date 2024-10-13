#------------------------------------------------------------------------------#
#                                                                              #
#                   Crude Metrics Data - Model Comparison                      #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# Thus function combines all of the metrics together, thus forming the crude   #
# metrics data set for the `Model Comparison` page. After some data            #
# manipulation, such as for dates and the inclusion of columns the model fit,  #
# forecast, and outside model metrics are all combined.                        #
#------------------------------------------------------------------------------#
#                         Author: Amanda Bleichrodt                            #
#------------------------------------------------------------------------------#
combining.metrics <- function(original.fit.input,orignal.forecast.input, new.input){
  
#------------------------------------------------------------------------------#
# Reading in inputs from the main script ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names for manipulation throughout the remainder of the code.                 #
#------------------------------------------------------------------------------#
  
  #####################
  # Model fit metrics #
  #####################
  orignalMetricsFit <- original.fit.input
  
  ##########################
  # Model forecast metrics #
  ##########################
  orignalMetricsForecast <- orignal.forecast.input
  
  #########################
  # Outside model metrics #
  #########################
  newMetrics <- new.input
  
#------------------------------------------------------------------------------#
# Cleaning up the original model fit metrics -----------------------------------
#------------------------------------------------------------------------------#
# About: This section cleans up the original model fit metrics. It adds the    #
# indicator for the type of metric (fit vs forecast) to prepare for later      #
# merging.                                                                     #
#------------------------------------------------------------------------------#
  
  ########################
  # Cleaning up the data #
  ########################
  cleanedFit <- orignalMetricsFit %>%
    dplyr::mutate(Type = "Fit") %>% # Type of metrics 
    dplyr::select(Type, everything()) # Ordering the variables
  
  
#------------------------------------------------------------------------------#
# Cleaning up the original model forecast metrics ------------------------------
#------------------------------------------------------------------------------#
# About: This section cleans up the original model forecast metrics. It adds   #
# the indicator for the type of metric (fit vs forecast) to prepare for later  #
# merging.                                                                     #
#------------------------------------------------------------------------------#
  
  ########################
  # Cleaning up the data #
  ########################
  cleanedForecast <- orignalMetricsForecast %>%
    dplyr::mutate(Type = "Forecast") %>% # Type of metrics 
    dplyr::select(Type, everything()) # Ordering the variables
  
#------------------------------------------------------------------------------#
# Cleaning up the new metrics --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section merges and cleans up the performance metrics read in by  #
# the user. It adds if its model fit or forecasts, and then combines each file.#
#------------------------------------------------------------------------------#
  
  # Data frame to fill
  allNewMetrics <- NULL
  
  ################################
  # Looping through metric files #
  ################################
  for(i in 1:length(newMetrics)){
    
    #######################
    # Indexed metric file #
    #######################
    indexedFile <- newMetrics[[i]]
    
    # Name of indexed metric file
    indexedFileName <- names(newMetrics)[i]
    
    ##################################
    # Determining the type of metric #
    ##################################
    metricType <- str_split(indexedFileName, "[-]")[[1]][2]
    
    ######################################
    # Determining the calibration period #
    ######################################
    calibrationLength <- as.numeric(str_split(str_split(indexedFileName, "[-]")[[1]][6], "[.]")[[1]][1])
    
    ############################################
    # Adding the information to the data frame #
    ############################################
    indexedFileFinal <- indexedFile %>%
      dplyr::mutate(Type = metricType,
                    Calibration = as.numeric(calibrationLength)) %>%
      dplyr::select(Type, Location, Model, Date, Calibration, everything())
    
    ###############################################
    # Cleaning up the dates: Weekly or Daily Data #
    ###############################################
    if(nchar(indexedFileFinal[1,4]) > 4){
      
      finalNew <- indexedFileFinal %>%
        dplyr::mutate(Date = anytime::anydate(Date))
      
    ###############################################
    # Cleaning up the dates: Yearly or Index Data #
    ###############################################
    }else{
      
      finalNew <- indexedFileFinal %>%
        dplyr::mutate(Date = as.numeric(Date))
      
    }
    
    #####################################
    # Adding the file to the data frame #
    #####################################
    allNewMetrics <- rbind(finalNew,allNewMetrics)
    
  }
  
#------------------------------------------------------------------------------#
# Preparing the final data for export ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section combines all of the data frames together, and then       #
# returns the result to the main dashboard. Additionally, it cleans up the     #
# dates for the original model fit and forecast performance metrics.           #
#------------------------------------------------------------------------------#
  
  ###############################################
  # Cleaning up the dates: Weekly or Daily Data #
  ###############################################
  if(nchar(cleanedFit[1,4]) > 4){
    
    # Fit
    finalFit <- cleanedFit %>%
      dplyr::mutate(Date = as.Date(Date))
    
    # Forecast
    finalForecast <- cleanedForecast %>%
      dplyr::mutate(Date = as.Date(Date))
    
  ###############################################
  # Cleaning up the dates: Yearly or Index Data #
  ###############################################
  }else{
    
    # Fit
    finalFit <- cleanedFit %>%
      dplyr::mutate(Date = as.numeric(Date))
    
    # Forecast
    finalForecast <- cleanedForecast %>%
      dplyr::mutate(Date = as.numeric(Date))
    
  }
  
  #########################
  # Combining the metrics #
  #########################
  
  # First combination 
  combinedCrude1 <- merge(finalFit, allNewMetrics, all = T)
  
  # Final combination 
  finalCombined <- merge(combinedCrude1, finalForecast, all = T)
  
  ##################################
  # Returning the final data frame #
  ##################################
  return(finalCombined)
}