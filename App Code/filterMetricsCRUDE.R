#------------------------------------------------------------------------------#
#                                                                              #
#                   Filtering the metrics in the dashboard                     #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function filters the metrics included on the model comparison metrics   #
# pages when and if the user requests for filtering to occur. It is            #
# applied both for the average and crude metrics.                              #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
filterMetricsCRUDE <- function(crudeMetrics.input, averageMetrics.input,
                          crudeModel.input, crudePerformance.input,
                          crudeLocation.input, AverageModel.input,
                          AveragePerformance.input, AverageLocation.input,
                          inputindicator){
  
#------------------------------------------------------------------------------#
# Renaming the input variables -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to ensure that nothing gets  #  
# overwritten.                                                                 #
#------------------------------------------------------------------------------#
  
  ##############################
  # Renaming the crude metrics #
  ##############################
  crudeMetricsOther <- crudeMetrics.input
  
  ################################
  # Renaming the average metrics #
  ################################
  averageMetricsOther <- averageMetrics.input
  
  ################################
  # Model filter - Crude Metrics #
  ################################
  CrudemodelFilter <- crudeModel.input
  
  ######################################
  # Performance filter - Crude Metrics #
  ######################################
  crudePerformanceFilter <- crudePerformance.input
  
  ###################################
  # Location filter - Crude Metrics #
  ###################################
  crudeLocationFilter <- crudeLocation.input
  
  ################################
  # Model filter - Average Metrics #
  ################################
  AverageModelFilter <- AverageModel.input
  
  ########################################
  # Performance filter - Average Metrics #
  ########################################
  AveragePerformanceFilter <- AveragePerformance.input
  
  #####################################
  # Location filter - Average Metrics #
  #####################################
  AverageLocationFilter <- AverageLocation.input
  
#------------------------------------------------------------------------------#
# Determining which settings to go with ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if we are working with the average or crude   #
# metrics list, and calls the appropriate filters.                             #
#------------------------------------------------------------------------------#
  
  ##################################
  # Working with the crude metrics #
  ##################################
  if(is.null(averageMetricsOther)){
    
    # Data
    data <- crudeMetricsOther
    
    # Model 
    model <- CrudemodelFilter
    
    # Performance 
    performance <- crudePerformanceFilter
    
    # Location
    location <- crudeLocationFilter
    
  ####################################  
  # Working with the average metrics #
  ####################################
  }else{
    
    # Data
    data <- averageMetricsOther
    
    # Model 
    model <- AverageModelFilter
    
    # Performance 
    performance <- AveragePerformanceFilter
    
    # Location
    location <- AverageLocationFilter
    
  }
  
  ############################
  # Renaming the data column #
  ############################
  # names(data) <- c("Location", "Model", "Date", "MSE", "MAE", "95%PI", "WIS")
  
#------------------------------------------------------------------------------#
# Determining if the data should be filtered -----------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if the data should be filtered or not. The    #
# only time it will not be filtered is the initial time it is rendered, or if  #
# truely no filters are specified. Additionally, it fixes the names of the     #
# table.                                                                       #
#------------------------------------------------------------------------------#
  
  ######################################
  # Checking if filtering should occur #
  ######################################
  if(inputindicator == 0){
    
    # Filtering the data
    finalData <- data
  
  ##################################
  # Runs if filtering should occur #
  ##################################
  }else{
  
    
    # Filtering the data
    finalData <- data %>%
      dplyr::select(Model, Location, all_of(performance)) %>%
      dplyr::filter(Model %in% c(model),
                    Location %in% c(location)) 
      
  
  }
  
  ############################
  # Returning the final data #
  ############################
  return(finalData)
  
}