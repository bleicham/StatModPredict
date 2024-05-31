#------------------------------------------------------------------------------#
#                                                                              #
#                   Filtering the metrics - Model Comparison                   #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function filters the metrics included on the model comparison and       #
# metrics pages when and if the user requests for filtering to occur. It is    #
# applied both for the average and crude metrics.                              #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
filterOtherMetrics <- function(crudeMetrics.input, averageMetrics.input,
                               crudeModel.input, crudePerformance.input,
                               crudeLocation.input, crudeCalibration.input,
                               crudeHorizon.input,  AverageModel.input,
                               AveragePerformance.input, AverageLocation.input,
                               AverageCalibration.input, AverageHorizon.input,
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
crudeMetricsOther <<- crudeMetrics.input

################################
# Renaming the average metrics #
################################
averageMetricsOther <<- averageMetrics.input

################################
# Model filter - Crude Metrics #
################################
CrudemodelFilter <<- crudeModel.input

######################################
# Performance filter - Crude Metrics #
######################################
crudePerformanceFilter <<- crudePerformance.input

###################################
# Location filter - Crude Metrics #
###################################
crudeLocationFilter <<- crudeLocation.input

######################################
# Calibration filter - Crude Metrics #
######################################
crudeCalibrationFilter <<- crudeCalibration.input

##################################
# Horizon filter - Crude Metrics #
##################################
crudeHorizonFilter <<- crudeHorizon.input

################################
# Model filter - Average Metrics #
################################
AverageModelFilter <<- AverageModel.input

######################################
# Performance filter - Average Metrics #
######################################
AveragePerformanceFilter <<- AveragePerformance.input

###################################
# Location filter - Average Metrics #
###################################
AverageLocationFilter <<- AverageLocation.input

######################################
# Calibration filter - Average Metrics #
######################################
AverageCalibrationFilter <<- AverageCalibration.input

##################################
# Horizon filter - Average Metrics #
##################################
AverageHorizonFilter <<- AverageHorizon.input

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
  
  # Calibration
  calibration <- crudeCalibrationFilter
  
  # Horizon
  horizon <- crudeHorizonFilter
  
}else{
  
  # Data
  data <- averageMetricsOther
  
  # Model 
  model <- AverageModelFilter
  
  # Performance 
  performance <- AveragePerformanceFilter
  
  # Location
  location <- AverageLocationFilter
  
  # Calibration
  calibration <- AverageCalibrationFilter
  
  # Horizon
  horizon <- AverageHorizonFilter
  
}

#------------------------------------------------------------------------------#
# Determining if the data should be filtered -----------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if the data should be filtered or not. The    #
# only time it will not be filtered is the initial time it is rendered, or if  #
# truely no filters are specified.                                             #
#------------------------------------------------------------------------------#

######################################
# Checking if filtering should occur #
######################################
if(inputindicator == 0){
  
  # Filtering the data
  finalData <- data
  
}else{
  
  # Filtering the data
  finalData <- data %>%
    dplyr::filter(Model %in% c(model),
                  Location %in% c(location),
                  `Performance Metric Type` %in% c(performance),
                  Calibration %in% c(calibration),
                  Horizon %in% c(horizon))
  
  
}
  
############################
# Returning the final data #
############################
return(finalData)

}