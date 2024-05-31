#------------------------------------------------------------------------------#
#                                                                              #
#                  Filtering the Winkler Scores - Model Comparison             #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function filters the Winkler scores location on the model comparison    #
# page dependent on the user's selctions.                                      #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
filterOtherWinkler <- function(Winkler.input, # Original metrics 
                               WinklerModel.input, # Winkler model filter
                               WinklerPerformance.input, # Winkler performance filter
                               WinklerLocation.input, # Winkler location filter 
                               WinklerCalibration.input, # Winkler calibration filter
                               WinklerHorizon.input, # Winkler horizon filter
                               indicatorToShow)
  {
  
#------------------------------------------------------------------------------#
# Renaming the input variables -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to ensure that nothing gets  #
# overwritten.                                                                 #
#------------------------------------------------------------------------------#
  
  ###############################
  # Renaming the Winkler Scores #
  ###############################
  winklerScores <- Winkler.input
  
  ############################
  # Renaming the model input #
  ############################
  modelFilter <- WinklerModel.input
  
  ##############################################
  # Renaming the performance metric type input #
  ##############################################
  performanceFilter <- WinklerPerformance.input
  
  ###############################
  # Renaming the location input #
  ###############################
  locationFilter <- WinklerLocation.input
  
  ##################################
  # Renaming the calibration input #
  ##################################
  calibrationFilter <- WinklerCalibration.input
  
  ##############################
  # Renaming the horizon input #
  ##############################
  horizonFilter <- WinklerHorizon.input
  

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
  if(indicatorToShow == 0 || all(is.null(modelFilter), is.null(locationFilter), is.null(performanceFilter), is.null(calibrationFilter), is.null(horizonFilter)) | is.null(winklerScores)){
    
    # Filtering the data
    finalData <- winklerScores
  
  #####################################
  # Running if filtering should occur #
  #####################################
  }else{
    
    # Filtering the data
    finalData <- winklerScores %>%
      dplyr::filter(Model %in% c(modelFilter), # Filtering model 
                    Location %in% c(locationFilter), # Filtering location
                    `Performance Metric Type` %in% c(performanceFilter), # Filtering performance metric type
                    Calibration %in% c(calibrationFilter), # Filtering calibration 
                    Horizon %in% c(horizonFilter)) # Filtering horizon 
    
    
  }

  
  ############################
  # Returning the final data #
  ############################
  return(finalData)
  
}