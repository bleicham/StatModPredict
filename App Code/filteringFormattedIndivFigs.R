#------------------------------------------------------------------------------#
#                                                                              #
#                   Filtering the Formatted Forecast Figures                   #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function filters the formatted forecast figure list based upon the      #
# users' selection. If no filtering is indicator, or the button is not         #
# clicked, all forecast files will show. The results of this page are shown    #
# on the main dashboard page.                                                  #
#------------------------------------------------------------------------------#
#                       Author: Amanda Bleichrodt                              #                                                    
#------------------------------------------------------------------------------#
filteringFormattedIndivFigs <- function(formattedForecast.input, 
                                        modelFilterFF.input,
                                        locationFilterFF.input,
                                        indicator.input,
                                        calibrationFilterFF.input){
  
#------------------------------------------------------------------------------#
# Renaming the input variables -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to ensure that nothing gets  #
# overwritten.                                                                 #
#------------------------------------------------------------------------------#
  
  ############################
  # Formatted forecast input #
  ############################
  formattedForecast <- formattedForecast.input
  
  ################
  # Model filter #
  ################
  modelFilter <- modelFilterFF.input
  
  ###################
  # Location Filter #
  ###################
  locationFilter <- locationFilterFF.input
  
  ######################
  # Calibration filter #
  ######################
  calibrationFilter <- calibrationFilterFF.input
  
  ################################
  # Forecast to show - Indicator #
  ################################
  dataToShow <- indicator.input
  
  ################################
  # Empty list to file with data #
  ################################
  finalData <- list()
  
#------------------------------------------------------------------------------#
# Determining if the figures should be filtered --------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if the figures should be filtered or not. The #
# only time it will not be filtered is the initial time it is rendered, or if  #
# truly no filters are specified.                                              #
#------------------------------------------------------------------------------#
  
  ######################################
  # Checking if filtering should occur #
  ######################################
  if(dataToShow == 0 || all(is.null(modelFilter), is.null(locationFilter), is.null(calibrationFilter))){
    
    # Filtering the data
    finalData <- formattedForecast
    
#------------------------------------------------------------------------------#
# Filtering the figures --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the figures based upon the user inputs selected  #
# by the user. It loops through each forecast file and determines if it should #
# be returned or not.                                                          #
#------------------------------------------------------------------------------#
    
  ###################### 
  # Filtering the data #
  ######################
  }else{
    
    ##############################################
    # Looping through each of the forecast files #
    ##############################################
    for(i in 1:length(formattedForecast)){
      
      # Forecast file
      forecastFile <- formattedForecast[[i]]
      
      # Forecast file name
      forecastFileName <- names(formattedForecast)[i]
      
      #######################################################################################
      # Pulling the location, model names, and calibration period length from the file name #
      #######################################################################################
      
      # Model 
      model <- qdapRegex::ex_between(forecastFileName, "", "-")[[1]][1]
      
      # Location
      location <- qdapRegex::ex_between(forecastFileName, paste0(model,"-"), "-")[[1]][1]
      
      # Calibration period length 
      caliLength <- qdapRegex::ex_between(forecastFileName, "Calibration-", "(")[[1]][1]
      
      #############################################################
      # Determining if the data should be added to the final list #
      #############################################################
      
      # Adding to the list
      if(all(model %in% c(modelFilter) & location %in% c(locationFilter) & caliLength %in% c(calibrationFilter))){
        
        # Adding the data
        finalData[[i]] <- forecastFile
        
        # Adding the name
        names(finalData)[i] <- forecastFileName
        
      # Not adding to the list 
      }else{
        
        # Adding an NA
        finalData[[i]] <- NULL
        
      }
      
    }# End of Loop 
    
  }# End of 'else'
  
#------------------------------------------------------------------------------#
# Returning the final list -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the final list to the main dashboard.            #
#------------------------------------------------------------------------------#
  
  # Removing null values
  finalData <- finalData[!sapply(finalData, is.null)]
  
  ######################
  # Returning the list #
  ######################
  return(finalData)
  
}