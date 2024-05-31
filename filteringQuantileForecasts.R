#------------------------------------------------------------------------------#
#                                                                              #
#                    Filtering the Quantile Forecast Data                      #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function filters the quantile forecast data used in the first panel of  #
# main dashboard page.                                                         #
#------------------------------------------------------------------------------#
# By: Amanda Bleichrodt                                                        #
#------------------------------------------------------------------------------#
filteringQuantileForecasts <- function(QuantileForecast.input, 
                                       modelFilterQ.input,
                                       locationFilterQ.input,
                                       indicator.input){
  
#------------------------------------------------------------------------------#
# Renaming the input variables -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to ensure that nothing gets  #
# overwritten.                                                                 #
#------------------------------------------------------------------------------#
  
  ###########################
  # Quantile forecast input #
  ###########################
  quantileForecast <- QuantileForecast.input
  
  ################
  # Model filter #
  ################
  modelFilter <- modelFilterQ.input
  
  ###################
  # Location Filter #
  ###################
  locationFilter <- locationFilterQ.input
  
  ################################
  # Forecast to show - Indicator #
  ################################
  dataToShow <- indicator.input
  
  ################################
  # Empty list to file with data #
  ################################
  finalData <- list()
  
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
  if(dataToShow == 0 || all(is.null(modelFilter), is.null(locationFilter))){
    
    # Filtering the data
    finalData <-  quantileForecast
    
#------------------------------------------------------------------------------#
# Filtering the data -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the data based upon the user inputs selected by  #
# the user. It loops through each forecast file and determines if it should    #
# be returned or not.                                                          #
#------------------------------------------------------------------------------#
    
  ###################### 
  # Filtering the data #
  ######################
  }else{
    
    ##############################################
    # Looping through each of the forecast files #
    ##############################################
    for(i in 1:length(quantileForecast)){
      
      # Forecast file
      forecastFile <- quantileForecast[[i]]
      
      # Forecast file name
      forecastFileName <- names(quantileForecast)[i]
      
      ###########################################################
      # Pulling the location and model names from the file name #
      ###########################################################
      
      # Model 
      model <- qdapRegex::ex_between(forecastFileName, "", "-")[[1]][1]
      
      # Location
      location <- qdapRegex::ex_between(forecastFileName, paste0(model,"-"), "-")[[1]][1]
      
      #############################################################
      # Determining if the data should be added to the final list #
      #############################################################
      
      # Adding to the list
      if(all(model %in% c(modelFilter) & location %in% c(locationFilter))){
        
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