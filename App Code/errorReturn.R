#------------------------------------------------------------------------------#
#                                                                              #
#                            Checking for Errors                               #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function checks for errors in the data inputted on the model comparison #
# page against the original data. Errors are returned if the columns are not   #
# included correctly, the naming scheme of the file is off, the date type does #
# not match the original data, or the locations do not match the original data.#
#------------------------------------------------------------------------------#
errorReturn <- function(orignalData.input, otherForecast.input, dateType.input,
                        horizon.input){
  
#------------------------------------------------------------------------------#
# Reading in inputs from the main script ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names for manipulation throughout the remainder of the code.                 #
#------------------------------------------------------------------------------#
 
  #################
  # Original data #
  #################
  orignalData <- orignalData.input
  
  ##############################
  # Model comparison forecasts #
  ##############################
  otherForecast <- otherForecast.input
  
  ###################################
  # Date type for the original data #
  ###################################
  dateType <- dateType.input
  
  ####################
  # Original Horizon #
  ####################
  orignalHorizon <- horizon.input
  
  ##################################
  # Creating the `not-in` function #
  ##################################
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
 
#------------------------------------------------------------------------------#
# Looping through model comparison forecasts -----------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each of the model comparison forecasts to  #
# check for various errors.                                                    #
#------------------------------------------------------------------------------#
  
  for(i in 1:length(otherForecast)){
    
    #########################
    # Indexed forecast file #
    #########################
    indexedForecast <- otherForecast[[i]]
    
    # Indexed file name
    indexedForecastName <- names(otherForecast)[i]
    
#------------------------------------------------------------------------------#
# Checking the file name -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the file name to make sure it follows the correct #
# format. The correct format is:                                               #
#                                                                              #
#  Model Framework-Model-horizon-#-calibration-#-location or group-DD-MM-YYYY  #
#                                                                              #
#     Model Framework-Model-horizon-#-calibration-#-location or group-YYYY     #
#                                                                              #
# If it does not follow this format, the code will stop and return an error    #
# telling the user to check their file names and re-load the data.             #
#------------------------------------------------------------------------------#
    
    ##################################
    # Splitting the file name by `-` #
    ##################################
    fileSplit <- strsplit(indexedForecastName, split = "-")
    
    ###############################################
    # Checking for certain words in the file name #
    ###############################################
    
    # Checking for the word horizon
    if(fileSplit[[1]][3] != "horizon"){
      
      return("ERROR1")
      break 
      
    }
    
    # Checking for the word calibration
    if(fileSplit[[1]][5] != "calibration"){
      
      return("ERROR1")
      break 
      
    }

#------------------------------------------------------------------------------#
# Checking the forecast horizon ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the forecasting horizon specified in the read-in  #
# files against that specified for the main dashboard.                         #
#------------------------------------------------------------------------------#
    
    ###################################################
    # Pulling the forecast horizon from the file name #
    ###################################################
    nameHorizon <- fileSplit[[1]][4]
    
    #######################################################
    # Comparing the file horizon to the dashboard horizon #
    #######################################################
    if(as.numeric(nameHorizon) != as.numeric( orignalHorizon)){
      
      return("ERROR5")
      break
      
    }
    
#------------------------------------------------------------------------------#
# Checking the locations against the original data -----------------------------
#------------------------------------------------------------------------------#
# About: This section checks the location listed in the file name of the index #
# forecast against those available in the original data. If they do not match, #
# an error is returned.                                                        #
#------------------------------------------------------------------------------#
    
    ############################################
    # Pulling the names from the original data #
    ############################################
    originalLocations <- c(unique(colnames(orignalData)))
    
    ############################################################
    # Comparing the location name for the forecast to the data #
    ############################################################
    if(fileSplit[[1]][7] %!in% c(originalLocations)){
      
      return("ERROR2")
      break 
      
    }
    
#------------------------------------------------------------------------------#
# Checking column names --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the names of the columns in the forecast files.   #
# If they do not match the correct order or spelling, and error returns.       #
#------------------------------------------------------------------------------#
    
    ############################
    # Pulling the column names #
    ############################
    if(any(colnames(indexedForecast) != c("Date", "data", "median", "LB", "UB"))){
      
      return("ERROR4")
      break
      
    }
    
#------------------------------------------------------------------------------#
# Checking the date type against the original data -----------------------------
#------------------------------------------------------------------------------#
# About: This section determines the date type of the indexed forecast, and    #
# compares it to that of the original data. If they do not match, and error    #
# is returned.                                                                 #
#------------------------------------------------------------------------------#
    
    ###############################################
    # Formatting the date column: Daily or Weekly #
    ###############################################
    if(nchar(indexedForecast[1,1]) > 4){
      
      # Changing to date format
      indexedForecastDate <- indexedForecast %>%
        dplyr::mutate(Date = anytime::anydate(Date))
      
      ##########################
      # Checking the date type #
      ##########################
      
      # Difference between the first and second dates 
      dateDifference <- as.numeric(indexedForecastDate[2,1] - indexedForecastDate[1,1])
      
    ####################################################
    # Formatting the date column: Yearly or Time Index #
    ####################################################
    }else{
      
      # Changing to date format
      indexedForecastDate <- indexedForecast %>%
        dplyr::mutate(Date = as.numeric(Date))
      
      # Difference between the first and second dates 
      dateDifference <- 1
      
    }
    
    ###################################
    # Determining the date type: Week #
    ###################################
    if(dateDifference == 7){
      
      dateForecast <- "week"
      
    ##################################
    # Determining the date type: Day #
    ##################################
    }else if(dateDifference == 1 & nchar(indexedForecast[1,1]) > 4){
      
      dateForecast <- "day"
      
    ##################################
    # Determining the date type: Year #
    ##################################
    }else if(dateDifference == 1 & nchar(indexedForecast[1,1]) == 4){
      
      dateForecast <- "year"
      
    ####################################
    # Determining the date type: Index #
    ####################################
    }else{
      
      dateForecast <- "index"
      
    }
    
    ##################################
    # Comparing to the original data #
    ##################################
    if(dateForecast != dateType){
      
      return("ERROR3")
      break
      
    }
    
    
  } # End of loop going through forecasts
  
  return("WORKED")
  
}
