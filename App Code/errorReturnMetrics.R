#------------------------------------------------------------------------------#
#                                                                              #
#                            Checking for Errors                               #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function checks for errors in the data inputted on the model comparison #
# page against the original data. Errors are returned if the columns are not   #
# included correctly, the naming scheme of the file is off, or the locations   #
# do not match the original data.                                              #    
#------------------------------------------------------------------------------#
errorReturnMetrics <- function(orignaldata.input, otherMetrics.input, 
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
  orignalData <- orignaldata.input
  
  ############################
  # Model comparison metrics #
  ############################
  otherMetrics <- otherMetrics.input
  
  ####################
  # Original Horizon #
  ####################
  orignalHorizon <- horizon.input
  
  ##################################
  # Creating the `not-in` function #
  ##################################
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
#------------------------------------------------------------------------------#
# Looping through the model performance metrics --------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each of the model comparison metrics to    #
# check for various errors.                                                    #
#------------------------------------------------------------------------------#
  
  for(i in 1:length(otherMetrics)){
    
    ###############################
    # Indexed performance metrics #
    ###############################
    indexedFile <- otherMetrics[[i]]
    
    # Indexed file name 
    indexedFileName <- names(otherMetrics)[i]
    
#------------------------------------------------------------------------------#
# Checking the file name format ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the naming format of the metrics files. If an     #
# error occurs, the function will break.                                       #
#------------------------------------------------------------------------------#
    
    ##############################################
    # Splitting the name of the file into pieces #
    ##############################################
    fileSplit <- strsplit(indexedFileName, split = "-")
    
    #######################################
    # Checking for the word 'Performance' #
    #######################################
    if(fileSplit[[1]][1] != "Performance"){
      
      return("ERROR1")
      break
      
    }
    
    #############################################
    # Checking for the word `Fit` or `Forecast` #
    #############################################
    if(fileSplit[[1]][2] %!in% c("Fit", "Forecast")){
      
      return("ERROR1")
      break
      
    }
    
    ###################################
    # Checking for the word `horizon` #
    ###################################
    if(fileSplit[[1]][3] != "horizon"){
      
      return("ERROR1")
      break
      
    }
    
    #####################################
    # Checking for the word calibration #
    #####################################
    if(fileSplit[[1]][5] != "calibration"){
      
      return("ERROR1")
      break
      
    }
    
#------------------------------------------------------------------------------#
# Checking locations against data ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section compares the locations included in the metrics file      #
# against the original data. If the locations in the metrics file does not     #
# match any location in the original data, and error is returned.              #
#------------------------------------------------------------------------------#
    
    #######################
    # Checking for `Date` #
    #######################
    if(unique(indexedFile$Location) %!in% colnames(orignalData)) {
      
      return("ERROR4")
      break
      
    }
    
  } # End of metrics loop 
  
#------------------------------------------------------------------------------#
# Checking the horizon against that of the other metrics -----------------------
#------------------------------------------------------------------------------#
# About: This section checks the horizon specified in the file names against   #
# that specified in the main sidebar. If they do not match, and error is       #
# returned.                                                                    #
#------------------------------------------------------------------------------#
    
    if(as.numeric(fileSplit[[1]][4]) != as.numeric(orignalHorizon)){
      
      return("ERROR2")
      break
      
    }
    
#------------------------------------------------------------------------------#  
# Checking the first three header names ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the first three header names to ensure that they  #
# are "Location", "Model, and "Date".                                          #
#------------------------------------------------------------------------------#
    
    ############################
    # Pulling the column names #
    ############################
    colNamesMetrics <- colnames(indexedFile)
    
    ###########################
    # Checking for `Location` #
    ###########################
    if(colNamesMetrics[1] != "Location"){
      
      return("ERROR3")
      break
      
    }
    
    ########################
    # Checking for `Model` #
    ########################
    if(colNamesMetrics[2] != "Model"){
      
      return("ERROR3")
      break
      
    }
    
    #######################
    # Checking for `Date` #
    #######################
    if(colNamesMetrics[3] != "Date"){
      
      return("ERROR3")
      break
      
    }
  

  #############################################
  # Returning if all went well with the files #
  #############################################
  return("WORKED")
  
}
