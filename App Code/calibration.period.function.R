#------------------------------------------------------------------------------#
#                                                                              #
#                      Preparing the calibration periods                       #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function takes in inputs related to the user selected calibration       #
# periods, the input data, and the vector of forecast dates from the previous  #
# from the previous function. It then creates a list of calibration periods    #
# (i.e., one for each unique forecast date and calibration period length).     # 
# The list is then used in conducting forecasts, plotting, and evaluating      #
# performance metrics.                                                         #
#------------------------------------------------------------------------------#
#                       Author: Amanda Bleichrodt                              #
#------------------------------------------------------------------------------#
calibration.period.function <- function(crude.data.input, calibration.period.input,
                                        forecast.period.input, date.input){
  
#------------------------------------------------------------------------------#
# Reading in inputs from the main script ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section saves function inputs under new names to be used within  #
# the following code.                                                          #
#------------------------------------------------------------------------------#

  #######################
  # Inputted crude data #
  #######################
  data.input.C <- crude.data.input
  
  ################################
  # Inputted calibration periods #
  ################################
  calibration.period <- calibration.period.input
  
  #######################################
  # User selected forecast period range #
  #######################################
  forecast.period.range <- forecast.period.input
  
  ##################
  # Date sequencer #
  ##################
  dateSeq <- date.input

#------------------------------------------------------------------------------#
# Preparing for calibration period loop ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates an empty list of calibration periods to output   #
# and changes the first column of the data to the name date for                #
# standardization across data sets.                                            #
#------------------------------------------------------------------------------#

  #############################################################
  # Empty list to fill in with calibration period data frames #
  #############################################################
  calibration.periods.list <- list()
  
  ############################
  # Renaming columns of data #
  ############################
  names(data.input.C)[1] <- "date"
  
  ############################################################
  # Determining the number that is the sequence for the date #
  ############################################################
  dateType <- switch(as.character(dateSeq), # Calling the data type
                    "week" = 7, # Sequence forecast periods by seven if weekly data
                    1) # Sequence forecast periods by one if daily, yearly, or time index
  
  ########################################
  # Fixing the date format - Week or Day #
  ########################################
  if(dateSeq %in% c("week", "day")){
    
    data.input.C <- data.input.C %>%
      dplyr::mutate(date = anytime::anydate(date))
   
  ##########################################
  # Fixing the date format - Year of Index #
  ##########################################
  }else{
    
    data.input.C <- data.input.C %>%
      dplyr::mutate(date = as.numeric(date))
    
  }
  
  ###########################################
  # Index for saving the calibration period #
  ###########################################
  savingIndex <- length(forecast.period.range) * length(calibration.period) + 1

#------------------------------------------------------------------------------#
# Forming the calibration periods ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each forecast period date, and subsets the #
# crude data to be the user indicated calibration period. The data frame is    #
# then saved within a list, that is outputted once the loop has concluded.     #
#------------------------------------------------------------------------------#

  ###############################################################
  # Looping through forecast periods to create calibration data #
  ###############################################################
  for(i in 1:length(forecast.period.range)){
  
    # Indexed forecast period 
    indexed.forecast.period <- forecast.period.range[i]

    ##############################################
    # Looping through calibration period lengths #
    ##############################################
    for(g in 1:length(calibration.period)){
      
        # Removing one from the saving index
        savingIndex <- savingIndex - 1
  
        # Indexed calibration period length
        indexed.calibration.length <- as.numeric(calibration.period[g])
        
        # Keeping all data older than or equal to the indexed forecast period 
        potentialData <- data.input.C %>%
          dplyr::filter(date <= indexed.forecast.period)
        
        # Keeping the calibration period data
        calibration.period.data <- potentialData[c((nrow(potentialData)-(as.numeric(indexed.calibration.length)-1)):nrow(potentialData)),]
  
      ########################################################
      # Adding the calibration period to the list for export #
      ########################################################
      calibration.periods.list[[savingIndex]] <- calibration.period.data
      
      } # End of calibration loop
    
  } # End of the forecast horizon loop 
    
  #########################################
  # Returning the list to the main script #
  #########################################
  return(calibration.periods.list)

}
