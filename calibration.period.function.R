#------------------------------------------------------------------------------#
#                                                                              #
#                      Preparing the calibration periods                       #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function takes in information related to the user selcted calibration   #
# period, the orignal data, and the vector of forecast dates from the previous #
# function. It then creates a list of calibration periods (i.e., one entry for #
# for each forecast period). The list is then used in conducting forecasts,    #
# plotting, and evaluating performance metrics.                                #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
calibration.period.function <- function(crude.data.input, calibration.period.input,
                                        forecast.period.input, date.number.input){
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

###############################
# Inputted calibration period #
###############################
calibration.period <- calibration.period.input

#######################################
# User selected forecast period range #
#######################################
forecast.period.range <- forecast.period.input

##################
# Date sequencer #
##################
dateSeqNum <- date.number.input

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
  
  #####################################################
  # Dealing with a one time period calibration period #
  #####################################################
  if(calibration.period == 1){
    
    # If the calibration period is equal to one, the data is length one
    calibration.period.data <- data.input.C %>%
      dplyr::filter(date == indexed.forecast.period) # Filtering the data
    
    }else{
      
      #########################################################
      # Determining the start date for the calibration period #
      #########################################################
      first.date.calibration <- indexed.forecast.period - ((dateSeqNum*calibration.period) - dateSeqNum)
      
      ######################################################################
      # Subsetting crude data to form the corresponding calibration period #
      ######################################################################
      calibration.period.data <- data.input.C %>%
        dplyr::filter(date >= first.date.calibration & date <= indexed.forecast.period)
      }
  
  ########################################################
  # Adding the calibration period to the list for export #
  ########################################################
  calibration.periods.list[[i]] <- calibration.period.data
  
}

#########################################
# Returning the list to the main script #
#########################################

return(calibration.periods.list)

}
