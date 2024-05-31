#------------------------------------------------------------------------------#
#                                                                              #
#                   Creating the list of forecast periods                      #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function takes in the type of date data (i.e., week, day, year, or      #
# time index), the first forecast period, the last forecast period, what to    #
# sequence dates by, and the column of date-data from the crude data. It then  #
# runs the inputs through some checks to ensure what is entered will work for  #
# later forecasting steps. Once the checks have been passed, and no warnings   #
# occur, the function outputs a vector of forecasting periods to be used in a  #
# loop in the `Running_Forecast.R` file.                                       #
#------------------------------------------------------------------------------#
#                      Author: Amanda Bleichrodt                               #
#------------------------------------------------------------------------------#

forecast.period.dates.function <- function(date.Type.input, start.input, 
                                           end.input, date.seq.input, dates.input){
  
#------------------------------------------------------------------------------#
# Creating the 'not-in' function -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the 'not-in' function. Therefore, `%!in%` now    #
# can be used as the inverse of the built-in `%in%` function.                  #
#------------------------------------------------------------------------------#
  
  `%!in%` <- function(x, y) {
    
    !(x %in% y)
    
  }
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Below takes in the inputs from the function, and saves them under new #        
# variable names to prevent over-writing of values.                            #
#------------------------------------------------------------------------------#
  
  #################
  # Type of dates #
  #################
  date.type.input.FP <- date.Type.input
  
  #################################
  # First desired forecast period #
  #################################
  start.input.FP <- start.input
  
  ################################
  # Last desired forecast period #
  ################################
  end.input.FP <- end.input
  
  #############################
  # What to sequence dates by #
  #############################
  date.seq.FP <- date.seq.input
  
  ###############
  # Crude dates #
  ###############
  dates.input.FP <- dates.input

#------------------------------------------------------------------------------#
# Data Error Checks ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section includes data validation checks to ensure that the       #
# forecast periods specified are the correct formatting, and work with the data#
# specified by the user.                                                       #
#------------------------------------------------------------------------------#
  
  #####################################################
  # Checking if the format of input/output is correct #
  #####################################################
  
  # Length of start input 
  startInputLength <- nchar(start.input.FP)
  
  # Length of the end input
  endInputLength <- nchar(end.input.FP)

#------------------------------------------------------------------------------#
# Creating the vector of forecast periods --------------------------------------
#------------------------------------------------------------------------------#
# About: This section is executed if the user-entered forecast periods pass the#
# the validation checks. It then creates a vector of the forecast periods which#
# are used in a loop in `Running_Forecast.R`.                                  #
#------------------------------------------------------------------------------#

  ############################################
  # If working with years or time-index data #
  ############################################
  if(date.type.input.FP %in% c("year", "index")){
    
    # Vector of forecasting period dates 
    forecast.period.dates <- seq(as.numeric(start.input.FP), as.numeric(end.input.FP), by = 1)

    
    ########################################
    # If working with weekly or daily data #
    ########################################
    }else{
      
      # Vector of forecasting period dates
      forecast.period.dates <- seq.Date(anytime::anydate(start.input.FP), anytime::anydate(end.input.FP), by = date.seq.F)
      
      }

##########################################
# Returning the list of forecast periods #
##########################################

return(forecast.period.dates)
  
}