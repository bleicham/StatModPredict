#------------------------------------------------------------------------------#
#                                                                              #
#                         Formatted Forecast Function                          #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function takes in the list of quantiles, the crude data, calibration    #
# period size and data type to create a list of formatted forecasts. The       #
# lower and upper bounds shown correspond to the 95% PIs for the given model's #
# forecast. It returns a list of formatted forecasts.                          #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
formatted.forecast.function <- function(quantile.input, data.input, 
                                        calibration.input, dateType.input,
                                        model.input, quantile.selected.input,
                                        horizon.input, smoothing.input){

#------------------------------------------------------------------------------#
# Reading in the inputs from the function --------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves their values      #
# under new names to be used throughout the rest of the code.                  #
#------------------------------------------------------------------------------#

  ###########################
  # List of quantile inputs #
  ###########################
  quantile.FF <- quantile.input
  
  ####################
  # Crude data input #
  ####################
  crude.data.FF <- data.input
  
  #############################
  # Calibration period length #
  #############################
  calibration.FF <- calibration.input
  
  #####################
  # Type of date data #
  #####################
  dateType.FF <- dateType.input

  ##############
  # Model type #
  ##############
  model.FF <- model.input
  
  #####################
  # Selected quantile #
  #####################
  quantile.selected.FF <- quantile.selected.input
  
  #######################
  # Forecasting horizon #
  #######################
  horizon.input.FF <- horizon.input
  
  ###################
  # Smoothing input #
  ###################
  smoothing.input.FF <- smoothing.input
  
#------------------------------------------------------------------------------#
# Preparing for the formatted forecast list ------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each quantile forecast to produce the      #
# formatted forecast files. It then saves the formatted forecasts to a list to #
# be shown in the dashboard.                                                   #
#------------------------------------------------------------------------------#
  
  ###########################
  # Creating the empty list #
  ###########################
  formattedForecastList <- list()
  
  ##########################################
  # Looping through each quantile forecast #
  ##########################################
  for(i in 1:length(quantile.FF)){
    
    # Calling the indexed quantile forecasts
    indexedForecast <- as.data.frame(quantile.FF[[i]])
    
    # Calling the name of the indexed quantile forecast
    nameForecast <- names(quantile.FF[i])
    
    # Sub-setting location/group name 
    locationGroupName <- strsplit(nameForecast, "[-]")[[1]][2]
    
    # Model type 
    model.FF <- strsplit(nameForecast, "[-]")[[1]][1]
    
    # Forecast period for weekly or daily data 
    if(dateType.FF %in% c("week", "day")){
      
      # Determining the forecast period from the name
      forecastPeriod <- substring(nameForecast, regexpr("-", nameForecast) + (nchar(locationGroupName) + 2))

      # Forecast period for yearly or time index data 
      }else{
        
        # Determining the forecast period from the name
        forecastPeriod <- strsplit(nameForecast, "[-]")[[1]][3]
        
      }
    
    # Determining the forecast length
    forecastLength <- as.numeric(horizon.input.FF)
    
    # Renaming the first column of the formatted forecasts
    names(crude.data.FF)[1] <- "dates"
    
    ############################
    # Preparing the crude data #
    ############################
    filteredData <- crude.data.FF %>%
      select("dates", locationGroupName) # Selecting needed columns 
    
    #########################
    # Dates for weekly data #
    #########################
    if(dateType.FF == "week"){
      
      # Forecast dates 
      forecastDates <- c(seq.Date(anydate(forecastPeriod) + 7, (anydate(forecastPeriod) + as.numeric(forecastLength)*7), 7))

      # Calibration dates 
      calibrationDates <- c(seq.Date(anydate(forecastPeriod) - ((as.numeric(calibration.FF)*7) - 7), anydate(forecastPeriod), 7))
      
      # Combining the dates
      allDates <- c(calibrationDates, forecastDates)
      
    }else if(dateType.FF == "day"){
      
      # Forecast dates 
      forecastDates <- c(seq.Date(anydate(forecastPeriod) + 1, (anydate(forecastPeriod) + as.numeric(forecastLength)), 1))
      
      # Calibration dates 
      calibrationDates <- c(seq.Date(anydate(forecastPeriod) - (as.numeric(calibration.FF) - 1), anydate(forecastPeriod), 1))
      
      # Combining the dates
      allDates <- c(calibrationDates, forecastDates)
      
    #######################################
    # Dates for yearly or time index data #
    #######################################
    }else{
      
      # Forecast dates 
      forecastDates <- c(seq(as.numeric(forecastPeriod) + 1, (as.numeric(forecastPeriod) + as.numeric(forecastLength)), 1))
      
      # Calibration dates 
      calibrationDates <- c(seq(as.numeric(forecastPeriod) - (as.numeric(calibration.FF) - 1), as.numeric(forecastPeriod), 1))
      
      # Combining the dates
      allDates <- c(calibrationDates, forecastDates)
      
    }
    
    ######################################################
    # Pulling the needed data from the quantile forecast #
    ######################################################
    
    # Determining which quantile to pull 
    chosenQuantile <- quantile.selected.FF
    
    # Smoothing input indicator
    if(is.null(smoothing.input.FF)){
      
      smoothingIndicator <- 0
      
    }else if(0 <= smoothing.input.FF & smoothing.input.FF <= 1){
      
      smoothingIndicator <- 0
      
    }else{
      
      smoothingIndicator <- 1
      
    }
    
    # Dates for forecast file
    if(model.FF == "ARIMA" || (smoothingIndicator == 1 & model.FF != "Prophet")){
      
      datesFinal <- forecastDates
      
    }else{
      
      datesFinal <- allDates
    }
    
    #########################################
    # Determining which variables to select #
    #########################################
    if(model.FF == "Prophet"){
      
    # Changing the forecast to a data frame
    indexedForecast2 <- data.frame(indexedForecast) %>%
      dplyr::mutate(Dates = datesFinal, 
                    data = NA) %>%
      dplyr::select(Dates, data, prediction, paste0("lower.", chosenQuantile, "."), paste0("upper.", chosenQuantile, "."))
    
    }else{
      
      # Changing the forecast to a data frame
      indexedForecast2 <- data.frame(indexedForecast) %>%
        dplyr::mutate(Dates = datesFinal, 
                      data = NA) %>%
        dplyr::select(Dates, data, means, paste0("lower.", chosenQuantile, "."), paste0("upper.", chosenQuantile, "."))
      
    }
    
    ####################################################################
    # Filtering the original data to keep needed observed data - ARIMA #
    ####################################################################
    
    # Renaming data
    names(filteredData) <- c("Dates", "data")
    
    # Handing dates in the observed data
    if(dateType.FF %in% c("day", "week")){
      
      observedData <- filteredData %>%
        dplyr::mutate(Dates = anytime::anydate(Dates)) %>%
        dplyr::filter(anytime::anydate(Dates) %in% c(allDates))
      
    }else{
      
      observedData <- filteredData %>%
        dplyr::mutate(Dates = as.numeric(Dates)) %>%
        dplyr::filter(as.numeric(Dates) %in% c(allDates))
      
    }
    
    # Combining the data
    formattedForecast <- merge(observedData, indexedForecast2,  by = "Dates", all = T) %>%
      dplyr::select(-data.y) 
    
    # Fixing names
    names(formattedForecast) <- c("Date", "data", "median", "LB", "UB")
    
    # Rounding
    formattedForecast <- formattedForecast %>%
      dplyr::mutate(data = round(data, 2), # Rounding the data 
                    median = round(median, 2), # Rounding Median
                    LB = round(LB, 2), # Rounding LB
                    UB = round(UB, 2)) # Rounding UB
    
    #############################################
    # Saving the formatted data frame in a list #
    #############################################
    
    # Adding the forecast to the list
    formattedForecastList[[i]] <- formattedForecast
    
    # Adding the name 
    names(formattedForecastList)[i] <- nameForecast
    
  }
  
#------------------------------------------------------------------------------#
# Returning the list of formatted forecasts ------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns a list of formatted forecasts to the main script.#
#------------------------------------------------------------------------------#

  ######################
  # Returning the list #
  ######################
  return(formattedForecastList)

}