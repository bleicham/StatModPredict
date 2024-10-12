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
# forecast. It returns a list of formatted forecasts which are later plotted   #
# and filtered.                                                                #
#------------------------------------------------------------------------------#
#                       Author: Amanda Bleichrodt                              #                             
#------------------------------------------------------------------------------#
formatted.forecast.function <- function(quantile.input, data.input, 
                                        dateType.input,model.input, 
                                        quantile.selected.input,
                                        horizon.input, smoothing.input,
                                        data.update, quantileSelected) {

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
  
  #########################
  # Updated observed data #
  #########################
  updated.data.FF <- data.update
  
  #####################
  # Selected Quantile #
  #####################
  selectedQuantile <- quantileSelected
  
  ###########################
  # Creating the empty list #
  ###########################
  formattedForecastList <- list()
  
#------------------------------------------------------------------------------#
# Preparing for the formatted forecast list ------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each quantile forecast to produce the      #
# formatted forecast files. It then saves the formatted forecasts to a list to #
# be shown in the dashboard.                                                   #
#------------------------------------------------------------------------------#
  
  ##########################################
  # Looping through each quantile forecast #
  ##########################################
  for(i in 1:length(quantile.FF)){
    
    # Calling the indexed quantile forecasts
    indexedForecast <- as.data.frame(quantile.FF[[i]])
    
    # Calling the name of the indexed quantile forecast
    nameForecast <- names(quantile.FF[i])
    
    ###########################################################
    # Pulling the location and model names from the file name #
    ###########################################################
    
    # Model 
    model.FF <- qdapRegex::ex_between(nameForecast, "", "-")[[1]][1]
    
    # Location
    locationGroupName <- qdapRegex::ex_between(nameForecast, paste0(model.FF,"-"), "-")[[1]][1]
    
    # Determining the forecast length
    forecastLength <- as.numeric(horizon.input.FF)

    #############################################################################
    # Determining the forecast date and calibration period length - Week or Day #
    #############################################################################
    if(dateType.FF %in% c("week", "day")){
      
      # Calibration period 
      calibration.FF <- strsplit(nameForecast, "-")[[1]][7] 
      
      # Determining the forecast period date from the name
      forecastPeriod <- anytime::anydate(paste0(strsplit(nameForecast, "[-]")[[1]][3], "-", strsplit(nameForecast, "[-]")[[1]][4], "-", strsplit(nameForecast, "[-]")[[1]][5]))
      
    ####################################################################################
    # Determining the forecast date and calibration period length - Year or Time Index #
    ####################################################################################
    }else{
      
      # Calibration period 
      calibration.FF <- strsplit(nameForecast, "-")[[1]][5] 
      
      # Determining the forecast period date from the name
      forecastPeriod <- strsplit(nameForecast, "[-]")[[1]][3]
      
    }
    
#------------------------------------------------------------------------------#
# Preparing the original data for later merging --------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the original data used in forecast production   #
# for later merging with the forecast file. If no updated data file is         #
# indicated, the original data will be used for the entire data column. If a   #
# updated file is indicated, the original data will be used only for the       #
# calibration period.                                                          #
#------------------------------------------------------------------------------#
    
    ##################################################
    # Renaming the first column of the original data #
    ##################################################
    names(crude.data.FF)[1] <- "dates"
    
    ##################################
    # Selecting the needed variables #
    ##################################
    filteredData <- crude.data.FF %>%
      select("dates", locationGroupName) # Selecting needed columns 
    
    # Renaming data columns
    names(filteredData) <- c("Dates", "data")
    
#------------------------------------------------------------------------------#
# Preparing the updated data for later merging ---------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the updated data loaded by the user for later   #
# merging with the forecast file. if no updated data file is indicated, the    #
# original data will be used for the entire data column. If an updated data    #
# file is loaded, the original data will be used for the calibration period    #
# and the updated data will be used for the forecast period.                   #
#------------------------------------------------------------------------------#
    
    ####################################
    # Runs if the updated file is NULL #
    ####################################
    if(is.null(updated.data.FF)){
      
      # Do nothing
      NULL
      
    ########################################
    # Runs if the updated file is not NULL #
    ########################################
    }else{
      
      # Renaming the first column of the updated data 
      names(updated.data.FF)[1] <- "dates"
      
      # Selecting the needed variables 
      updatedfilteredData <- updated.data.FF %>%
        select("dates", locationGroupName) # Selecting needed columns 
      
      # Renaming data columns
      names(updatedfilteredData) <- c("Dates", "data")
      
    } # End of 'else'
    
    
#------------------------------------------------------------------------------#
# Determining the forecast and calibration period dates ------------------------
#------------------------------------------------------------------------------#
# About: This section determines the forecast and calibration period dates     #
# based on the type of date data. It creates vectors of dates to be used in    #
# later filtering of dates.                                                    #
#------------------------------------------------------------------------------#

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
      
    #########################
    # Dates for daily data #
    #########################
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

#------------------------------------------------------------------------------#
# Determining if the original data or uploaded data should be used --------------
#------------------------------------------------------------------------------#
# About: This section determines if the original data should be used for the   #
# data column, or if the uploaded data should be used. If the uploaded data    #
# should be used, it will only update the forecast period values.              #
#------------------------------------------------------------------------------#
    
    ############################################################
    # Runs if the updated file is NULL - Use the original data #
    ############################################################
    if(is.null(updated.data.FF)){
      
      ################################################################
      # Filtering the original data and setting dates - Week and Day #
      ################################################################
      if(dateType.FF %in% c("day", "week")){
        
        observedData <- filteredData %>%
          dplyr::mutate(Dates = anytime::anydate(Dates)) %>% # Setting the dates 
          dplyr::filter(anytime::anydate(Dates) %in% c(allDates)) # Filtering the dates to include all data 
       
      #######################################################################
      # Filtering the original data and setting dates - Year and Time Index #
      #######################################################################
      }else{
        
        #######################################################################
        # Filtering the original data and setting dates - Year and Time Index #
        #######################################################################
        observedData <- filteredData %>%
          dplyr::mutate(Dates = as.numeric(Dates)) %>% # Setting the dates 
          dplyr::filter(as.numeric(Dates) %in% c(allDates)) # Filtering the dates to include all data 
        
      }
    
    ######################################### 
    # Runs if the user uploads updated data #
    #########################################
    }else{
      
      #####################################################
      # Handing dates in the observed data - Week and Day #
      #####################################################
      if(dateType.FF %in% c("day", "week")){
        
        # Calibration data
        calibrationData <- updatedfilteredData %>%
          dplyr::mutate(Dates = anytime::anydate(Dates)) %>%
          dplyr::filter(anytime::anydate(Dates) %in% c(calibrationDates))
        
        # Forecast data
        forecastData <- updatedfilteredData %>%
          dplyr::mutate(Dates = anytime::anydate(Dates)) %>%
          dplyr::filter(anytime::anydate(Dates) %in% c(forecastDates))
        
        # Creating the observed data
        observedData <- rbind(calibrationData, forecastData)
       
      #######################################################################
      # Filtering the original data and setting dates - Year and Time Index #
      ####################################################################### 
      }else{
        
        # Calibration data
        calibrationData <- updatedfilteredData %>%
          dplyr::mutate(Dates = as.numeric(Dates)) %>%
          dplyr::filter(as.numeric(Dates) %in% c(calibrationDates))
        
        # Forecast data
        forecastData <- updatedfilteredData %>%
          dplyr::mutate(Dates = as.numeric(Dates)) %>%
          dplyr::filter(as.numeric(Dates) %in% c(forecastDates))
        
        # Creating the observed data
        observedData <- rbind(calibrationData, forecastData)
        
      } # End of 'else' for dates
      
    } # End of 'else' for uploaded data 
    
#------------------------------------------------------------------------------#
# Filtering the quantile forecasts ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the quantile forecast based upon the selected   #
# quantile and data smoothing. The file later merged with the correct          #
# observed data.                                                               #
#------------------------------------------------------------------------------#
    
    ###############################################
    # Indicator for data smoothing - No Smoothing #
    ###############################################
    if(is.null(smoothing.input.FF)){
      
      # No smoothing 
      smoothingIndicator <- 0
      
    ###############################################
    # Indicator for data smoothing - No Smoothing #
    ############################################### 
    }else if(0 <= smoothing.input.FF & smoothing.input.FF <= 1){
      
      # No smoothing 
      smoothingIndicator <- 0
      
    ############################################
    # Indicator for data smoothing - Smoothing #
    ############################################
    }else{
      
      # Smoothing 
      smoothingIndicator <- 1
      
    }
    
    ############################################################
    # Determining which dates to merge with quantile forecasts #
    ############################################################
    
    # If working with ARIMA or smoothing and not Prophet models 
    if(model.FF == "ARIMA" || (smoothingIndicator == 1 & model.FF != "Prophet")){
      
      datesFinal <- forecastDates
     
    # Pulling all dates  
    }else{
      
      datesFinal <- allDates
      
    }
    
    #########################################################
    # Determining which variables to select - Prophet model #
    #########################################################
    if(model.FF == "Prophet"){
      
      # Changing the forecast to a data frame
      indexedForecast2 <- data.frame(indexedForecast) %>%
        dplyr::mutate(Dates = datesFinal, 
                      data = NA) %>%
        dplyr::select(Dates, data, prediction, paste0("lower.", quantile.selected.FF, "."), paste0("upper.", quantile.selected.FF, "."))
     
    ######################################################################
    # Determining which variables to select - GLM, GAM, and ARIMA models #
    ###################################################################### 
    }else{
      
      # Changing the forecast to a data frame
      indexedForecast2 <- data.frame(indexedForecast) %>%
        dplyr::mutate(Dates = datesFinal, 
                      data = NA) %>%
        dplyr::select(Dates, data, means, paste0("lower.", quantile.selected.FF, "."), paste0("upper.", quantile.selected.FF, "."))
      
    }
    
#------------------------------------------------------------------------------#
# Combining the quantile forecast and observed data ----------------------------
#------------------------------------------------------------------------------#
# About: This section merges the quantile forecast data frame and the observed #
# data data frame. It then formats the data with the correct names, and rounds #
# the all of the numbers to two decimal points.                                #
#------------------------------------------------------------------------------#
    
    ###################################################
    # Combining the observed and forecast data frames #
    ###################################################
    formattedForecast <- merge(observedData, indexedForecast2,  by = "Dates", all = T) %>%
      dplyr::select(-data.y) 
    
    ##################################
    # Fixing names of the data frame #
    ##################################
    names(formattedForecast) <- c("Date", "data", "median", "LB", "UB")
    
    #########################################
    # Rounding all values of the data frame #
    #########################################
    formattedForecast <- formattedForecast %>%
      dplyr::mutate(data = round(data, 2), # Rounding the data 
                    median = round(median, 2), # Rounding Median
                    LB = round(LB, 2), # Rounding LB
                    UB = round(UB, 2)) # Rounding UB
    
#------------------------------------------------------------------------------#
# Saving and returning the formatted forecast file -----------------------------
#------------------------------------------------------------------------------#
# About: This section adds the formatted forecast to the list of that will be  #
# exported back to the main data frame. It also adds the names of the files to #
# each data frame in the list to be referenced later.                          #
#------------------------------------------------------------------------------#
    
    ###################################
    # Adding the forecast to the list #
    ###################################
    formattedForecastList[[i]] <- formattedForecast
    
    # Adding the name 
    names(formattedForecastList)[i] <- paste0(nameForecast, " (", selectedQuantile, "% PI)")
    
  }
  
  ######################
  # Returning the list #
  ######################
  return(formattedForecastList)

}