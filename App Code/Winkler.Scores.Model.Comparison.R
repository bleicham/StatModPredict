#------------------------------------------------------------------------------#
#                                                                              #
#                 Calculating Winkler Scores - Model Comparison                #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function calculates the Winkler Scores for each of the forecasts        #
# produced in earlier steps of the dashboard and read into the dashboard in    #
# on the Model Comparison page. The Winkler scores provide a measure           #
# to compare prediction interval coverage for model fits and forecasts. The    #
# Winkler scores calculated in this function follow the equation given in:     # 
# https://otexts.com/fpp3/distaccuracy.html.                                   #
#------------------------------------------------------------------------------#
#                         By: Amanda Bleichrodt                                #
#------------------------------------------------------------------------------#
Winkler.Scores.Model.Comparison <- function(formatted.forecast.DASHBOARD,
                                            formatted.forecast.Other,
                                            date.type.input,
                                            avgWinler.input,
                                            quantile.input) {
  
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
# Reading in inputs ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to avoid any type of over-   #
# writing.                                                                     #
#------------------------------------------------------------------------------#
  
  #####################################################################
  # Reading in the formatted forecasts - ARIMA, GLM, GAM, and Prophet #
  #####################################################################
  formatted.forecast.input <- formatted.forecast.DASHBOARD
  
  ##############################################
  # Reading in the formatted forecasts - Other #
  ##############################################
  formatted.forecast.other.input <- formatted.forecast.Other 
  
  ############################
  # Reading in the date type #
  ############################
  dateType <- date.type.input
  
  ##########################
  # Average Winkler Scores #
  ##########################
  averageWinklerIndicator <- avgWinler.input
  
  #####################
  # Selected quantile #
  #####################
  quantileSelected <- quantile.input
  
  ##########################################
  # Data frame to fill with winkler scores #
  ##########################################
  allWinklerScores <- data.frame()
  
  
#------------------------------------------------------------------------------#
# Determining the alpha value --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines the alpha value that is used in the Winkler   #
# score calculations based upon the prediction interval selected by the user.  #
#------------------------------------------------------------------------------#
  
  #########################
  # Calculating the alpha #
  #########################
  alphaToUse <-(100-as.numeric(quantileSelected))/100
  
  
#------------------------------------------------------------------------------#
# Combining the forecast lists -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section combines the forecasts from the dashboard and the        #
# forecasts read in by the user.                                               #
#------------------------------------------------------------------------------#
  
  #####################################################
  # Combining the list if all forecasts are available #
  #####################################################
  if(!is.null(formatted.forecast.input)){
    
    # Combined list
    combinedList <- c(formatted.forecast.input, formatted.forecast.other.input)
    
  ######################################
  # Including only the other forecasts #
  ######################################
  }else{
    
    # Combined list
    combinedList <- formatted.forecast.other.input
    
  }
  
#------------------------------------------------------------------------------#
# Winkler scores Function ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the function to calculate Winkler scores for     #
# each formatted forecast created in the main dashboard. It takes in the UB    #
# and LB, observed data, and alpha value that corresponds to the prediction    #
# interval coverage. 
#------------------------------------------------------------------------------#
  winkler_score <- function(upper.bound, lower.bound, data, alpha){
    
    ##################################################
    # Runs if the observed data is lower than the LB #
    ##################################################
    if(data < lower.bound){
      
      # Calculated Winkler Score
      score <- (upper.bound - lower.bound) + (2/alpha)*(lower.bound-data)
      
    #################################################
    # Runs if the observed data falls in the bounds #
    #################################################
    }else if(data >= lower.bound & data <= upper.bound){
      
      # Calculated Winkler score
      score <- upper.bound - lower.bound
      
    ################################################
    # Runs if the observed data falls above the UB #
    ################################################
    }else{
      
      # Calculated Winkler score 
      score <- (upper.bound - lower.bound) + (2/alpha)*(data - upper.bound)
      
    }
    
    #######################
    # Returning the score #
    #######################
    return(score)
    
  }
  
#------------------------------------------------------------------------------#
# Looping through formatted forecasts ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through the formatted forecasts to determine the   #
# forecast period (i.e., split calibration and forecasts), and to calculate    #
# the Winkler scores, and saving the results for outputting.                   #
#------------------------------------------------------------------------------#
  
  for(w in 1:length(combinedList)){
    
    # Indexed forecast
    indexForecast <- combinedList[[w]]
    
    # Name indexed forecast
    nameForecast <- names(combinedList[w])
    
    ##########################################################
    # Determining what pattern to use in pulling information #
    ##########################################################
    
    # Splitting file name 
    splitName <- strsplit(nameForecast, "[-]")[[1]]
    
    # List of dashboard models
    dashboardModels <- c("ARIMA", "GLM", "GAM", "Prophet")
    
    ###########################################################################
    # Pulling needed information from the list element name: Dashboard Models #
    ###########################################################################
    if(splitName[1] %in% c(dashboardModels)){
      
      # Model name
      model <- qdapRegex::ex_between(nameForecast, "", "-")[[1]][1]
      
      # Location name
      location <- qdapRegex::ex_between(nameForecast, paste0(model, "-"), "-")[[1]][1]
      
      # Calibration period length
      calibrationLength <- as.numeric(qdapRegex::ex_between(nameForecast, "Calibration-", " (")[[1]][1])
      
      # Forecast period: Day or Week
      if(dateType %in% c("week", "day")){
        
        forecastDate <- anytime::anydate(paste0(str_split(nameForecast, pattern = "-")[[1]][3], "-", str_split(nameForecast, pattern = "-")[[1]][4], "-", str_split(nameForecast, pattern = "-")[[1]][5]))
        
      # Forecast period: Year or Time Index
      }else{
        
        forecastDate <- as.numeric(paste0(str_split(nameForecast, pattern = "-")[[1]][3]))
        
      }
      
    #######################################################################
    # Pulling needed information from the list element name: Other Models #
    #######################################################################
    }else{
      
      # Model name
      model <- paste0(splitName[1], "-", splitName[2])
      
      # Location name
      location <- splitName[7]
      
      # Calibration period length
      calibrationLength <- as.numeric(splitName[6])
      
      # Forecast period date - week or day
      if(dateType %in% c("week", "day")){
        
        # Pulling the year
        yearChar <- strsplit(strsplit(nameForecast, "[-]")[[1]][10], "[.]")[[1]][1]
        
        # Forecast period date
        forecastPeriod <- anytime::anydate(paste0(yearChar, "-", strsplit(nameForecast, "[-]")[[1]][8], "-", strsplit(nameForecast, "[-]")[[1]][9]))
        
      # Forecast period date - year or time index 
      }else{
        
        # Forecast period date
        forecastPeriod <- as.numeric(strsplit(strsplit(nameForecast, "[-]")[[1]][8], "[.]")[[1]][1])
        
      }
      
    } # End of 'else'
  
      
#------------------------------------------------------------------------------#
# Preparing the final data -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the final data prior to applying the Winkler    #
# score function.                                                              #
#------------------------------------------------------------------------------#
    
    ##############
    # Final data #
    ##############
    formattedPreWinkler <- indexForecast %>%
      dplyr::mutate(Model = model, # Model type 
                    Date = anytime::anydate(Date), # Date format
                    Location = location, # Location
                    Calibration = calibrationLength, # Calibration length 
                    `Forecast Date` = forecastDate, # Forecast date
                     CalibrationIndicator = ifelse(Date <= `Forecast Date`, 1, 0)) %>%
      dplyr::mutate(`Winkler Score` = NA) %>% # Empty column for later step 
      dplyr::select(CalibrationIndicator, Model, Location, Calibration, `Forecast Date`, Date, data, median, LB, UB, `Winkler Score`)
    
    
#------------------------------------------------------------------------------#
# Checking if the Winkler Scores can be calculated for the forecast ------------
#------------------------------------------------------------------------------#
# About: This section checks if all observed data is available for the         #
# forecast period. If it is not, the forecast rows are removed from the data.  #
#------------------------------------------------------------------------------#
    
    ############################
    # Sub-setting the forecast #
    ############################
    forecast.temp <- formattedPreWinkler %>%
      dplyr::filter(CalibrationIndicator == 0)
    
    ####################
    # Checking for NAs #
    ####################
    if(any(is.na(forecast.temp$data))){
      
      formattedPreWinkler <- formattedPreWinkler %>%
        dplyr::filter(CalibrationIndicator == 1)
      
    }else{
      
      formattedPreWinkler <- formattedPreWinkler 
      
    }
    
#------------------------------------------------------------------------------#
# Applying the Winkler Score function ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section applies the Winkler Score function to each row of the    #
# formatted forecast. Later steps will calculate the average Winkler score for #
# each forecast period, which is then exported in a later step.                #
#------------------------------------------------------------------------------#
    
    ####################################
    # Looping through rows of data set #
    ####################################
    for(r in 1:nrow(formattedPreWinkler)){
      
      # Handling NAs in the data - Skipping that row 
      if(is.na(formattedPreWinkler[r,9]) | is.na(formattedPreWinkler[r,10])){
        
        next
        
      }
      
      # Indexed row 
      winklerScore <- winkler_score(upper.bound = formattedPreWinkler[r,10], 
                                    lower.bound = formattedPreWinkler[r,9], 
                                    data = formattedPreWinkler[r,7], 
                                    alpha = alphaToUse)
      
      # Adding the score to the data frame
      formattedPreWinkler[r,11] <- winklerScore
      
    } # End of row loop
    
    
#------------------------------------------------------------------------------#
# Preparing the final Winkler score data frames --------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the final data frame with Winkler scores for    #
# exporting to the main dashboard. It also calculates the average Winker score #
# for each forecast period date, location, calibration combonation.            #
#------------------------------------------------------------------------------#
    
    ####################################
    # Preparing the data for exporting #
    ####################################
    
    # If working with daily or weekly data
    if(dateType %in% c("week", "day")){
      
      winklerData <- formattedPreWinkler %>%
        dplyr::mutate(`Forecast Date` = anytime::anydate(`Forecast Date`), 
                      Date = anytime::anydate(Date)) %>%
        dplyr::select(CalibrationIndicator, Model, Location, Calibration, Date, `Forecast Date`, `Winkler Score`) %>%
        dplyr::group_by(CalibrationIndicator) %>%
        dplyr::mutate(`Avg. Winkler Score` = round(mean(`Winkler Score`), 2))
      
    # If working with yearly or time index data  
    }else{
      
      winklerData <- formattedPreWinkler %>%
        dplyr::mutate(`Forecast Date` = as.numeric(`Forecast Date`),
                      Date = as.numeric(Date)) %>%
        dplyr::select(CalibrationIndicator, Model, Location, Calibration, Date, `Forecast Date`, `Winkler Score`) %>%
        dplyr::group_by(CalibrationIndicator) %>%
        dplyr::mutate(`Avg. Winkler Score` = round(mean(`Winkler Score`), 2))
    }
    
    ####################################################
    # Adding the winkler score to the final data frame #
    ####################################################
    allWinklerScores <- rbind(allWinklerScores, winklerData)
    
  } # End of loop going through forecast files 
  
#------------------------------------------------------------------------------#
# Determining if the Winkler Scores should be averaged across forecast dates ---
#------------------------------------------------------------------------------#
# About: This section averages the Winkler scores across forecast period dates #
# if indicated by the user. If it is not indicated, the scores remain for      #
# each forecast period date.                                                   #
#------------------------------------------------------------------------------#
  
  ###############################
  # Calculating average metrics #
  ###############################
  if(averageWinklerIndicator == 1){
    
    finalWinkler <- allWinklerScores %>%
      dplyr::group_by(Location, Model, Calibration, CalibrationIndicator) %>%
      dplyr::mutate(`Avg. Winkler` = round(mean(`Avg. Winkler Score`, na.rm = T), 2)) %>% # Calculating the average Winkler Score
      dplyr::distinct(Location, Model, Calibration, CalibrationIndicator, .keep_all = T) %>% # Removing un-needed rows 
      na.omit() %>% # Removing NA rows
      dplyr::select(CalibrationIndicator, Location, Model, Calibration,`Avg. Winkler`) %>% # Selecting needed variables 
      dplyr::rename("Type" = CalibrationIndicator) %>% # Renaming the type indicator 
      dplyr::mutate(Type = ifelse(Type == 1, "Fit", "Forecast"))
    
    
  #############################   
  # Keeping the crude metrics #
  #############################
  }else{
    
    finalWinkler <- allWinklerScores %>%
      dplyr::select(CalibrationIndicator, Location, Model, Calibration, `Forecast Date`, `Avg. Winkler Score`) %>% # Selecting needed variables 
      dplyr::distinct(Location, Model, Calibration, CalibrationIndicator, `Forecast Date`, .keep_all = T) %>% # Removing un-needed rows 
      na.omit() %>% # Removing NA rows
      dplyr::rename("Winkler Score" = `Avg. Winkler Score`,  # Renaming the column containing Winkler Scores 
                    "Type" = CalibrationIndicator) %>% # Renaming the type indicator 
      dplyr::mutate(Type = ifelse(Type == 1, "Fit", "Forecast"))
    
  }
  
  ######################################
  # Returning the final Winkler Scores #
  ######################################
  return(finalWinkler)
  
}
    