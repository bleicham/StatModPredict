#------------------------------------------------------------------------------#
#                                                                              #
#                         Calculating Winkler Scores                           #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function calculates the Winkler Scores for each of the forecasts        #
# produced in earlier steps of the dashboard. Winkler scores provide a measure #
# to compare prediction interval coverages for model fits and forecasts. The   #
# Winkler scores calculated in this function follow the equation given in:     # 
# https://otexts.com/fpp3/distaccuracy.html.                                   #
#------------------------------------------------------------------------------#
#                         By: Amanda Bleichrodt                                #
#------------------------------------------------------------------------------#
winkler.scores.AGGP <- function(formattedForecasts,
                                filterIndicator.input,
                                averageIndicator.input,
                                metricPage.input, quantile.input,
                                orginData.input,  date.type.input,
                                horizon.input) {

  
#------------------------------------------------------------------------------#
# Reading in inputs ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to avoid any type of over-   #
# writing.                                                                     #
#------------------------------------------------------------------------------#

  ######################################
  # Reading in the formatted forecasts #
  ######################################
  formatted.forecast.input <<- formattedForecasts
  
  #######################
  # Filtering indicator #
  #######################
  filteringIndicator <<- filterIndicator.input
  
  ####################################
  # Average Winkler Scores Indicator #
  ####################################
  averageWinklerIndicator <<- averageIndicator.input
  
  ###################
  # Metrics to show #
  ###################
  metricToShow <<- metricPage.input
  
  ##################
  # Quantile input #
  ##################
  quantileSelected <<- quantile.input
  
  ###################
  # Comparison data #
  ###################
  compareData <<- orginData.input
  
  #############
  # Date type #
  #############
  dateType <<- date.type.input
  
  ####################
  # Forecast Horizon #
  ####################
  forecastHorzion <<- 4
  
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

  for(w in 1:length(formatted.forecast.input)){
    
    # Indexed forecast
    indexForecast <- formatted.forecast.input[[w]]
    
    # Name indexed forecast
    nameForecast <- names(formatted.forecast.input[w])
    
    #########################################################
    # Pulling needed information from the list element name #
    #########################################################
    
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
    
#------------------------------------------------------------------------------#
# Determining the observed data to use -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines which observed data should be used in the     #
# Winkler score calculations.                                                  #
#------------------------------------------------------------------------------#
    
    #################################
    # Determining the dates to pull #
    #################################
    datesToPull <- c(indexForecast$Date)
    
    #################################################
    # Pulling the data from the observed data frame #
    #################################################
    
    # Renaming column one
    colnames(compareData)[1] <- "Date"
  
    # Filtering the data 
    observedData <- compareData %>%
      dplyr::filter(Date %in% c(datesToPull)) %>%
      dplyr::select(Date, location)
    
    # Renaming the last column
    colnames(observedData)[2] <- "Obs."
    
    #########################################################
    # Combining the original forecast with the compare data #
    #########################################################
    combinedData <- merge(indexForecast, observedData, by = "Date", all = T)
    
#------------------------------------------------------------------------------#
# Preparing the final data -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the final data prior to applying the Winkler    #
# score function.                                                              #
#------------------------------------------------------------------------------#
    
    ##############
    # Final data #
    ##############
    formattedPreWinkler <- combinedData %>%       
      dplyr::mutate(Model = model, # Model type 
                    Location = location, # Location
                    Calibration = calibrationLength, # Calibration length 
                    `Forecast Date` = forecastDate, # Forecast date
                    data = `Obs.`, # Replacing the observed data
                    CalibrationIndicator = ifelse(Date <= `Forecast Date`, 1, 0), # Calibration indicator 
                    `Winkler Score` = NA) %>% # Empty column for later step 
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
        na.omit() %>%
        dplyr::mutate(`Forecast Date` = anytime::anydate(`Forecast Date`)) %>%
        dplyr::select(CalibrationIndicator, Model, Location, Calibration, Date, `Forecast Date`, `Winkler Score`) %>%
        dplyr::group_by(CalibrationIndicator) %>%
        dplyr::mutate(`Avg. Winkler Score` = round(mean(`Winkler Score`), 2))
      
    # If working with yearly or time index data  
    }else{
      
      winklerData <- formattedPreWinkler %>%
        na.omit() %>%
        dplyr::mutate(`Forecast Date` = as.numeric(`Forecast Date`)) %>%
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
      dplyr::select(Location, Model, Calibration, CalibrationIndicator, `Avg. Winkler`) # Selecting needed variables 
   
  #############################   
  # Keeping the crude metrics #
  #############################
  }else{
    
    finalWinkler <- allWinklerScores %>%
      dplyr::select(Location, Model, Calibration, `Forecast Date`, CalibrationIndicator, `Avg. Winkler Score`) %>% # Selecting needed variables 
      dplyr::distinct(Location, Model, Calibration, CalibrationIndicator, `Forecast Date`, .keep_all = T) %>% # Removing un-needed rows 
      na.omit() %>% # Removing NA rows
      dplyr::rename("Winkler Score" = `Avg. Winkler Score`) # Renaming the column containing Winkler Scores 

  }
  
#------------------------------------------------------------------------------#
# Filtering the Winkler Scores -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section applies the necessary filtering selections to the data.  #
# For example, if working with model fit statistics, it shows the Winkler      #
# Score for the fit or forecast.                                               #
#------------------------------------------------------------------------------#
  
  ###########################################################
  # Creating the indicator for page-specific Winkler Scores #
  ###########################################################
  pageIndicator <- switch (metricToShow,
    "Model Fit" = 1,
    0
  )
  
  ######################################
  # Preparing the final Winkler scores #
  ######################################
  filteredWinkler <- finalWinkler %>%
      dplyr::ungroup() %>% # Ungrouping 
      dplyr::filter(CalibrationIndicator == pageIndicator) %>% # Page specific metrics 
      dplyr::select(-CalibrationIndicator) # Removing the page-specific indicator 
    

  ################################################
  # Returning the data frame with winkler scores #
  ################################################
  return(filteredWinkler)
  
  
  } # End of function

  
  
 