#------------------------------------------------------------------------------#
#                                                                              #
#          Calculating Winkler Scores - ARIMA/GLM/GAM/Prophet Toolbox          #
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
                                locations.input,
                                models.input,
                                filterIndicator.input,
                                averageIndicator.input,
                                metricPage.input) {

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
  
  ############################
  # Reading in the locations #
  ############################
  locationsInput <<- locations.input
  
  #########################
  # Reading in the models #
  #########################
  modelsInput <<- models.input
  
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
  
  ##########################################
  # Data frame to fill with winkler scores #
  ##########################################
  allWinklerScores <- data.frame()


#------------------------------------------------------------------------------#
# Winkler scores function ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the function to calculate Winkler scores for     #
# each formatted forecast executed in the main dashboard.                      #
#------------------------------------------------------------------------------#
  winkler_score <- function(upper.bound, lower.bound, data){
    
    ##################################################
    # Runs if the observed data is lower than the LB #
    ##################################################
    if(data < lower.bound){
      
      # Calculated Winkler Score
      score <- (upper.bound - lower.bound) + (2/0.05)*(lower.bound-data)
      
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
      score <- (upper.bound - lower.bound) + (2/0.05)*(data - upper.bound)
      
    }
    
    #######################
    # Returning the score #
    #######################
    return(score)
    
  }

#------------------------------------------------------------------------------#
# Looping through forematted forecasts -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through the formatted forecasts to determine the   #
# forecast period (i.e., split calibration and forecasts), and to calculate    #
# the winkler scores, and saving the results for outputting.                   #
#------------------------------------------------------------------------------#

  for(w in 1:length(formatted.forecast.input)){
    
    # Indexed forecast
    indexForecast <- formatted.forecast.input[[w]]
    
    # Name indxed forecast
    nameForecast <- names(formatted.forecast.input[w])
    
    #########################################################
    # Pulling needed information from the list element name #
    #########################################################
    
    # Model name
    model <- qdapRegex::ex_between(nameForecast, "", "-")[[1]][1]
    
    # Location name
    location <- qdapRegex::ex_between(nameForecast, paste0(model, "-"), "-")[[1]][1]
    
    # Adjusting for possible parenthesis in the name
    if(grepl("\\)", location) | grepl("\\(", location)) {
      
      # Adding \\ before the first instance of parenthesis 
      firstParenthesis <- gsub("\\(", "\\\\(", location)
      
      # Adding \\ before he last instance of parenthesis 
      locationForDate <- gsub("\\)", "\\\\)", firstParenthesis)
      
    }else{
      
      locationForDate <- location
      
    }
    
    # Forecast period
    forecastDate <- sub(paste0('.*-', locationForDate, '-'), '', nameForecast)
    
    #######################################################
    # Formatted the data frame prior to applying function #
    #######################################################
    
    formattedPreWinkler <- indexForecast %>%
      dplyr::mutate(Model = model, # Model type 
                    Location = location, # Location
                    `Forecast Date` = forecastDate, # Forecast date
                    `Winkler Score` = NA) # Empty column for later step 
    
    ########################################
    # Applying the winkler scores function #
    ########################################
    
    # Looping through rows of data set
    for(r in 1:nrow(formattedPreWinkler)){
      
      # Handling NAs in the data - Skipping that row 
      if(is.na(formattedPreWinkler[r,5]) | is.na(formattedPreWinkler[r,4]) | is.na(formattedPreWinkler[r,2])){
        
        next
        
      }
      
      # Indexed row 
      winklerScore <- winkler_score(formattedPreWinkler[r,5], formattedPreWinkler[r,4], formattedPreWinkler[r,2])
      
      # Adding the score to the data frame
      formattedPreWinkler[r,9] <- winklerScore
      
    } # End of row loop
    
    ####################################
    # Preparing the data for exporting #
    ####################################
    
    # If working with daily or weekly data
    if(nchar(formattedPreWinkler[1,1]) > 4){
      
      # Final data set to export 
      winklerData <- formattedPreWinkler %>%
        dplyr::mutate(`Forecast Date` = anytime::anydate(`Forecast Date`)) %>% # Formatted forecast date
        dplyr::mutate(CalibrationIndicator = ifelse(Date <= `Forecast Date`, 1, 0)) %>% # Indicator for calibration period 
        dplyr::select(Location, Model, Date, `Forecast Date`, `Winkler Score`, CalibrationIndicator) %>% # Selecting needed variables 
        dplyr::group_by(CalibrationIndicator) %>% # Grouping by forecast period type 
        dplyr::mutate(avgWinkler = round(mean(`Winkler Score`), 2)) # Average Winkler score across forecast or calibration periods 
    
    # If working with yearly or time index data  
    }else{
      
      # Final data set to export 
      winklerData <- formattedPreWinkler %>%
        dplyr::mutate(`Forecast Date` = as.numeric(`Forecast Date`)) %>% # Formatted forecast date
        dplyr::mutate(CalibrationIndicator = ifelse(Date <= `Forecast Date`, 1, 0)) %>% # Indicator for calibration period 
        dplyr::select(Location, Model, Date, `Forecast Date`, `Winkler Score`, CalibrationIndicator) %>% # Selecting needed variables 
        dplyr::group_by(CalibrationIndicator) %>% # Grouping by forecast period type 
        dplyr::mutate(avgWinkler = round(mean(`Winkler Score`), 2)) # Average Winkler score across forecast or calibration periods
      
    }
    
    ####################################################
    # Adding the winkler score to the final data frame #
    ####################################################
    allWinklerScores <- rbind(allWinklerScores, winklerData)
    
  } # End of loop going through forecast files 
  
  
#------------------------------------------------------------------------------#
# Determining if average or crude metrics need to be calculated ----------------
#------------------------------------------------------------------------------#
# About: This section calculates the average metrics if indicated by the user. #
#------------------------------------------------------------------------------#
  
  ###############################
  # Calculating average metrics #
  ###############################
  if(averageWinklerIndicator == 1){
    
    finalWinkler <- allWinklerScores %>%
      dplyr::group_by(Location, Model, CalibrationIndicator) %>%
      dplyr::mutate(`Avg. Winkler` = round(mean(avgWinkler, na.rm = T), 2)) %>% # Calculating the average Winkler Score
      dplyr::distinct(Location, Model, CalibrationIndicator, .keep_all = T) %>% # Removing un-needed rows 
      na.omit() %>% # Removing NA rows
      dplyr::select(Location, Model, CalibrationIndicator, `Avg. Winkler`) # Selecting needed variables 
   
  #############################   
  # Keeping the crude metrics #
  #############################
  }else{
    
    finalWinkler <- allWinklerScores %>%
      dplyr::select(Location, Model, `Forecast Date`, CalibrationIndicator, avgWinkler) %>% # Selecting needed variables 
      dplyr::distinct(Location, Model, CalibrationIndicator, `Forecast Date`, .keep_all = T) %>% # Removing un-needed rows 
      na.omit() %>% # Removing NA rows
      dplyr::rename("Winkler Score" = avgWinkler) # Renaming the colum containing Winkler Scores 
    
  }
  
#------------------------------------------------------------------------------#
# Determining when to use the specified filtering ------------------------------
#------------------------------------------------------------------------------#
# About: Based upon the filtering indicator, this section determines if the    #
# metrics should be filtered or not.                                           #
#------------------------------------------------------------------------------#
  
  ###########################################################
  # Creating the indicator for page-specific Winkler Scores #
  ###########################################################
  pageIndicator <- switch (metricToShow,
    "Model Fit" = 1,
    0
  )
  
  #############################
  # Not filtering the metrics #
  #############################
  if(filteringIndicator == 0){
    
    # Data with no filtering
    filteredWinkler <- finalWinkler %>%
      dplyr::ungroup() %>% # Ungrouping 
      dplyr::filter(CalibrationIndicator == pageIndicator) %>% # Page specific metrics 
      dplyr::select(-CalibrationIndicator) # Removing the page-specific indicator 
    
  #########################
  # Filtering the metrics #
  #########################
  }else{
    
    # Data with no filtering
    filteredWinkler <- finalWinkler %>%
      dplyr::ungroup() %>% # Ungrouping 
      dplyr::filter(CalibrationIndicator == pageIndicator, # Page specific metrics
                    Location %in% c(locationsInput), # Filtering locations 
                    Model %in% c(modelsInput)) %>% # Filtering Models 
      dplyr::select(-CalibrationIndicator) # Removing the page-specific indicator 
    
    
  }
  
    ################################################
    # Returning the data frame with winkler scores #
    ################################################
    return(filteredWinkler)
  
  } # End of function

  
  
 