#------------------------------------------------------------------------------#
#                                                                              #
#         Calculating forecasting metrics for each individual forecast         #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file reads in the individual quantile forecasts produced in an earlier  #
# step, the observed data, and the outputs the mean squared error (MSE), mean  #
# absolute error, 95% prediction interval coverage, and weighted interval      #
# scores (WIS). One file is outputted for each forecasting period, therefore,  #
# if the forecasting horizon is greater than one, the outputted value is       #
# average across forecasting horizons for a single forecasting period.         #                                                
# Applications of the metrics in past works can be found at:                   #
#                                                                              #
# (1) Chowell G, Dahal S, Tariq A, Roosa K, Hyman JM, Luo R. An ensemble       #
#     n-sub-epidemic modeling framework for short-term forecasting epidemic    #
#     trajectories: Application to the COVID-19 pandemic in the USA. PLoS      #
#     Comput Biol. 2022;18: e1010602. doi:10.1371/journal.pcbi.1010602.        #
# (2) Bleichrodt A, Dahal S, Maloney K, Casanova L, Luo R, Chowell G.          #
#     Real-time forecasting the trajectory of monkeypox outbreaks at the       #
#     national and global levels, July-October 2022. BMC Med. 2023;21: 19.     #
#     doi:10.1186/s12916-022-02725-2.                                          #
#------------------------------------------------------------------------------#
# Authors: Amanda Bleichrodt and Gerardo Chowell                               #
#------------------------------------------------------------------------------#

forecastingMetrics <- function(crude.data.input, horizon.input, 
                               date.Type.input, quantile.list.input){
  
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Below takes in the inputs into the function, and saves them under new #        
# variable names to prevent over-writing of values.                            #
#------------------------------------------------------------------------------#
  
  #############################
  # Reading in the crude data #
  #############################
  data.input.FM <- crude.data.input
  
  ###############################
  # Saving the forecast horizon #
  ###############################
  horizon.input.FM <- as.numeric(horizon.input)
  
  #############
  # Date type #
  #############
  date.Type.input.FM <- date.Type.input
  
  ##############################
  # List of quantile forecasts #
  ##############################
  quantile.forecast.input.FM <- quantile.list.input 
  
  ######################################
  # Empty list to add forecast metrics #
  ######################################
  forecastMetrics <- data.frame(Location = NA, 
                                Model = NA, 
                                Date = NA, 
                                meanMSE = NA, 
                                meanMAE = NA, 
                                mean95PI = NA, 
                                meanWIS = NA)
  
  ##############################################
  # Empty list for metrics that can not be run #
  ##############################################
  notRun <- list()
  
  ########################
  # Final list to export #
  ########################
  finalList <- list()
  
  
#------------------------------------------------------------------------------#
# Looping through the quantile forecast ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each quantile forecast, determines if the  #
# forecasting statistics can be determined and subsets the observed data for   #
# later calculations.                                                          #
#------------------------------------------------------------------------------#
  for(q in 1:length(quantile.forecast.input.FM)){
    
    #############################
    # Indexed quantile forecast #
    #############################
    indexedQuantile <- as.data.frame(quantile.forecast.input.FM[[q]])
    
    # Name of the indexed quantile
    indexedQuantileName <- names(quantile.forecast.input.FM[q])
    
    #######################################################
    # Pulling information from the quantile forecast name #
    #######################################################
    
    # Model name
    modelName <- strsplit(indexedQuantileName, "[-]")[[1]][1]
    
    # Location name
    location <- strsplit(indexedQuantileName, "[-]")[[1]][2]
    
    # Forecast period
    
    # Forecast period for weekly or daily data 
    if(date.Type.input.FM %in% c("week", "day")){
      
      # Determining the forecast period from the name
      forecastPeriod <- substring(indexedQuantileName, regexpr("-", indexedQuantileName) + (nchar(location) + 2))
      
      # Forecast period for yearly or time index data 
    }else{
      
      # Determining the forecast period from the name
      forecastPeriod <- strsplit(indexedQuantileName, "[-]")[[1]][3]
      
    }
    
    ############################################
    # Determining the calibration period dates #
    ############################################
    forecastDates <- switch(date.Type.input.FM,
                            "week" = c(seq.Date(anydate(forecastPeriod) + 7, anydate(forecastPeriod) + (horizon.input.FM * 7), by = "1 week")),
                             "day" = c(seq.Date(anydate(forecastPeriod) + 1, anydate(forecastPeriod) + horizon.input.FM, by = "1 day")),
                              c(seq(as.numeric(forecastPeriod) + 1, as.numeric(forecastPeriod) + horizon.input.FM, by = 1))
    )
    
    ###############################
    # Preparing the observed data #
    ###############################
    if(date.Type.input.FM %in% c("day", "week")){
      
      observedData <- data.input.FM %>%
        dplyr::filter(anytime::anydate(data.input.FM[,1]) %in% c(forecastDates)) %>% # Filtering dates
        dplyr::select(location) # Selecting the right group 
      
    }else{
      
      observedData <- data.input.FM %>%
        dplyr::filter(as.numeric(data.input.FM[,1]) %in% c(forecastDates)) %>% # Filtering dates
        dplyr::select(location) # Selecting the right group 
      
    }

    
    ##################################
    # Checking if metrics can be run #
    ##################################
    if(nrow(observedData) < horizon.input.FM){
      
      # Empty data frame
      notRunData <- NA
      
      # Creating the data frame to add to list
      notRunData <- data.frame(Model = modelName, 
                               Location = location, 
                               Date = forecastPeriod)
      
      # Adding it to the list
      notRun[[q]] <- notRunData
      
      # Skipping to next loop iteration
      next
    
    ######################################################
    # Saving an NA in the row if the forecast can be run #
    ######################################################
    }else{
      
      # Adding a NA to the list
      notRun[[q]] <- NA
      
    }
    
    ########################################
    # Preparing the quantile forecast data #
    ########################################
    if(modelName == "Prophet"){
      
      quantileForecastCleaned <- indexedQuantile[(nrow(indexedQuantile) - (horizon.input.FM - 1)):nrow(indexedQuantile),] %>%
        dplyr::select(prediction, `lower.95%`, `upper.95%`) %>% # Selecting needed columns
        dplyr::mutate(observed = observedData[,1]) %>% # Adding observed data 
        dplyr::rename("means" = prediction)
      
    }else{
      
      quantileForecastCleaned <- indexedQuantile[(nrow(indexedQuantile) - (horizon.input.FM - 1)):nrow(indexedQuantile),] %>%
        dplyr::select(means, `lower.95%`, `upper.95%`) %>% # Selecting needed columns
        dplyr::mutate(observed = observedData[,1]) # Adding observed data 
      
    }
    
    # Vector of observed data
    true_values <- c(observedData[,1])
    
#------------------------------------------------------------------------------#
# Calculating 95% PI, MSE, and MAE for forecast metrics ------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the 95% PI, MSE, and MAE for the forecast     #
# metrics.                                                                     #
#------------------------------------------------------------------------------#
    
    PI_MSE_MAE <- quantileForecastCleaned %>%
      dplyr::mutate(inCoverage = ifelse(observed <= `upper.95%` & observed >= `lower.95%`, 1, 0), # Determining if in coverage
                    PercentCoverage = (sum(inCoverage)/horizon.input.FM)*100, # Calculating the percent coverage
                    mean95PI = mean(PercentCoverage)) %>% # Avg 95% PI
      dplyr::select(observed, means, mean95PI) %>% # Selected needed variables 
      dplyr::mutate(MSE = ((observed-means)^2), # Calculating MSE
                    MAE = (abs(observed - means)), # Calculating MAE
                    meanMAE = mean(MAE), # Avg MAE
                    meanMSE = mean(MSE)) %>% # Avg MSE
      dplyr::select(meanMSE, meanMAE, mean95PI) %>% # Selecting the needed variables 
      dplyr::distinct(.keep_all = T) # Keeping only unique rows 
    

#------------------------------------------------------------------------------#
# Calculating Weighted Interval Scores for the forecast metrics ----------------
#------------------------------------------------------------------------------#
# About: This section calculates the average WIS for the forecast metrics.     #
#------------------------------------------------------------------------------#
    
    ##################################################
    # Preparing for the loop going through quantiles #
    ##################################################
    
    # Indexed quantile forecast
    data.WIS <- indexedQuantile[c(nrow(indexedQuantile) - horizon.input.FM + 1):nrow(indexedQuantile),]
    
    # Alphas related to quantiles 
    alphas <- c(0.02, 0.05, seq(0.1, 0.9, by = 0.1))
    
    # Variable used below 
    w0 <- 1/2;
    
    # Creating the sum vector for later use
    sum1 <- 0;
    
    # Variable used below 
    K <- length(alphas);
    
    # Empty data frame 
    WISF <- data.frame(row.names = NULL)
    
    #########################################
    # Looping through forecast period dates #
    #########################################
    for(j in 1:length(true_values)){
      
      # Column to start with 
      colx <- 12;
      
      # Resetting the sum 
      sum1 <- 0;
      
      # Observed data during week one of calibration period 
      y <- true_values[j]
      
      # Empty list for interval scores 
      IS <- list()
      
      ##########################
      # Looping through alphas #
      ##########################
      for(k in 1:K){
        
        # Indexed alpha 
        alpha <- alphas[k]
        
        # Alpha/2
        w_k <- alpha/2
        
        # Lower bound associated with indexed date and alpha 
        Lt <- data.WIS[j,colx]
        
        # Upper bound associated with indexed date and alpha 
        Ut <- data.WIS[j,colx+11]
        
        ##################################
        # Calculating the Interval Score #
        ##################################
        IS[k]= c((Ut-Lt)+(2/alpha)*(Lt-y)*(y<Lt)+(2/alpha)*(y-Ut)*(y>Ut))
        
        # Used in WIS calculation 
        sum1 <- sum1+w_k*as.numeric(IS[k])
        
        # Moving the column index 
        colx <- colx-1
        
      } # End of loop going through the alphas 
      
      ####################################################
      # Mean prediction for the current indexed forecast #
      ####################################################
      m <- data.WIS[j, 1]
      
      ###################
      # Calculating WIS #
      ###################
      WISF[j,1] <- (1/(K+1/2))*(w0*abs(y-m) + sum1)
      
    } # End of loop through observed values
    
#------------------------------------------------------------------------------#
# Creating the final data frame with all metrics -------------------------------
#------------------------------------------------------------------------------#
# About: This section combines all average metrics into one data frame, and    #
# saves them in a list for later exportation.                                  #
#------------------------------------------------------------------------------#
    
    #########################
    # Combining all metrics #
    #########################
    allMetrics <- PI_MSE_MAE %>%
      dplyr::mutate(meanWIS = mean(WISF[,1]),
                    Model = modelName,
                    Location = location,
                    Date = forecastPeriod) %>%
      dplyr::select(Location, Model, Date, meanMSE, meanMAE, mean95PI, meanWIS)
    
    ##################################
    # Adding the metrics to the list #
    ##################################
    forecastMetrics <- rbind(forecastMetrics, allMetrics)

    
  } # End of calibration loop
    

#------------------------------------------------------------------------------#
# Adding the 'Not-Run' list to the metrics list, and exporting -----------------
#------------------------------------------------------------------------------#
# About: This section adds the non run list to the main metrics list, and      #
# outputs it to the main list that is exported.                                #
#------------------------------------------------------------------------------#

  # Removing NA in Model_Fits data
  forecastMetrics <- forecastMetrics[-1,]
  
  
  # Returning the list
  return(forecastMetrics)
  
}    