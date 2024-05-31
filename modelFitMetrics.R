#------------------------------------------------------------------------------#
#                                                                              #
#           Calculating model fit metrics for each individual forecast         #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file reads in the individual quantile forecasts and model fits produced #
# in an earlier step, the observed data, and the outputs the mean squared      #
# error (MSE), mean absolute error, 95% prediction interval coverage, and      #
# weighted interval scores (WIS). One file is outputted for each forecasting   #
# period, therefore, if the forecasting horizon is greater than one, the       #
# outputted value is average across forecasting horizons for a single          #
# forecasting period.                                                          #
#                                                                              #
# A detailed description of the calculations used for MSE, MAE, 95% PI         #
# coverage, and WIS can be found at:                                           #
#                                                                              #
# (1) Gneiting T, Raftery AE. Strictly proper scoring rules, prediction, and   #
#     estimation. Journal of the American statistical Association. 2007;102    #
#     (477):359-378. https://doi.org/10.1198/016214506000001437.               #
# (2) Kuhn M, Johnson K. Applied predictive modeling. Vol 26. Springer; 2013.  #
# (3) University of Nicosia. M4Competition Competitor’s Guide: Prizes and      # 
#     Rules. 2018. Accessed June 28, 2023. http://www.unic.ac.cy/test/wp-      #
#     content/uploads/sites/2/2018/09/M4-Competitors-Guide.pdf.                #
# (4) Bracher J, Ray EL, Gneiting T, Reich NG. Evaluating epidemic forecasts   #
#     in an interval format. PLoS computational biology. 2021;17(2):e1008618.  #
#     https://doi.org/10.1371/journal.pcbi.1008618.                            #
# (5) Cramer EY, Ray EL, Lopez VK, et al. Evaluation of individual and         #
#     ensemble probabilistic forecasts of COVID-19 mortality in the United     #
#     States. Proceedings of the National Academy of Sciences. 2022;119(15):e21#
#     13561119. https://doi.org/10.1073/pnas.2113561119.                       #
#                                                                              #
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

modelFitMetrics <- function(crude.data.input, calibration.input,
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
data.input.MF <- crude.data.input

######################################
# Saving the calibration period size #
######################################
calibration.input.MF <- as.numeric(calibration.input)

#############
# Date type #
#############
date.Type.input.MF <- date.Type.input

##############################
# List of quantile forecasts #
##############################
quantile.forecast.input.MF <- quantile.list.input 

#######################################
# Empty list to add model fit metrics #
#######################################
Model_Fits <- data.frame(Location = NA, 
                         Model = NA, 
                         Date = NA, 
                         meanMSE = NA, 
                         meanMAE = NA, 
                         mean95PI = NA, 
                         meanWIS = NA)

########################
# Final list to export #
########################
finalList <- list()

##############################
# Indicator for ARIMA models #
##############################
ARIMAIndicator <- 0 

#------------------------------------------------------------------------------#
# Looping through the quantile forecast ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each quantile forecast, determines if the  #
# model fit statistics can be determined (i.e., it can not for ARIMA models),  #
# and subsets the observed data for later calculations.                        #
#------------------------------------------------------------------------------#
for(q in 1:length(quantile.forecast.input.MF)){
  
  #############################
  # Indexed quantile forecast #
  #############################
  indexedQuantile <- as.data.frame(quantile.forecast.input.MF[[q]])
  
  # Name of the indexed quantile
  indexedQuantileName <- names(quantile.forecast.input.MF[q])
  
  #######################################################
  # Pulling information from the quantile forecast name #
  #######################################################
  
  # Model name
  modelName <- strsplit(indexedQuantileName, "[-]")[[1]][1]
  
  # Location name
  location <- strsplit(indexedQuantileName, "[-]")[[1]][2]
  
  # Forecast period
  
  # Forecast period for weekly or daily data 
  if(date.Type.input.MF %in% c("week", "day")){
    
    # Determining the forecast period from the name
    forecastPeriod <- substring(indexedQuantileName, regexpr("-", indexedQuantileName) + (nchar(location) + 2))
    
    # Forecast period for yearly or time index data 
  }else{
    
    # Determining the forecast period from the name
    forecastPeriod <- strsplit(indexedQuantileName, "[-]")[[1]][3]
    
  }
  
  ##################################################
  # Determining if the model fit can be determined #
  ##################################################
  if(modelName == "ARIMA"){
    
    # Switches to one if there is an ARIMA model in the list
    ARIMAIndicator <- 1
    
    next

  }
  
  
  ############################################
  # Determining the calibration period dates #
  ############################################
  calibrationDates <- switch(date.Type.input.MF,
                             "week" = c(seq.Date(anydate(forecastPeriod) - ((7*calibration.input.MF) - 7), anydate(forecastPeriod), by = "1 week")),
                             "day" = c(seq.Date(anydate(forecastPeriod) - (calibration.input.MF - 1), anydate(forecastPeriod), by = "1 day")),
                             c(seq(as.numeric(forecastPeriod) - (calibration.input.MF - 1), as.numeric(forecastPeriod), by = 1))
  )
  
  ###############################
  # Preparing the observed data #
  ###############################
  if(date.Type.input.MF %in% c("day", "week")){
    
  observedData <- data.input.MF %>%
    dplyr::filter(anytime::anydate(data.input.MF[,1]) %in% c(calibrationDates)) %>% # Filtering dates
    dplyr::select(location) # Selecting the right group 
  
  }else{
    
    observedData <- data.input.MF %>%
      dplyr::filter(as.numeric(data.input.MF[,1]) %in% c(calibrationDates)) %>% # Filtering dates
      dplyr::select(location) # Selecting the right group 
  }
  
  ########################################
  # Preparing the quantile forecast data #
  ########################################
  if(modelName == "Prophet"){
    
    quantileForecastCleaned <- indexedQuantile[1:calibration.input.MF,] %>%
      dplyr::select(prediction, `lower.95%`, `upper.95%`) %>% # Selecting needed columns
      dplyr::mutate(observed = observedData[,1]) %>% # Adding observed data 
      dplyr::rename("means" = prediction)
    
  }else{
    
    quantileForecastCleaned <- indexedQuantile[1:calibration.input.MF,] %>%
      dplyr::select(means, `lower.95%`, `upper.95%`) %>% # Selecting needed columns
      dplyr::mutate(observed = observedData[,1]) # Adding observed data 
    
  }

  
  # Vector of observed data
  true_values <- c(observedData[,1])
  
#------------------------------------------------------------------------------#
# Calculating 95% PI, MSE, and MAE for model fit -------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the 95% PI, MSE, and MAE for the model fit.   #
#------------------------------------------------------------------------------#
  
  PI_MSE_MAE <- quantileForecastCleaned %>%
    dplyr::mutate(inCoverage = ifelse(observed <= `upper.95%` & observed >= `lower.95%`, 1, 0), # Determining if in coverage
                  PercentCoverage = (sum(inCoverage)/calibration.input.MF)*100, # Calculating the percent coverage
                  mean95PI = mean(PercentCoverage)) %>% # Avg 95% PI
    dplyr::select(observed, means, mean95PI) %>% # Selected needed variables 
    dplyr::mutate(MSE = ((observed-means)^2), # Calculating MSE
                  MAE = (abs(observed - means)), # Calculating MAE
                  meanMAE = mean(MAE), # Avg MAE
                  meanMSE = mean(MSE)) %>% # Avg MSE
    dplyr::select(meanMSE, meanMAE, mean95PI) %>% # Selecting the needed variables 
    dplyr::distinct(.keep_all = T) # Keeping only unique rows 
  
#------------------------------------------------------------------------------#
# Calculating Weighted Interval Scores for model fit ---------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the average WIS across the model fit.         #
#------------------------------------------------------------------------------#
  
  ##################################################
  # Preparing for the loop going through quantiles #
  ##################################################
  
  # Indexed quantile forecast
  data.WIS <- indexedQuantile[c(1:calibration.input.MF), ]
  
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
  
  ############################################
  # Looping through calibration period dates #
  ############################################
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
  Model_Fits <- rbind(Model_Fits, allMetrics)

  } # End of calibration loop

  ##################################
  # Renaming the model fit columns #
  ##################################
  Model_Fits <- Model_Fits %>%
    dplyr::rename("MSE" = meanMSE,
                  "MAE" = meanMAE,
                  "95%PI" = mean95PI,
                  "WIS" = meanWIS) %>%
    dplyr::mutate(MSE = round(MSE, 2),
                  MAE = round(MAE, 2),
                  `95%PI` = round(`95%PI`, 2),
                  WIS = round(WIS, 2))


#------------------------------------------------------------------------------#
# Adding the ARIMA indicator to the list, and exporting ------------------------
#------------------------------------------------------------------------------#
# About: This section adds the ARIMA indicator to the list, and exports to the #
# main shiny app.                                                              #
#------------------------------------------------------------------------------#

# Removing NA in Model_Fits data
Model_Fits <- Model_Fits[-1,]

# Adding it to the list
finalList[[1]] <- Model_Fits

finalList[[2]] <- ARIMAIndicator


# Returning the list
return(finalList)

}