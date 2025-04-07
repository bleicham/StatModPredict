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
# forecasting period. Additionally, the model outputs the AIC, BIC, and AICc   #
# along with multiple model fit statistics for the ARIMA model.                #
#                                                                              #
#                                                                              #
# A detailed description of the calculations used for MSE, MAE, 95% PI         #
# coverage, and WIS can be found at:                                           #
#                                                                              #
# (1) Gneiting T, Raftery AE. Strictly proper scoring rules, prediction, and   #
#     estimation. Journal of the American statistical Association. 2007;102    #
#     (477):359-378. https://doi.org/10.1198/016214506000001437.               #
# (2) Kuhn M, Johnson K. Applied predictive modeling. Vol 26. Springer; 2013.  #
# (3) University of Nicosia. M4Competition Competitor’s Guide: Prizes and      # 
#     Rules. 2018. Accessed June 28, 2023. http://www.unic.ac.cy/errorTypeGLM /wp-      #
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
#              Authors: Amanda Bleichrodt and Gerardo Chowell                  #
#------------------------------------------------------------------------------#

modelFitMetrics <- function(crude.data.input, date.Type.input,
                            quantile.list.input, ARIMAFit,
                            GAMFit, GLMFit, selectedQuantile){


#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Below takes in the inputs into the function, and saves them under new #        
# variable names to prevent over-writing of values.                            #
#------------------------------------------------------------------------------#

  ###################################################
  # Reading in the crude data (original or updated) #
  ###################################################
  data.input.MF <- crude.data.input
  
  #############
  # Date type #
  #############
  date.Type.input.MF <- date.Type.input
  
  ##############################
  # List of quantile forecasts #
  ##############################
  quantile.forecast.input.MF <- quantile.list.input 
  
  ########################
  # ARIMA fit statistics #
  ########################
  ARIMAFitInput <- ARIMAFit
  
  ######################
  # GAM fit statistics #
  ######################
  GAMFitInput <- GAMFit
  
  ######################
  # GLM fit statistics #
  ######################
  GLMFitInput <- GLMFit
  
  #####################
  # Selected quantile #
  #####################
  quantileCalculation <- selectedQuantile
  

#------------------------------------------------------------------------------#
# Preparing the needed data frames and lists -----------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the needed data frames and list for the loop    #
# below.                                                                       #
#------------------------------------------------------------------------------#

  #######################################
  # Empty list to add model fit metrics #
  #######################################
  Model_Fits <- data.frame(Location = NA,
                           Model = NA,
                           Date = NA,
                           Calibration = NA,
                           MSE = NA,
                           MAE = NA,
                           PI = NA,
                           WIS = NA,
                           AICc = NA,
                           AIC = NA,
                           BIC = NA,
                           ModelSpec.Non.Seasonal = NA,
                           ModelSpec.Seasonal = NA,
                           Intercept = NA,
                           Q = NA,
                           df = NA,
                           PValue = NA)

  ########################
  # Final list to export #
  ########################
  finalList <- list()


#------------------------------------------------------------------------------#
# Looping through the quantile forecast ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each quantile forecast, determines if the  #
# model fit statistics can be determined and subsets the observed data for     #
# later calculations.                                                          #
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
    
    # Forecast period and calibration period length for weekly or daily data 
    if(date.Type.input.MF %in% c("week", "day")){
      
      # Determining the forecast period from the name
      forecastPeriod <- paste0(strsplit(indexedQuantileName, "[-]")[[1]][3], "-", strsplit(indexedQuantileName, "[-]")[[1]][4], "-", strsplit(indexedQuantileName, "[-]")[[1]][5])
      
      # Calibration period length 
      calibrationLength <- as.numeric(strsplit(indexedQuantileName, "[-]")[[1]][7])
      
    # Forecast period and calibration period length for yearly or time index data 
    }else{
      
      # Determining the forecast period from the name
      forecastPeriod <- strsplit(indexedQuantileName, "[-]")[[1]][3]
      
      # Calibration period length 
      calibrationLength <- as.numeric(strsplit(indexedQuantileName, "[-]")[[1]][5])
      
    }
    
    ############################################
    # Determining the calibration period dates #
    ############################################
    calibrationDates <- switch(date.Type.input.MF,
                               "week" = c(seq.Date(anydate(forecastPeriod) - ((7*calibrationLength) - 7), anydate(forecastPeriod), by = "1 week")),
                               "day" = c(seq.Date(anydate(forecastPeriod) - (calibrationLength - 1), anydate(forecastPeriod), by = "1 day")),
                               c(seq(as.numeric(forecastPeriod) - (calibrationLength - 1), as.numeric(forecastPeriod), by = 1))
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
      
      quantileForecastCleaned <- indexedQuantile[1:calibrationLength,] %>%
        dplyr::select(prediction, paste0("lower.", quantileCalculation, "%"), paste0("upper.", quantileCalculation, "%")) %>% # Selecting needed columns
        dplyr::mutate(observed = observedData[,1]) %>% # Adding observed data 
        dplyr::rename("means" = prediction)
      
    }else{
      
      quantileForecastCleaned <- indexedQuantile[1:calibrationLength,] %>%
        dplyr::select(means, paste0("lower.", quantileCalculation, "%"), paste0("upper.", quantileCalculation, "%")) %>% # Selecting needed columns
        dplyr::mutate(observed = observedData[,1]) # Adding observed data 
      
    }
    
    ###########################
    # Vector of observed data #
    ###########################
    true_values <- c(observedData[,1])
    
#------------------------------------------------------------------------------#
# Handling the ARIMA forecasts -------------------------------------------------
#------------------------------------------------------------------------------#
# About: As the MSE, MAE, WIS, and prediction interval coverage can not be     #
# calculated for the ARIMA model the loop is skipped to the next iteration if  #
# the indexed quantile is from the ARIMA model.                                #
#------------------------------------------------------------------------------#
    
    ##########################################
    # Checking if the ARIMA model is indexed #
    ##########################################
    if(modelName == "ARIMA"){
      
      # Skipping to the next loop iteration
      next
      
    }
    
#------------------------------------------------------------------------------#
# Calculating the prediction interval coverage, MSE, and MAE -------------------
#------------------------------------------------------------------------------#
# About: This section calculates the selected prediction interval coverage,    #
# mean squared error, and mean absolute error for the indexed quantile fit.    #
# The comparison data is either the original data used to produce the          #
# forecasts or the data uploaded by the user.                                  #
#------------------------------------------------------------------------------#
    
    PI_MSE_MAE <- quantileForecastCleaned %>%
      dplyr::mutate(inCoverage = ifelse(observed < get(paste0("upper.", quantileCalculation, "%")) & observed > get(paste0("lower.", quantileCalculation, "%")), 1, 0), # Determining if in coverage
                    mean95PI = (sum(inCoverage)/calibrationLength)*100) %>% # Calculating the percent coverage
      dplyr::select(observed, means, mean95PI) %>% # Selected needed variables 
      dplyr::mutate(MSE = ((means - observed)^2), # Calculating MSE
                    MAE = (abs(means - observed)), # Calculating MAE
                    meanMAE = mean(MAE), # Avg MAE
                    meanMSE = mean(MSE)) %>% # Avg MSE
      dplyr::select(meanMSE, meanMAE, mean95PI) %>% # Selecting the needed variables 
      dplyr::distinct(.keep_all = T) # Keeping only unique rows 
    
    
#------------------------------------------------------------------------------#
# Calculating Weighted Interval Scores for model fit ---------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the average WIS for the GLM, GAM, and Prophet #
# models for the model fit. It is then averaged across the entire calibration  #
# period.                                                                      #
#------------------------------------------------------------------------------#
    
    ##################################################
    # Preparing for the loop going through quantiles #
    ##################################################
    
    # Indexed quantile forecast
    data.WIS <- indexedQuantile[c(1:calibrationLength), ]
    
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
      WISF[j,1] <- (1/(K+(1/2)))*(w0*abs(y-m) + sum1)
      
    } # End of loop through observed values
    
#------------------------------------------------------------------------------#
# Creating the data frame with the calculated metrics --------------------------
#------------------------------------------------------------------------------#
# About: This section combines the above calculated metrics into one data set. #
# In a later step, this data set will be combined with the data frames with    #
# AICc, BIC, AIC, and other ARIMA fit metrics.                                 #
#------------------------------------------------------------------------------#
    
    #########################
    # Combining all metrics #
    #########################
    allMetrics <- PI_MSE_MAE %>%
      dplyr::mutate(meanWIS = mean(WISF[,1]),
                    Model = modelName,
                    Location = location,
                    Date = forecastPeriod,
                    Calibration = calibrationLength,
                    MSE = meanMSE, 
                    MAE = meanMAE,
                    PI = mean95PI,
                    WIS = meanWIS,
                    AICc = NA,
                    AIC = NA,
                    BIC = NA, 
                    ModelSpec.Non.Seasonal = NA,
                    ModelSpec.Seasonal = NA, 
                    Intercept = NA,
                    Q = NA,
                    df = NA,
                    PValue = NA) %>%
      dplyr::select(Location, Model, Date, Calibration, MSE, MAE, PI, 
                    WIS, AICc, AIC, BIC, ModelSpec.Non.Seasonal,
                    ModelSpec.Seasonal, Intercept, Q, df, PValue)
    
   
    ##############################################
    # Adding the metrics to the shell data frame #
    ##############################################
    Model_Fits <- rbind(Model_Fits, allMetrics)
    
    } # End of calibration loop

#------------------------------------------------------------------------------#
# Cleaning up the other metrics ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section cleans up and combines the fit statistics from the ARIMA #
# GLM, and GAM models. The resulting table will later be combined with the     #
# metrics calculated above and exported.                                       #
#------------------------------------------------------------------------------#
  
  ##############################
  # Creating empty data frames #
  ##############################
  
  # ARIMA
  ARIMACleaned <- NULL
  
  # GAM
  GAMCleaned <- NULL
  
  # GLM 
  GLMCleaned <- NULL
  
  ####################################
  # Cleaning up the ARIMA data frame #
  ####################################
  if(!is.null(ARIMAFitInput)){
    
  ARIMACleaned <- ARIMAFitInput %>%
    dplyr::mutate(Date = Forecast.Date, 
                  Calibration = Calibration.Period.Length,
                  Model = "ARIMA",
                  ModelSpec.Non.Seasonal = Model.Specification..Non.Seasonal.,
                  ModelSpec.Seasonal = Model.Specification..Seasonal., 
                  PValue = p.value,
                  Q = Q., 
                  MSE = NA,
                  MAE = NA,
                  PI = NA,
                  WIS = NA) %>%
    dplyr::select(Location, Model, Date, Calibration, MSE, MAE, PI, 
                  WIS, AICc, AIC, BIC, ModelSpec.Non.Seasonal,
                  ModelSpec.Seasonal, Intercept, Q, df, PValue)
  
  # Removing row names
  rownames(ARIMACleaned) <- NULL
  
  }
  
  ##################################
  # Cleaning up the GAM data frame #
  ##################################
  if(!is.null(GAMFitInput)){
    
  GAMCleaned <- GAMFitInput %>%
    dplyr::mutate(Date = Forecast.Date,
                  Calibration = Calibration.Period.Length,
                  Model = "GAM",
                  ModelSpec.Non.Seasonal = NA,
                  ModelSpec.Seasonal = NA, 
                  Intercept = NA, 
                  PValue = NA,
                  df = NA, 
                  Q = NA, 
                  MSE = NA,
                  MAE = NA,
                  PI = NA,
                  WIS = NA) %>%
    dplyr::select(Location, Model, Date, Calibration, MSE, MAE, PI, 
                  WIS, AICc, AIC, BIC, ModelSpec.Non.Seasonal,
                  ModelSpec.Seasonal, Intercept, Q, df, PValue)
  
  }
  
  ##################################
  # Cleaning up the GLM data frame #
  ##################################
  if(!is.null(GLMFitInput)){
    
  GLMCleaned <- GLMFitInput %>%
    dplyr::mutate(Date = Forecast.Date,
                  Calibration = Calibration.Period.Length,
                  Model = "GLM",
                  ModelSpec.Non.Seasonal = NA,
                  ModelSpec.Seasonal = NA, 
                  Intercept = NA, 
                  PValue = NA,
                  df = NA, 
                  Q = NA, 
                  MSE = NA,
                  MAE = NA,
                  PI = NA,
                  WIS = NA) %>%
    dplyr::select(Location, Model, Date, Calibration, MSE, MAE, PI, 
                  WIS, AICc, AIC, BIC, ModelSpec.Non.Seasonal,
                  ModelSpec.Seasonal, Intercept, Q, df, PValue)
  
  }
  
  ###################################
  # Combining the three data frames #
  ###################################
  combinedStatMetrics <- rbind(ARIMACleaned, GAMCleaned, GLMCleaned)
  
#------------------------------------------------------------------------------#
# Preparing the final data frame -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the final data frame that will be exported to    #
# the main data frame. It combines all of the metrics into one data set, and   #
# rounds the values when needed. Additionally, it removes any columns that are #
# not needed.                                                                  #
#------------------------------------------------------------------------------#
  
  ################################
  # Combining all of the metrics #
  ################################
  if(!is.null(combinedStatMetrics)){
    
    # Combining the final data frames 
    finalData <- merge(Model_Fits, combinedStatMetrics, by = c("Location", "Model", "Date", "Calibration"), all = TRUE)
    
    # Cleaning up the data 
    finalDataCleaned <- finalData %>%
      dplyr::select(Location, Model, Date, Calibration, MSE.x, MAE.x, PI.x, WIS.x,
                    AICc.y, AIC.y, BIC.y, ModelSpec.Non.Seasonal.y, ModelSpec.Seasonal.y, 
                    Intercept.y, Q.y, df.y, PValue.y) %>%
      dplyr::rename("MSE" = MSE.x,
                    "MAE" = MAE.x,
                    "PI" = PI.x, 
                    "WIS" = WIS.x,
                    "AICc" = AICc.y,
                    "AIC" = AIC.y,
                    "BIC" = BIC.y,
                    "Non-Seasonal-Specification" = ModelSpec.Non.Seasonal.y,
                    "Seasonal-Specification" = ModelSpec.Seasonal.y,
                    "Intercept" = Intercept.y,
                    "Q" = Q.y,
                    "df" = df.y,
                    "p-Value" = PValue.y) %>%
      dplyr::mutate(MSE = round(MSE, 2),
                    MAE = round(MAE, 2),
                    PI = round(PI, 2),
                    WIS = round(WIS, 2),
                    AICc = round(AICc, 2),
                    AIC = round(AIC, 2),
                    BIC = round(BIC, 2),
                    Q = round(Q, 2),
                    `p-Value` = round(`p-Value`, 2)) %>%
      dplyr::select(where(~ !all(is.na(.)))) %>% # Removing columns that are fully NAs
      dplyr::filter(rowSums(is.na(.)) != ncol(.)) # Removing rows that are fully NAs
    
  }else{
    
    # Final data frame 
    finalData <- Model_Fits
    
    # Cleaning up the data 
    finalDataCleaned <- finalData %>%
      dplyr::rename("Non-Seasonal-Specification" = ModelSpec.Non.Seasonal,
                    "Seasonal-Specification" = ModelSpec.Seasonal, 
                      "p-Value" = PValue) %>%
      dplyr::mutate(MSE = round(MSE, 2),
                    MAE = round(MAE, 2),
                    PI = round(PI, 2),
                    WIS = round(WIS, 2),
                    AICc = round(AICc, 2),
                    AIC = round(AIC, 2),
                    BIC = round(BIC, 2),
                    Q = round(Q, 2),
                    `p-Value` = round(`p-Value`, 2)) %>%
      dplyr::select(where(~ !all(is.na(.)))) %>% # Removing columns that are fully NAs
      dplyr::filter(rowSums(is.na(.)) != ncol(.)) # Removing rows that are fully NAs
    
  }
  
  
  
#------------------------------------------------------------------------------#
# Exporting the data frame with model fit metrics ------------------------------
#------------------------------------------------------------------------------#
# About: This section exports the model fit metrics to the main dashboard.     #
#------------------------------------------------------------------------------#

  ############################
  # Returning the data frame #
  ############################
  return(finalDataCleaned)

}