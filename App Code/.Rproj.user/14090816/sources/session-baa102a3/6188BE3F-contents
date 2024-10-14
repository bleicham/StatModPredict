#------------------------------------------------------------------------------#
#                                                                              #
#          Auto-regressive Integrate Moving Average Model Forecasting          #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file takes in output related to the user selected forecasting periods,  #
# desired forecasting horizon, type of date data, data smoothing, and 'auto.   #
# ARIMA' related parameters to produce a list of ARIMA based quantile          #
# forecasts. The forecasts are outputted in quantile format, with a median     #
# prediction as the model used assumes normality, and 22 additional quantiles. #
# The user also has the option to customize the ARIMA modeling parameters.     #
#                                                                              #
# (1) Hyndman R. auto.arima: Fit best ARIMA model to univariate time series.   #
#     Version 8.21.1. [cited 2023 Oct 16]. Available from: https://www.rdocu   #
#     mentation.org/packages/forecast/versions/8.21.1/topics/auto.arima        #
#                                                                              #
# Examples of the applications of the function in forecasting efforts can be   #
# found in the following:                                                      #
#                                                                              #
# (1) Chowell G, Dahal S, Tariq A, Roosa K, Hyman JM, Luo R. An ensemble n-sub #
#     -epidemic modeling framework for short-term forecasting epidemic traject #
#     ories: Application to the COVID-19 pandemic in the USA. PLoS Comput Biol.# 
#     2022;18: e1010602. doi:10.1371/journal.pcbi.1010602.                     #
#                                                                              #
#------------------------------------------------------------------------------#
#                Authors: Amanda Bleichrodt and Ruiyan Luo                     #
#------------------------------------------------------------------------------#

ARIMA <- function(calibration.input, horizon.input, 
                  smoother.input, parameter.input, seasonality.input){
 
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Below takes in the inputs from the function, and saves them under new #        
# variable names to prevent over-writing of values.                            #
#------------------------------------------------------------------------------#
  
  ######################################
  # Reading in the calibration periods #
  ######################################
  calibration.periods.A <- calibration.input

  ##################################
  # Saving the forecasting horizon #
  ##################################
  horizon.input.A <- horizon.input

  #####################
  # Smoothing of data #
  #####################
  smoothing.input.A <- smoother.input

  #######################
  # Parameter Selection #
  #######################
  parameter.selection.arima.input.A <- parameter.input
  
  ###############
  # Seasonality #
  ###############
  seasonality.input.A <- seasonality.input
  
  ########################################
  # Creating an empty list for quantiles #
  ########################################
  quantile.list <- list()
  
  #######################################################
  # Creating an empty list for quantiles with locations #
  #######################################################
  quantile.list.locations <- list()
  
  ##############################################
  # Creating an empty list for the output data #
  ##############################################
  final.list <- list()
  
  ################################################################
  # Creating an empty table to fill with the best fitting models #
  ################################################################
  best.fit.models <- data.frame("Location" = NA, 
                                "Forecast Date" = NA,
                                "Calibration Period Length" = NA, 
                                "Model-Specification (Non-Seasonal)" = NA,
                                "Model-Specification (Seasonal)" = NA,
                                "Intercept" = NA, 
                                "Q*" = NA,
                                "df" = NA,
                                "p-value" = NA,
                                "AIC" = NA,
                                "AICc" = NA,
                                "BIC" = NA)
  
  #######################################################
  # Creating the parameter for checking for seasonality #
  #######################################################
  if(seasonality.input.A == 1){seasonCheck <- F}else{seasonCheck <- T}
  
  
#------------------------------------------------------------------------------#
# Master Calibration loop ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the loop that goes through each of the           #
# data sets containing the calibration periods indicated by the user. Once the #
# loop is complete, the user will have a few large lists containing model fit  #
# information and related forecasts. Each element will be used throughout the  #
# remainder of the toolbox quite frequently.                                   #
#------------------------------------------------------------------------------#

  ################################################
  # Looping through the calibration periods list #
  ################################################
  for(c in 1:length(calibration.periods.A)){
    
#------------------------------------------------------------------------------#
# Setting up the ARIMA loop  ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This sections uses the inputs from above to prepare to go through the #
# calibration period list for quantile and best-fit model outputs.             #
#------------------------------------------------------------------------------#
    
    ##############################
    # Indexed calibration period #
    ##############################
    index.calibration.period <- calibration.periods.A[[c]]
    
    ###########################################################
    # Pulling information from the indexed calibration period # 
    ###########################################################
    
    # Location names
    location.names <- names(index.calibration.period)[-1]
    
    # Determining the calibration period length
    calibrationPeriodLength <- nrow(index.calibration.period)
    
    # Determining the forecast period date - Daily or Weekly data
    if(all(str_length(index.calibration.period[,1]) > 4)){
   
       forecast.period.date <- max(anytime::anydate(index.calibration.period[,1]))
     
    # Determining the forecast period date - Yearly or Time Index
    }else{
      
      forecast.period.date <- max(as.numeric(index.calibration.period[,1]))
      
    }
    
    ##################################
    # Removing the time-index column #
    ##################################
    timeseries.no.date <- index.calibration.period %>%
      dplyr::select(all_of(location.names)) # Selecting only locations 
    
#------------------------------------------------------------------------------#
# Looping through the locations ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through all of the locations selected by the user. #
# For each combination of location and calibration period length, it then      #
# returns information related to the model fit and the assoicated forecast.    #
#------------------------------------------------------------------------------#
    
    #############################
    # Looping through locations #
    #############################
    for(i in 1:ncol(timeseries.no.date)){
      
#------------------------------------------------------------------------------#
# Setting up the information needed for each iteration of the location loop ----
#------------------------------------------------------------------------------#
# About: This section sets up the information needed to run the ARIMA model.   #
#------------------------------------------------------------------------------#
      
      ###########################################################
      # Selecting the data associated with the indexed location #
      ###########################################################
      data <- timeseries.no.date[i]
      
      ####################
      # Location indexed #
      ####################
      location.index <- location.names[i]
         
      #####################################################################
      # Creating a matrix containing all zeros to fill in with parameters #
      #####################################################################
      
      # the number of rows = number of columns in the data, and 7 columns
      orders <- matrix(0,ncol(timeseries.no.date), 7)

      # Setting the row names for the orders 
      rownames(orders) <- location.names

      #######################################################
      # Creating a vector of zeros to fill in with p-values #
      #######################################################
      pvalues <- rep(0,1)

      ###########################################################################
      # Creating an matrix of zeros, that has the number of rows = the forecast #
      # period length (e.g., 4 for # 4 weeks) and the number of columns =       #
      # the number of columns in the data -- Will be filled in with the         #
      # forecasted means.                                                       #
      ########################################################################### 
      forecast.mean <- matrix(0, horizon.input.A, 1)

      ###########################################################################
      # Creating an matrix of zeros, that has the number of rows = the forecast #
      # period length (e.g., 4 for # 4 weeks) and the number of columns = the   #
      # number of columns in the data -- Will be filled in with the upper       #
      # bounds of the forecasts means                                           #
      ###########################################################################
      forecast.upper <- forecast.mean

      ###########################################################################
      # Creating an matrix of zeros, that has the number of rows = the forecast #
      # period length (e.g., 4 for # 4 weeks) and the number of columns = the   #
      # number of columns in the data -- Will be filled in with  # the lower    #
      # bounds of the forecasts means                                           #
      ###########################################################################
      forecast.lower <- forecast.mean

      # Creating a vector of "p-values" to run through the model
      levels <- 100-c(0.02, 0.05, seq(0.1, 0.9, by=0.1))*100

      # Creating an empty list to fill in with forecasts
      coefs <- list()
      
#------------------------------------------------------------------------------#
# Smoothing --------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section implements smoothing if indicated by the user.  #
# Additionally, it runs a check to see if smoothing can be implemented         #
# (i.e., only with daily data). If smoothing is miss-entered, the function will#
# return a warning, and not implement the smoothing indicated by the user.     #
#------------------------------------------------------------------------------#
      
      ####################################################
      # Applying (or not applying) smoothing to the data #
      ####################################################
      if(smoothing.input.A == 1 || is.null(smoothing.input.A)){
        
        # Data to be used for the remainder of the code
        data <- data
        
      #################################
      # Run if smoothing is indicated #
      #################################
      }else{
        
        # Data w/smoothing employed
        data <- rollmean(data, smoothing.input.A)
        
      }
      
#------------------------------------------------------------------------------#
# Fitting the ARIMA Model ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section fits the ARIMA model to the time series data, using the  #
# the 'auto.arima' function. The residuals from the fit are also outputted,    #
# along with information related to the model fit: AIC, BIC, AICc, etc.        #
#------------------------------------------------------------------------------#
      
      ############################################################
      # Transforming the sdata vector to a R-time series object. #
      ############################################################
      sdata.ts <- ts(data, start=1, frequency=as.numeric(seasonality.input.A))
      
      # Transforming the sdata vector to a R-time series object. The start = 1,
      # indicates that the # time series starts at the first value of the
      # vector read into the ts() function ('sdata') and the # frequency = 1
      # indicates that there is only one "observation" per unit of time
      
      ###########################
      # Fitting the ARIMA model #
      ###########################
      fit <- forecast::auto.arima(sdata.ts,
                                  start.p = parameter.selection.arima.input.A[1], # Non-seasonal
                                  max.p = parameter.selection.arima.input.A[2], # Non-seasonal
                                  start.q = parameter.selection.arima.input.A[3],# Non-seasonal
                                  max.q = parameter.selection.arima.input.A[4], # Non-seasonal
                                  start.P = parameter.selection.arima.input.A[6], # Seasonal 
                                  max.P = parameter.selection.arima.input.A[7], # Seasonal
                                  start.Q = parameter.selection.arima.input.A[8], # Seasonal
                                  max.Q = parameter.selection.arima.input.A[9], # Seasonal
                                  max.d = parameter.selection.arima.input.A[5], # Non-seasonal
                                  max.D = parameter.selection.arima.input.A[10], # Seasonal
                                  trace = F, # Showing all possible ARIMA models
                                  seasonal = seasonCheck) # Considering seasonality
      
      ###########################################################################################
      # Returning an error if the calibration fit is to small or if the model could not be fit. #
      ###########################################################################################
      if(AIC(fit) == "-Inf"){
        
        # Filling in the exported list with an error 
        quantile.list.locations[[i]] <- NA
        
        # Adding names to list data frames
        names(quantile.list.locations)[i] <- paste0("ARIMA-", location.index, "-", forecast.period.date, "-Calibration-", calibrationPeriodLength)
        
        # Going to next loop iteration 
        next
        
        }
      
#------------------------------------------------------------------------------#
# ARIMA model fit and best ft statistics ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a list of best fit models and Ljung-Box test     #
# statistics. In addition, it exports statistics related to the model fit such #
# as the AIC, BIC, and AICc. The results are then outputted to the model fit   #
# section of the dashboard.                                                    #
#------------------------------------------------------------------------------#
      
      #################################
      # Saving the orders in a matrix #
      #################################
      orders[i,] <- fit$arma #p,q,P,Q,s,d,D
      
      ##############################
      # Running the Ljung-Box test #
      ##############################
      pvalues <- forecast::checkresiduals(fit)
      
      ##########################
      # Re-formatting the date #
      ##########################
      
      # Determining the forecast period date - Daily or Weekly data
      if(all(str_length(index.calibration.period[,1]) > 4)){
        
        forecast.period.date <- max(anytime::anydate(index.calibration.period[,1]))
        
        # Determining the forecast period date - Yearly or Time Index
      }else{
        
        forecast.period.date <- max(as.numeric(index.calibration.period[,1]))
        
      }
      
      ##########################################################################
      # Creating a table that contains the best fit parameters, results of     #
      # the Ljung-Box test, AIC, BIC, and AICc.                                #
      ##########################################################################
      
      # If considering seasonal patterns
      if(seasonCheck){
        
        newRowFit <- data.frame("Location" = location.index, 
                                "Forecast Date" = as.character(forecast.period.date),
                                "Calibration Period Length" = calibrationPeriodLength, 
                                "Model-Specification (Non-Seasonal)" = paste0("ARIMA(", orders[i,1], ",", orders[i,6], ",", orders[i,2],")"),
                                "Model-Specification (Seasonal)" = paste0("SARIMA(", orders[i,3], ",", orders[i,7], ",", orders[i,6],")"),
                                "Intercept" = orders[i,5], 
                                "Q*" = pvalues$statistic,
                                "df" =  pvalues$parameter,
                                "p-value" = pvalues$p.value,
                                "AIC" = fit[["aic"]],
                                "AICc" = fit[["aicc"]],
                                "BIC" = fit[["bic"]])
        
      # If working with non-Seasonal data 
      }else{
        
        newRowFit <- data.frame("Location" = location.index, 
                                "Forecast Date" = as.character(forecast.period.date),
                                "Calibration Period Length" = calibrationPeriodLength, 
                                "Model-Specification (Non-Seasonal)" = paste0("ARIMA(", orders[i,1], ",", orders[i,6], ",", orders[i,2],")"),
                                "Model-Specification (Seasonal)" = NA,
                                "Intercept" = orders[i,5], 
                                "Q*" = pvalues$statistic,
                                "df" =  pvalues$parameter,
                                "p-value" = pvalues$p.value,
                                "AIC" = fit[["aic"]],
                                "AICc" = fit[["aicc"]],
                                "BIC" = fit[["bic"]])

      }
      
      # Merging with the old data
      best.fit.models <- rbind(best.fit.models, newRowFit)
      
#------------------------------------------------------------------------------#
# ARIMA Forecasting ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the model fit from above, and forecasts out the    #
# indexed horizon amount. Any produced negative quantiles are truncated at     #
# zero, and produced quantiles are combined into a single data frame.          #
#------------------------------------------------------------------------------#
      
      ################################################################
      # Producing forecasts and PIs using the ARIMA model from above #
      ################################################################
      
      # Producing the forecasts
      fcst <- forecast::forecast(fit, h=horizon.input.A, level=levels)
      
      # Creating a variable that includes the forecasted values/means
      means <- round(fcst$mean, 2)
      
      # Changing all neg. forecasted values to 0
      means[means<0] <- 0
      
      # Creating a variable that includes the lower PI values
      lower <- round(fcst$lower, 2)
      
      # Changing all neg. lower CI values to 0
      lower[lower<0] <- 0
      
      # Creating a variable that contains the upper PI values
      upper <- round(fcst$upper, 2)
      
      # Changing all neg. upper PI values to 0
      upper[upper<0] <- 0
      
      # Combining the means, and PIs into one data frame
      fcstval <- cbind(means, lower, upper)
      
#------------------------------------------------------------------------------#
# Creating a list of quantile frames -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a list of quantile forecasts to be later added   #
# to a list with the best-fit models. Each forecast is labeled with its        #
# location and forecast period and calibration period length                   #
#------------------------------------------------------------------------------#
      
      ############################################
      # Adding the forecast to the quantile list #
      ############################################
      quantile.list.locations[[i]] <- fcstval
      
      # Adding names to list data frames
      names(quantile.list.locations)[i] <- paste0("ARIMA-", location.index, "-", forecast.period.date, "-Calibration-", calibrationPeriodLength)
      
    } # End of location loop 
    
    #########################################
    # Adding the forecasts to the main list #
    #########################################
    quantile.list <- c(quantile.list, quantile.list.locations)
    
  } # End of calibration loop

#------------------------------------------------------------------------------#
# Combining lists to return ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section combines the quantile forecasts and best fit model info  #
# to be returned to the main shiny app.                                        #
#------------------------------------------------------------------------------#
  
  ###################
  # Making the list #
  ###################
  final.list <- list(quantile.list, best.fit.models)
  
  # Adding names
  names(final.list) <- c("Forecasts", "ModelFit")
  
  ######################
  # Returning the list #
  ######################
  return(final.list)
  
} # End of function

    
  