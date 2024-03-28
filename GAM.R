#------------------------------------------------------------------------------#
#                                                                              #
#                Generalized Additive Model (GAM) Forecasting                  #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file takes in output related to the user selected forecasting periods,  #
# desired forecasting horizon, type of date data, data smoothing, and 'GAM'    #
# related parameters to produce a list of GAM based quantile forecasts. The    #
# forecasts are outputted in quantile format, with a median prediction and 22  #
# additional quantiles. The user also has the option to customize the GLM      #
# error distribution, and other GAM related model fitting options. Available   #
# error distributions include normal, Poisson, and negative binomial.          #
# The 'gam' function within the "mgcv" package is used to fit the model, and   #
# additional details on the function can be found at:                          #
#                                                                              #
# (1) Wood S. gam: Generalized additive models with integrated smoothness      #
#     estimation. Version 1.9-0. [cited 2023 Oct 16]. Available from: https:// #
#     www.rdocumentation.org/packages/mgcv/versions/1.9-0/topics/gam.          #
#                                                                              #
# Forecasts are outputted in quantile format, with a a median prediction, as   #
# the model assumes normality, and 22 additional quantiles.                    #
#------------------------------------------------------------------------------#
#                Authors: Amanda Bleichrodt and Ruiyan Luo                     #
#------------------------------------------------------------------------------#

GAM <- function(calibration.input, horizon.input, date.Type.input, 
                smoothing.input, error.input, k.input, smoothingTerm.input){
  
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Below takes in the inputs from the function, and saves them under new #        
# variable names to prevent over-writing of values.                            #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Reading in the list of calibration periods #
  ##############################################
  calibration.list.GAM <- calibration.input
  
  ##################################
  # Saving the forecasting horizon #
  ##################################
  horizon.input.G <- horizon.input
  
  #############
  # Date type #
  #############
  date.Type.input.G <- date.Type.input
  
  #####################
  # Smoothing of data #
  #####################
  smoothing.input.G <- smoothing.input
  
  ###############
  # Error input #
  ###############
  error.input.G <- error.input
  
  #############################
  # Number of basis functions #
  #############################
  k.input.G <- as.numeric(k.input)
  
  ##################
  # Smoothing term #
  ##################
  smoothing.term.G <- smoothingTerm.input
  
  ########################################
  # Creating an empty list for quantiles #
  ########################################
  quantile.list <- list()
  
  #######################################################
  # Creating an empty list for quantiles with locations #
  #######################################################
  quantile.list.locations <- list()
  
  ###########################################################
  # Creating an empty list for the residuals with locations #
  ###########################################################
  residuals.list.locations <- list()
  
  ############################################
  # Creating an empty list for the residuals #
  ############################################
  residuals.list <- list()
  
#------------------------------------------------------------------------------#
# Setting up the GAM loop  -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets up the information needed for GAM forecasting,      #
# including preparing the data and setting up the parameters needed for GAM    #
# forecasting. It also sets up the loops going through calibration periods.    #
#------------------------------------------------------------------------------#
  
  ########################################
  # Starting the calibration period loop #
  ########################################
  for(c in 1:length(calibration.list.GAM)){
    
    ##############################
    # Indexed calibration period #
    ##############################
    index.calibration.period <- calibration.list.GAM[[c]]
    
    ###########################################################
    # Pulling information from the indexed calibration period # 
    ###########################################################
    
    # Location names
    location.names <- names(index.calibration.period)[-1]
    
    
    # Determining the forecast period - Daily or Weekly
    if(all(str_length(index.calibration.period[,1]) > 4)){
      
      forecast.period.date <- max(anytime::anydate(index.calibration.period[,1]))
      
    }else{
      
      forecast.period.date <- max(as.numeric(index.calibration.period[,1]))
      
    }
    
    ##################################
    # Removing the time-index column #
    ##################################
    timeseries.no.date <- index.calibration.period %>%
      dplyr::select(all_of(location.names)) # Selecting only locations 
    
    #######################################################
    # Determining the number of columns in the data frame #
    #######################################################
    ncols <- base::ncol(timeseries.no.date)
    
    ###########################################################
    # Saving the desired alpha values in which to produce PIs #
    ###########################################################
    alphas <- c(0.02, 0.05, seq(0.1, 0.9, by=0.1))
    
    #############################
    # Looping through locations #
    #############################
    for(g in 1:ncols){
      
      ####################################################################
      # Setting up the information needed for each iteration of the loop #
      ####################################################################
      
      # Selecting the data associated with the indexed location 
      data <- timeseries.no.date[,g]
      
      # Location indexed
      location.index <- location.names[g]
      
      # Saving the calibration period for modeling as vector 
      data.cur <- na.omit(as.numeric(data))
    
#------------------------------------------------------------------------------#
# Data Smoothing ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section implements smoothing if indicated by the user.  #
# Additionally, it runs a check to see if smoothing can be implemented         #
# (i.e., only with daily data). If smoothing is miss-entered, the function will#
# return a warning, and not implement the smoothing indicated by the user. The #
# section also rounds the smoothed values to the nearest integer when using    #
# the NB or Poisson distributions.                                             #
#------------------------------------------------------------------------------#
      
      ######################################
      # Not applying smoothing to the data #
      ######################################
      if(smoothing.input.G == 1 || is.null(smoothing.input.G)){
        
        # Data to be used for the remainder of the code
        data.cur <- data.cur
      
        ######################################################################
        # Runs if smoothing is indicated - Rounds if NB or Poisson is chosen #
        ######################################################################
        }else{
          
          # Rounding the data if the distribution is not normal 
          if(error.input.G %in% c("Negative Binomial (NB)", "Poisson")){
            
            # Rounding the smoothed data
            data.cur <- round(rollmean(data.cur, smoothing.input.G), 0)
            
          }else{
            
            # No rounding with the smoothed data 
            data.cur <- rollmean(data.cur, smoothing.input.G) 
            
          } # End of else for rounding smoothing 
          
          } # End of else for determining if should round or not   
      
      #########################################################################
      # Creating the `time` predictor, which is the length of the calibration #
      #########################################################################
      time <- seq(1:length(data.cur))
    
    
#------------------------------------------------------------------------------#
# Fitting the GAM Model --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section fits the GAM model to the time series data, using the    #
# the 'gam' function and employing the 'k' value determined above. The user    #
# can select the underlying distribution in the 'options.R' file.              #
#------------------------------------------------------------------------------#

      #############################
      # Determining the model fit #
      #############################
      
      # Checking for the error
      test <- try(
        
      gam.mod <- base::switch(error.input.G,
                              "Normal" = mgcv::gam(data.cur~s(time,bs=smoothing.term.G,k=k.input.G)), # Normal distribution 
                              "Poisson" = mgcv::gam(data.cur~s(time,bs=smoothing.term.G,k=k.input.G), family=poisson), # Poisson distribution
                               mgcv::gam(data.cur~s(time,bs=smoothing.term.G,k=k.input.G), family=nb())) # Negative-binomial distribution 
    
      )
      
      #######################
      # If the error occurs #
      #######################
      if (inherits(test, 'try-error')) {
        
        # Saving an NA in the list 
        quantile.list.locations[[g]] <- NA
        
        # Adding names to list data frames
        names(quantile.list.locations)[g] <- paste0("GAM-", location.index, "-", forecast.period.date)
        
        # Skipping to next loop 
        next
        
      }

#------------------------------------------------------------------------------#
# GAM Forecasting --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the model fit from above, and forecasts out the    #
# indexed horizon amount. Any produced negative quantiles are truncated at     #
# zero, and produced quantiles are combined into a single data frame. The data #
# frame is also re-ordered an re-labeled to be consistent with the other       #
# models available within the toolbox.                                         #
#------------------------------------------------------------------------------#    
    
    ############################################
    # Producing forecast from fitted GAM model #
    ############################################
    
    # Forecasting from the model fit
    fcst <- stats::predict(gam.mod, data.frame(time=1:(length(data.cur) + horizon.input.G)), se.fit=T)
    
    # Creating a variable that includes the forecasted values/means 
    GAMForecast <- as.data.frame(as.numeric(fcst$fit))
    
    # Checking residuals of GAM model 
    residuals <- capture.output(mgcv::gam.check(gam.mod))
    
    ##################################
    # Producing Prediction intervals #
    ##################################
    for(iAlpha in 1:length(alphas)){
      
      # Lower bounds of the prediction intervals 
      lower <- fcst$fit+qnorm(alphas[iAlpha]/2)*fcst$se.fit 
      
      # Upper bounds of the prediction intervals 
      upper <- fcst$fit+qnorm(1-alphas[iAlpha]/2)*fcst$se.fit 
      
      # Combining the forecasted means, and upper and lower PIs into one
      GAMForecast <- base::cbind(GAMForecast,base::cbind(as.numeric(lower),as.numeric(upper)))
      
      } # End of loop producing PI intervals
    
    # Changing all negative values to 0 in the forecast data frame 
    GAMForecast[GAMForecast < 0] <- 0
    
    # Inverse of link function if using NB or Poisson
    if(error.input.G %in% c("Poisson", "Negative Binomial (NB)")){
      
      GAMForecast <- exp(GAMForecast)
      
    }
    
    ###############################################
    # Labeling the columns of the forecasted data #
    ###############################################
    
    # Assigning the names to the columns of the forecasted data frame 
    names(GAMForecast) <- c("fitted", paste(rep(c("l", "u"), length(alphas)),
                                            rep(alphas,each=2), sep=" "))
    
    
    # Re-ordering and re-naming data columns for producing metrics later
    GAMForecast <- GAMForecast %>%
      # Re-ordering the columns
      dplyr::select(fitted, `l 0.9`, `l 0.8`, `l 0.7`, `l 0.6`, `l 0.5`, `l 0.4`, `l 0.3`, `l 0.2`,
                    `l 0.1`, `l 0.05`, `l 0.02`, `u 0.9`, `u 0.8`, `u 0.7`, `u 0.6`, `u 0.5`, `u 0.4`,
                    `u 0.3`, `u 0.2`, `u 0.1`, `u 0.05`, `u 0.02`) %>%
      # Re-naming the columns
      dplyr::rename(`means` = fitted,
                    `lower.10%` = `l 0.9`,
                    `lower.20%` = `l 0.8`,
                    `lower.30%` = `l 0.7`,
                    `lower.40%` = `l 0.6`,
                    `lower.50%` = `l 0.5`,
                    `lower.60%` = `l 0.4`,
                    `lower.70%` = `l 0.3`,
                    `lower.80%` = `l 0.2`,
                    `lower.90%` = `l 0.1`,
                    `lower.95%` = `l 0.05`,
                    `lower.98%` = `l 0.02`,
                    `upper.10%` = `u 0.9`,
                    `upper.20%` = `u 0.8`,
                    `upper.30%` = `u 0.7`,
                    `upper.40%` = `u 0.6`,
                    `upper.50%` = `u 0.5`,
                    `upper.60%` = `u 0.4`,
                    `upper.70%` = `u 0.3`,
                    `upper.80%` = `u 0.2`,
                    `upper.90%` = `u 0.1`,
                    `upper.95%` = `u 0.05`,
                    `upper.98%` = `u 0.02`) %>%
      mutate_all(~ round(., 2))
    
    ########################################
    # Handling when data smoothing is used #
    ########################################
    if(smoothing.input.G == 1 || is.null(smoothing.input.G)){
      
      # Keeping orignal forecast
      GAMForecast <- GAMForecast
      
      
    ################################
    # Runs if smoothing is applied #
    ################################
    }else{
      
      # Keeping only the forecast horizon
      GAMForecast <- GAMForecast[(nrow(GAMForecast) - horizon.input.G + 1):nrow(GAMForecast),]

    }
    
#------------------------------------------------------------------------------#
# Creating a list of quantile frames -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a list of quantile forecasts to be later added   #
# to a list with the residuals. Each forecast is labeled with its              #
# location and forecast period.                                                #
#------------------------------------------------------------------------------#
    
    ############################################
    # Adding the forecast to the quantile list #
    ############################################
    quantile.list.locations[[g]] <- GAMForecast
    
    # Adding names to list data frames
    names(quantile.list.locations)[g] <- paste0("GAM-", location.index, "-", forecast.period.date)
    
    #########################################
    # Adding residuals to the residual list #
    #########################################
    residuals.list.locations[[g]] <- residuals
    
    # Adding names to list data frames
    names(residuals.list.locations)[g] <- paste0("GAM-", location.index, "-", forecast.period.date)
    
    } # End of locations loop 
    
    #########################################
    # Adding the forecasts to the main list #
    #########################################
    quantile.list <- c(quantile.list, quantile.list.locations)
    
    #####################################
    # Adding residuals to the main list #
    #####################################
    residuals.list <- c(residuals.list, residuals.list.locations)
    
  } # End of calibration period loop 
  
#------------------------------------------------------------------------------#
# Combining lists to return ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section combines the quantile forecasts and residual info        #
# to be returned to the main shiny app.                                        #
#------------------------------------------------------------------------------#
  
  ###################
  # Making the list #
  ###################
  final.list <- list(quantile.list, residuals.list)
  
  # Adding names
  names(final.list) <- c("Forecasts", "Residuals")
  
  ######################
  # Returning the list #
  ######################
  return(final.list)
  
} # End of function 


  