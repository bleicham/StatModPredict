#------------------------------------------------------------------------------#
#                                                                              #
#                Generalized Linear Regression (GLM) Forecasting               #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file takes in output related to the user selected forecasting periods,  #
# desired forecasting horizon, type of date data, data smoothing, and 'GLM'    #
# related parameters to produce a list of GLM based quantile forecasts. The    #
# forecasts are outputted in quantile format, with a median                    #
# prediction and 22 additional quantiles. The user also has the option to      #
# customize the GLM error distribution. Available error distributions include  #
# normal, Poisson, and negative binomial. The 'gam' function within the        #
# "stats" package is used to fit the model with time as the only covariate,    #
# and additional details on the function can be found at:                      #
#                                                                              #
# (1) glm: Fitting Generalized Linear Models. Version 3.6-2. [cited 2023 Oct   #
#     23]. Available from: https://www.rdocumentation.org/packages/stats/vers  #
#     ions/3.6.2/topics/glm                                                    #
#                                                                              #
# Forecasts are outputted in quantile format, with a a median prediction, as   #
# the model assumes normality, and 22 additional quantiles. Available error    #
# distributions include normal, Poisson, and negative binomial.                #
#------------------------------------------------------------------------------#
#                Authors: Amanda Bleichrodt and Ruiyan Luo                     #
#------------------------------------------------------------------------------#


GLM <- function(calibration.input, horizon.input, date.Type.input, 
                smoothing.input, error.input){
  
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Below takes in the inputs from the function, and saves them under new #        
# variable names to prevent over-writing of values.                            #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Reading in the list of calibration periods #
  ##############################################
  calibration.list.GLM <- calibration.input
  
  ##################################
  # Saving the forecasting horizon #
  ##################################
  horizon.input.GLM <- horizon.input
  
  #############
  # Date type #
  #############
  date.Type.input.GLM <- date.Type.input
  
  #####################
  # Smoothing of data #
  #####################
  smoothing.input.GLM <- smoothing.input
  
  ###############
  # Error input #
  ###############
  error.input.GLM <- error.input
  
  ########################################
  # Creating an empty list for quantiles #
  ########################################
  quantile.list <- list()
  
  #######################################################
  # Creating an empty list for quantiles with locations #
  #######################################################
  quantile.list.locations <- list()
  
#------------------------------------------------------------------------------#
# Setting up the GLM loop  -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets up the information needed for GAM forecasting,      #
# including preparing the data and setting up the parameters needed for GAM    #
# forecasting. It also sets up the loops going through calibration periods.    #
#------------------------------------------------------------------------------#
  
  ########################################
  # Starting the calibration period loop #
  ########################################
  for(c in 1:length(calibration.list.GLM)){
    
    ##############################
    # Indexed calibration period #
    ##############################
    index.calibration.period <- calibration.list.GLM[[c]]
    
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
    for(s in 1:ncols){
      
      ####################################################################
      # Setting up the information needed for each iteration of the loop #
      ####################################################################
      
      # Selecting the data associated with the indexed location 
      data <- timeseries.no.date[,s]
      
      # Location indexed
      location.index <- location.names[s]
      
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
      if(smoothing.input.GLM == 1 || is.null(smoothing.input.GLM)){
        
        # Data to be used for the remainder of the code
        data.cur <- data.cur
        
        ######################################################################
        # Runs if smoothing is indicated - Rounds if NB or Poisson is chosen #
        ######################################################################
        }else{
        
          # Rounding the data if the distribution is not normal 
          if(error.input.GLM %in% c("Negative Binomial (NB)", "Poisson")){
            
            # Rounding the smoothed data
            data.cur <- round(rollmean(data.cur, smoothing.input.GLM), 0)
            
          }else{
            
            # No rounding with the smoothed data 
            data.cur <- rollmean(data.cur, smoothing.input.GLM) 
            
          } # End of else for rounding smoothing 
          
        } # End of else for determining if should round or not   
      
      #########################################################################
      # Creating the `time` predictor, which is the length of the calibration #
      #########################################################################
      time <- seq(1:length(data.cur))

#------------------------------------------------------------------------------#
# Fitting the GLM Model --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section fits the GLM model to the time series data, using the    #
# the 'glm' function and employing time as the only covariate.                 #
#------------------------------------------------------------------------------#    
      
      #############################################################
      # Fitting the GLM model, based on error assumption selected #
      #############################################################
    
      # Checking for the error
      test <- try(
        
        # Selecting the model fit based on the error distribution selected by the user
        glm.mod <- base::switch(error.input.GLM,
                                "Normal" = stats::glm(data.cur ~ time), # Normal distribution 
                                "Poisson" = glm(data.cur ~ time, family = poisson), # Poisson distribution
                                glm.nb(data.cur ~ time)) # Negative-binomial distribution 
        
      )
      
      #######################
      # If the error occurs #
      #######################
      if (inherits(test, 'try-error')) {
        
        # Saving an NA in the list 
        quantile.list.locations[[s]] <- NA
        
        # Adding names to list data frames
        names(quantile.list.locations)[s] <- paste0("GLM-", location.index, "-", forecast.period.date)
        
        # Skipping to next loop 
        next
        
      }


#------------------------------------------------------------------------------#
# GLM Forecasting --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the model fit from above, and forecasts out the    #
# indexed horizon amount. Any produced negative quantiles are truncated at     #
# zero, and produced quantiles are combined into a single data frame. The data #
# frame is also re-ordered an re-labeled to be consistent with the other       #
# models available within the toolbox.                                         #
#------------------------------------------------------------------------------#   
    
    ############################################
    # Producing forecast from fitted GLM model #
    ############################################
    
    # Forecasting from the model fit
    fcst <- stats::predict(glm.mod, data.frame(time=1: 
                                                 (length(data.cur) + horizon.input.GLM)), se.fit=T)
    
  
    # Saving the model forecasts - both historic and ahead 
    GLMForecast <- as.data.frame(as.numeric(fcst$fit))
    
    ##################################
    # Producing Prediction intervals #
    ##################################
    for(iAlpha in 1:length(alphas)){
      
      # Lower bounds of the prediction intervals 
      lower <- fcst$fit+qnorm(alphas[iAlpha]/2)*fcst$se.fit 
      
      # Upper bounds of the prediction intervals 
      upper <- fcst$fit+qnorm(1-alphas[iAlpha]/2)*fcst$se.fit 
      
      # Combining the forecasted means, and upper and lower PIs into one
      GLMForecast <- base::cbind(GLMForecast,base::cbind(as.numeric(lower),as.numeric(upper)))
      
    } # End of loop producing PI intervals
    
    # Changing all negative values to 0 in the forecast data frame 
    GLMForecast[GLMForecast < 0] <- 0
    
    # Inverse of link function if using NB or Poisson
    if(error.input.GLM %in% c("Poisson", "Negative Binomial (NB)")){
      
      GLMForecast <- exp(GLMForecast)
      
    }
    
    ###############################################
    # Labeling the columns of the forecasted data #
    ###############################################
    
    # Assigning the names to the columns of the forecasted data frame 
    names(GLMForecast) <- c("fitted", paste(rep(c("l", "u"), length(alphas)),
                                            rep(alphas,each=2), sep=" "))
    
    
    # Re-ordering and re-naming data columns for producing metrics later
    GLMForecast <- GLMForecast %>%
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
    if(smoothing.input.GLM == 1 || is.null(smoothing.input.GLM)){
      
      # Keeping what was used before
      GLMForecast <- GLMForecast 
    
    ############################
    # Runs if an error occured #
    ############################
    }else{

      # Keeping only the forecast horizon
      GLMForecast <- GLMForecast[(nrow(GLMForecast) - horizon.input.GLM + 1):nrow(GLMForecast),]

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
    quantile.list.locations[[s]] <- GLMForecast
    
    # Adding names to list data frames
    names(quantile.list.locations)[s] <- paste0("GLM-", location.index, "-", forecast.period.date)
    
    # #########################################
    # # Adding residuals to the residual list #
    # #########################################
    # residuals.list.locations[[g]] <- residuals
    # 
    # # Adding names to list data frames
    # names(residuals.list.locations)[g] <- paste0("GAM-", location.index, "-", forecast.period.date)
    
    } # End of locations loop 
    
    #########################################
    # Adding the forecasts to the main list #
    #########################################
    quantile.list <- c(quantile.list, quantile.list.locations)
    
    #####################################
    # Adding residuals to the main list #
    #####################################
    #residuals.list <- c(residuals.list, residuals.list.locations)
    
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
  final.list <- list(quantile.list)
  
  # Adding names
  names(final.list) <- c("Forecasts")
  
  ######################
  # Returning the list #
  ######################
  return(final.list)
  
} # End of function 

