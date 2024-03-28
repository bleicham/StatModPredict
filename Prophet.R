#------------------------------------------------------------------------------#
#                                                                              #
#                   Facebook's Prophet Model Forecasting                       #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file takes in the forecast periods from the main dashboard, and Prophet #
# specifications. Additionally, it takes in information regarding the indexed  #
# forecasting horizon, the process of interest, the type of data, the          #
# date composition of the data (i.e., week, day, year, time index), and if     #
# the data should be smoothed in any way. The inputted information is then     #
# used to fit the Prophet model using the default settings of the model, and   #
# produce forecasts using the fitted model and the inputted forecasting        #
# horizon. The 'prophet' function from the 'prophet' package is used to fit    #
# the model, and additional details on the function can be found at:           #                                        #
#                                                                              #
# (1) Taylor S. prophet: Prophet forecaster. Version 1.0. 2021 [cited 2023     #
#     Oct 16]. Available: https://www.rdocumentation.org/packages/prophet/     #
#     versions/1.0/topics/prophet                                              #
#                                                                              #
# (2) Taylor SJ, Letham B. Forecasting at scale. Am Stat. 2018;72: 37–45.      #
#     doi:10.1080/00031305.2017.1380080.                                       #
#                                                                              #
# Examples of the applications of the function in forecasting efforts can be   #
# found in the following:                                                      #
#                                                                              #
# (1) Kirpich A, Shishkin A, Thomas A, Weppelmann A, Perez Tcher- Nov P, Skums #
#     Y. Excess mortality in belarus during the covid-19 pan- demic as the     #
#     case study of a country with limited non-pharmaceutical interventions    #
#     and limited reporting. Scientific Reports. 2022;12. doi:10.1038/s41598   #
#     -022-093454-z.                                                           #
#                                                                              #
# (2) Battineni G, Chintalapudi N, Amenta F. Forecasting of COVID-19 epidemic  #
#     size in four high hitting nations (USA, Brazil, India and Russia) by Fb- #
#     Prophet machine learning model. Appl Comput Inform. 2020. doi:10.1108/a  #
#     ci-09-2020-0059.                                                         #
#                                                                              #
# Forecasts are outputted in quantile format, with a a median prediction, as   #
# the model assumes normality, and 22 additional quantiles.                    #
#------------------------------------------------------------------------------#
#             Authors: Amanda Bleichrodt and Alexander Kirpich                 #
#------------------------------------------------------------------------------#

Prophet <- function(calibration.input, horizon.input, date.type.input,
                    smoother.input, seasonalityProphet.input,
                    holidayDates.input, growthTrend.input){
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Below takes in the inputs from the function, and saves them under new #        
# variable names to prevent over-writing of values.                            #
#------------------------------------------------------------------------------#
  
  ######################################
  # Reading in the calibration periods #
  ######################################
  calibration.input.P <<- calibration.input
  
  ##################################
  # Saving the forecasting horizon #
  ##################################
  horizon.input.P <<- horizon.input
  
  #############
  # Date type #
  #############
  date.Type.input.P <<- date.type.input
  
  #####################
  # Smoothing of data #
  #####################
  smoothing.input.P <<- smoother.input
  
  ###############
  # Growth type #
  ###############
  growth.input.P <<- growthTrend.input
  
  #######################
  # Seasonality Prophet #
  #######################
  seasonality.input.P <<- seasonalityProphet.input
  
  #################
  # Holiday dates #
  #################
  holidayDates.input.P <<- holidayDates.input
  
  ########################################
  # Creating an empty list for quantiles #
  ########################################
  quantileList <- list()
  
  #######################################################
  # Creating an empty list for quantiles with locations #
  #######################################################
  quantileListLocations <- list()
  
#------------------------------------------------------------------------------#
# Creating the data frame for holidays -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes in the holiday dates specified by the users and    #
# creates the needed data frame for the Prophet object.                        #
#------------------------------------------------------------------------------#
  
  ###############################
  # Runs if dates are specified #
  ###############################
  if(length(holidayDates.input.P > 0)){
    
    # Arrange holiday dates from oldest to newest 
    holiday.dates <- sort(holidayDates.input.P) 
    
    # Creating the data frame
    holiday.data.frame <- data.frame(holiday = c(as.character(seq(1:length(holiday.dates)))),
                                     ds = c(holiday.dates))
    
  }else{
    
    holiday.data.frame <- NULL
    
  }
  
#------------------------------------------------------------------------------#
# Handling seasonality inputs --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the seasonality indicated by the user to create a   #
# vector that is then employed in the prophet object.                          #
#------------------------------------------------------------------------------#
  
  #########################
  # If 'Auto' is selected #
  #########################
  if(seasonality.input.P == "Auto"){
    
    # What is shown for 'yearly', 'weekly', and 'daily'
    seasonalDataFrame <- c("auto", "auto", "auto")
    
    #########################
    # If 'None' is selected #
    #########################
  }else if(seasonality.input.P == "None"){
    
    # What is shown for 'yearly', 'weekly', and 'daily'
    seasonalDataFrame <- c(FALSE, FALSE, FALSE)
    
    ###########################
    # If 'Yearly' is selected #
    ###########################
  }else if(seasonality.input.P == "Yearly"){
    
    # What is shown for 'yearly', 'weekly', and 'daily'
    seasonalDataFrame <- c(TRUE, FALSE, FALSE)
    
    ###########################
    # If 'Weekly' is selected #
    ###########################
  }else if(seasonality.input.P == "Weekly"){
    
    # What is shown for 'yearly', 'weekly', and 'daily'
    seasonalDataFrame <- c(FALSE, TRUE, FALSE)
    
    ##########################
    # If 'Daily' is selected #
    ##########################
  }else{
    
    # What is shown for 'yearly', 'weekly', and 'daily'
    seasonalDataFrame <- c(FALSE, FALSE, TRUE)
    
  }
  
#------------------------------------------------------------------------------#
# Setting up the Prophet loop  -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets up the information needed for Prophet forecasting   #
# such as the needed quantiles, the names of the included locations, the data, #
# naming the columns in the data, and starting the loop that will produce      #
# forecasts for one location at a time and one calibration period at a time.   #
#------------------------------------------------------------------------------#
  
  ###################################################
  # Quantiles used in model fitting and forecasting #
  ###################################################
  quantiles <- c(0.02, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
  
  #######################################
  # Looping through calibration periods #
  #######################################
  for(i in 1:length(calibration.input.P)){
    
    # Indexed calibration period
    data.input.P <- calibration.input.P[[i]]
    
    ################################
    # Creating a list of locations #
    ################################
    location.names <- names(data.input.P)[-1]
    
    #############################
    # Looping through locations #
    #############################
    for(p in 1:length(location.names)){
      
      # Location indexed
      location.index <- location.names[[p]]
      
      # Calibration period 
      data1 <- data.input.P %>%
        dplyr::select(date, location.index)
      
      # Determining the forecast period - Daily or Weekly
      if(all(str_length(data1[,1]) > 4)){
        
        forecast.period.date <- max(anytime::anydate(data1[,1]))
        
      }else{
        
        forecast.period.date <- max(as.numeric(data1[,1]))
        
      }
      
      # Re-naming columns for forecasting 
      names(data1) <- c("ds", "y")
      
#------------------------------------------------------------------------------#
# Data Smoothing ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section implements smoothing if indicated by the user.  #
# Additionally, it runs a check to see if smoothing can be implemented         #
# (i.e., only with daily data). If smoothing is miss-entered, the function will#
# return a warning, and not implement the smoothing indicated by the user.     #
#------------------------------------------------------------------------------#
      
      # ####################################################
      # # Applying (or not applying) smoothing to the data #
      # ####################################################
      # if(smoothing.input.P == 0 || is.null(smoothing.input.P)){
      #   
      #   # Data to be used for the remainder of the code
      #   data1 <- data1
      #   
      #   # Run if smoothing is indicated 
      # }else{
      #   
      #   # Data w/smoothing employed 
      #   data1 <- rollmean(data1[,2], smoothing.input.P) 
      #   
      # } 
      
      # Determining the forecast period - Daily or Weekly
      if(all(str_length(data1$ds) > 4)){
          
        data1 <- data1 %>%
          dplyr::mutate(ds = anytime::anydate(ds))
        
      }else{
        
        data1 <- data1 %>%
          dplyr::mutate(ds = as.numeric(ds))
      }
      
#------------------------------------------------------------------------------#
# Fitting from the Prophet Model -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the quantiles for fitting and forecasting from  #
# the model, and fits  the Prophet model with default settings to the time     #
# series data.                                                                 #
#------------------------------------------------------------------------------#       
      
      #####################################################################
      # Preparing the quantiles for forecasting and model fit uncertainty #
      #####################################################################
      
      # List of quantiles to be forecasted 
      quantiles_names_list <- paste0(100 - 100*quantiles, "%") 
      
      # Creating the empty data frame to fill in with estimates
      all_quantiles <- data.frame(base::matrix(data = 0, nrow = dim(data1)[1] + horizon.input.P, ncol = 24))
      
      # Naming the columns of the `all_quantiles` data frame
      names(all_quantiles) <- c("ds", "prediction", paste0( "lower.", quantiles_names_list), paste0( "upper.", quantiles_names_list))
      
      
      ################################################################
      # Looping through quantiles to produce forecast and model fits #
      ################################################################
      for(quantile_current in quantiles) {
        
        # Loop indexed quantile 
        uncertainty_level_current <- 1 - quantile_current
        
        ##############################
        # Creating a prophet object. #
        ##############################

        # Checking for the error
        test <- try(
          
        # Fitting a prophet model that does not assume weekly seasonality
        prophet_object_current <- prophet::prophet(data1, 
                                                   interval.width = uncertainty_level_current, 
                                                   growth = growth.input.P,
                                                   holidays = holiday.data.frame,
                                                   yearly.seasonality = seasonalDataFrame[1], 
                                                   weekly.seasonality = seasonalDataFrame[2],
                                                   daily.seasonality = seasonalDataFrame[3])
        
        )
        
        if (inherits(test, 'try-error')) {
          
          break
          
        }
        

#------------------------------------------------------------------------------#
# Producing Prophet Forecasts --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the forecast period dates for producing the     #
# Prophet forecasts from the model fit above, and formatting the quantile      #
# output. It also adjusts the labels of the quantile columns to correspond     #
# to the other models included in the toolbox.                                 #
#------------------------------------------------------------------------------#
        
        ###################################
        # Preparing the forecasting dates #
        ###################################
        forecasted_dates <- switch(date.Type.input.P,
                                    "day" = c(seq.Date(max(anytime::anydate(data1$ds)) + 1, 
                                                       max(anytime::anydate(data1$ds)) + horizon.input.P, by = "1 day")),
                                    "week" = c(seq.Date(max(anytime::anydate(data1$ds)) + 7, 
                                                        max(anytime::anydate(data1$ds)) + horizon.input.P*7, by = "1 week")),
                                    c(seq(max(as.numeric(data1$ds)) + 1, max(as.numeric(data1$ds)) + horizon.input.P)))
        
        # Creating the dates for forecasting
        if(date.Type.input.P %in% c("week", "day")){
          
          # Dates to produce estimates and PIs for
          dates_for_forecast <- base::rbind(subset(data1, select = anytime::anydate(ds)),
                                            data.frame(ds = c(as.character(forecasted_dates))))
          
        }else{
          
          # Dates to produce estimates and PIs for
          dates_for_forecast <- base::rbind(subset(data1, select = ds),
                                            data.frame(ds = c(as.numeric(forecasted_dates))))
          
        }
        
        ######################################
        # Predicting for the specified dates #
        ######################################
        prophet_predictions <- stats::predict(prophet_object_current, dates_for_forecast) %>%
          mutate(ds = dates_for_forecast[,1]) # Reformatting date
        
        #########################################################################
        # Combining the observed values and predicted values by the date column #
        #########################################################################
        predictions_observed_data <- prophet_predictions %>%
          dplyr::select(ds, yhat, yhat_lower, yhat_upper)
        
        ############################################################
        # Setting labels for the model fit and quantile data frame #
        ############################################################
        
        # Labeling the predicted value
        all_quantiles_yhat_text  <-
          paste0("all_quantiles$prediction <- predictions_observed_data$yhat")
        # Running predicted value (text) code
        eval(parse(text = all_quantiles_yhat_text))
        
        # Labeling the lower bounds 
        all_quantiles_yhat_lower_text  <-
          paste0("all_quantiles$`lower.", 100*uncertainty_level_current, "%` <- predictions_observed_data$yhat_lower")
        # Running LB value (text) code
        eval(parse(text = all_quantiles_yhat_lower_text))
        
        # Labeling the upper bounds
        all_quantiles_yhat_upper_text  <-
          paste0("all_quantiles$`upper.", 100*uncertainty_level_current, "%` <- predictions_observed_data$yhat_upper")
        # Running UB value (text) code
        eval(parse(text = all_quantiles_yhat_upper_text))
        
      }
      
 
      #############################################################
      # Formatting the all-quantiles data frame for later metrics #
      #############################################################
      
      # Changing any negative value in the data frame to zero 
      all_quantiles[all_quantiles<0] <- 0
      
      ####################################
      # Renaming and re-ordering columns #
      ####################################
      all_forecast <- all_quantiles[,-1] %>%
        dplyr::select(prediction, 
                      `lower.10%`,
                      `lower.20%`,
                      `lower.30%`,
                      `lower.40%`,
                      `lower.50%`,
                      `lower.60%`,
                      `lower.70%`,
                      `lower.80%`,
                      `lower.90%`,
                      `lower.95%`,
                      `lower.98%`,
                      `upper.10%`,
                      `upper.20%`,
                      `upper.30%`,
                      `upper.40%`,
                      `upper.50%`,
                      `upper.60%`,
                      `upper.70%`,
                      `upper.80%`,
                      `upper.90%`,
                      `upper.95%`,
                      `upper.98%`) %>%
        mutate_all(~ round(., 2))
      
      
#------------------------------------------------------------------------------#
# Creating a list of quantile frames -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a list of quantile forecasts to be later added   #
# to a list with the best-fit models. Each forecast is labeled with its        #
# location and forecast period.                                                #
#------------------------------------------------------------------------------#
      
      #######################
      # If the error occurs #
      #######################
      if (inherits(test, 'try-error')) {
        
        # Saving an NA in the list
        quantileListLocations[[p]] <- NA
        
        # Adding names to list data frames
        names(quantileListLocations)[p] <- paste0("Prophet-", location.index, "-", forecast.period.date)
        
        # Skipping to next loop
        next
        
      }

      ############################################
      # Adding the forecast to the quantile list #
      ############################################
      quantileListLocations[[p]] <- all_forecast
      
      # Adding names to list data frames
      names(quantileListLocations)[p] <- paste0("Prophet-", location.index, "-", forecast.period.date)
 
    } # End of location loop 
    
    #########################################
    # Adding the forecasts to the main list #
    #########################################
    quantileList <- c(quantileList, quantileListLocations)
    
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
  finalList <- list(quantileList)
  
  # Adding names
  names(finalList) <- c("Forecasts")
  
  ######################
  # Returning the list #
  ######################
  return(finalList)
  
} # End of function
    
    
