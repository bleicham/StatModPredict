#------------------------------------------------------------------------------#
#                                                                              #
#                      Combining the Performance Metrics                       #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function combines the metrics calculated in the dashboard for the ARIMA,#
# GLM, GAM, and Prophet functions and any performance metrics a user wants to  #
# read into the dashboard. There are few limiations on the format of the files #
# read into the dashboard; however, metrics related to MSE, MAE, WIS, and 95%  #
# PI must be labeled MSE, MAE, WIS, and Coverage.95.PI, respectivly. Also,     #
# the file naming scheme for the performance metrics is as follows:            #
#                                                                              #
# Performance-<Fit or Forecast>-horizon-<Horizon #>-calibration-<calibration   #
# #>-Location-<Forecast Period Date DD-MM-YYYY or YYYY>                        #
#                                                                              #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
combiningAllMetrics <- function(otherMetrics.input, modelFit.input, 
                                modelForecast.input, horizon.input, 
                                calibration.input, locations.input) {
  
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
# Renaming the inputs ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the inputs used throughout the remainder of the  #
# dashboard.                                                                   #
#------------------------------------------------------------------------------#
  
####################################################
# Reading in the list of other performance metrics #
####################################################
otherMetricsList <<- otherMetrics.input

#################################################################
# Model fit metrics from the ARIMA, GLM, GAM, Prophet dashboard #
#################################################################
fitMetrics <<- modelFit.input

######################################################################
# Model forecast metrics from the ARIMA, GLM, GAM, Prophet dashboard #
######################################################################
metricsForecast <<- modelForecast.input

#####################################
# Forecast horizon in the dashboard #
#####################################
horizonInput <<- horizon.input

#######################################
# Calibration period in the dashboard #
#######################################
calibrationInput <<- calibration.input

##############################
# Locations in original data #
##############################
locationsOrg <<- locations.input


#-------------------------------------------------------------------------------
# Looping through the other files for cleaning ---------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through the list of performance metrics,           #
# determines if they are related to model fit or forecast metrics, the date    #
# the metrics are associated with, and prepares the data for final merging     #
# with the ARIMA, GLM, GAM, and Prophet metrics.                               #
#------------------------------------------------------------------------------#

##########################################
# Empty list data set to merge all files #
##########################################
otherPerformanceMetrics <- NULL

##############################################################
# Looping through other metric files read into the dashboard #
##############################################################
for(i in 1:length(otherMetricsList)){
  
  ##########################################
  # Calling the indexed performance metric #
  ##########################################
  metricSet <- otherMetricsList[[i]]
  
  # Calling the name of the indexed performance metric
  metricSetName <- names(otherMetricsList)[i]
  
  ##########################################
  # Pulling information from the file name #
  ##########################################
  
  # Type of performance metrics
  metricType <- qdapRegex::ex_between(metricSetName, "Performance-", "-")[[1]][1]
  
  # Forecast horizon
  horizon <- qdapRegex::ex_between(metricSetName, "horizon-", "-calibration")[[1]][1]
  
  # Calibration size 
  calibration <- qdapRegex::ex_between(metricSetName, "calibration-", "-")[[1]][1]
  
  # Location
  location <- qdapRegex::ex_between(metricSetName, paste0("calibration-", calibration, "-"), "-")[[1]][1]
  
  # Checking if the data used and the data for performance metrics match
  if(location %!in% c(locationsOrg)){
    
    print("Error2")
    return("Error2")
    
  }
  
  # Forecast period date
  forecastPeriodDate <- qdapRegex::ex_between(metricSetName, paste0(location, "-"), ".csv")[[1]][1]
  
  ######################################
  # Adding information to the data set #
  ######################################
  
  # Preparing the forecast period date
  if(nchar(forecastPeriodDate) > 4){
    
    Date <- anytime::anydate(forecastPeriodDate)
    
  }else{
    
    Date <- as.numeric(forecastPeriodDate)
    
  }
  
  # Creating the new data set 
  combinedOtherOne <- metricSet %>%
    dplyr::mutate(PerformanceType = metricType, # Model fit or forecast metrics
                  Location = location, # Location
                  Calibration = calibration, # Calibration length
                  Horizon = horizon, # Forecast horizon
                  Date = Date) # Forecast period date
  
  ##########################
  # Adding to the data set #
  ##########################
  otherPerformanceMetrics <- rbind(otherPerformanceMetrics, combinedOtherOne)
}

#------------------------------------------------------------------------------#
# Final preparations for the other performance metrics -------------------------
#------------------------------------------------------------------------------#
# About: This section switches the other performance metrics data set from     #
# wide-to-long format. This is to allow for easy merging in later steps.       #
#------------------------------------------------------------------------------#
longOtherMetrics <- pivot_longer(data = otherPerformanceMetrics, -c(Model, PerformanceType, Location, Calibration, Horizon, Date), names_to = "Metric", values_to = "Value")

#------------------------------------------------------------------------------#
# Preparing the ARIMA, GLM, GAM, and Prophet Metrics ---------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the ARIMA, GLM, GAM, and Prophet model fit and  #
# forecast metrics for merging with the other metrics in a later step. It      #
# ensures that all the needed columns are there, and renames some common       #
# metrics (MSE, MAE, WIS, and 95% PI) for ease of later combination.           #
#------------------------------------------------------------------------------#

###################################
# Preparing the model fit metrics #
###################################

# Organizing the main data set 
modelFitAGGP <- fitMetrics %>%
  dplyr::mutate(MSE = meanMSE, # Renaming MSE
                MAE = meanMAE, # Renaming MAE
                Coverage.95.PI = mean95PI, # Renaming 95% PI Coverage
                WIS = meanWIS, # Renaming WIS
                PerformanceType = "Fit", # Performance metric type
                Calibration = calibrationInput, # Calibration period size
                Horizon = horizonInput) %>% # Forecasting horizon
  dplyr::select(Model, MAE, MSE, Coverage.95.PI, WIS, PerformanceType, Location, Calibration, Horizon, Date) # Ordering needed variables 

# Handling dates
if(nchar(fitMetrics$Date[1]) > 4){
  
  modelFitAGGP$Date <- anytime::anydate(fitMetrics$Date)
  
}else{
  
  modelFitAGGP$Date <- as.numeric(fitMetrics$Date)
  
}

###################################
# Preparing the model fit metrics #
###################################

# Organizing the main data set 
modelForecastAGGP <- metricsForecast %>%
  dplyr::mutate(MSE = meanMSE, # Renaming MSE
                MAE = meanMAE, # Renaming MAE
                Coverage.95.PI = mean95PI, # Renaming 95% PI Coverage
                WIS = meanWIS, # Renaming WIS
                PerformanceType = "Forecast", # Performance metric type
                Calibration = calibrationInput, # Calibration period size
                Horizon = horizonInput) %>% # Forecasting horizon
  dplyr::select(Model, MAE, MSE, Coverage.95.PI, WIS, PerformanceType, Location, Calibration, Horizon, Date) # Ordering needed variables 

# Handling dates
if(nrow(modelForecastAGGP) == 0){
  
  NULL
  
}else if(nchar(metricsForecast$Date[1]) > 4){
  
  modelForecastAGGP$Date <- anytime::anydate(metricsForecast$Date)
  
}else{
  
  modelForecastAGGP$Date <- as.numeric(metricsForecast$Date)
  
}

#------------------------------------------------------------------------------#
# Final preparations for the dashboard performance metrics ---------------------
#------------------------------------------------------------------------------#
# About: This section combines the model fit and forecast metrics from the     #
# main dashboard, and then switches the data set from wide-to-long format.     #
# This is to allow for easy merging in the next step.                          #
#------------------------------------------------------------------------------#

#######################################################
# Merging the model fit and forecast metric data sets #
#######################################################
AGGPMetrics <- rbind(modelFitAGGP, modelForecastAGGP)

####################################
# Wide to long performance metrics #
####################################
longAGGPMetrics <- pivot_longer(data = AGGPMetrics, -c(Model, PerformanceType, Location, Calibration, Horizon, Date), names_to = "Metric", values_to = "Value")

#------------------------------------------------------------------------------#
# Combining the other and dashboard metrics ------------------------------------
#------------------------------------------------------------------------------#
# About: This section combines the other and dashboard metrics into a single   #
# data frame. It then switches the data back to wide format, and prepares it   #
# for final export to the main dashbaord page.                                 #
#------------------------------------------------------------------------------#

###########################################
# Combing the other and dashboard metrics #
###########################################
allMetrics <- rbind(longOtherMetrics, longAGGPMetrics)

# Rounding the metrics values to two digits
allMetrics <- allMetrics %>%
  dplyr::mutate(Value = round(Value, 2)) %>%
  dplyr::filter(Location %in% c(locationsOrg))

#######################################
# Preparing the final data for export #
#######################################
finalMetrics <- pivot_wider(allMetrics, names_from = Metric, values_from = Value)

# Returning the final list
return(finalMetrics)

}