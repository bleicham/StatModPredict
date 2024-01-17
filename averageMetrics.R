#------------------------------------------------------------------------------#
#                                                                              #
#                      Calculating the Average Metrics                         #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This function calculates the average MSE, MAE, WIS, and 95% PI across #
# forecasting periods for each location and model. It then outputs the         #
# corresponding data frame.                                                    #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#

averageMetrics <- function(metrics.input, dateType.input){

#------------------------------------------------------------------------------#
# Reading in inputs ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in function inputs and saves them under different  #
# names to be used throughout the rest of the function.                        #
#------------------------------------------------------------------------------#

######################
# Crude Metrics List #
######################
crude.metrics.input.AM <- metrics.input

#############
# Date type #
#############
date.type.input.AM <- dateType.input

######################################
# Empty data frame for crude metrics #
######################################
allMetrics <- NA

#------------------------------------------------------------------------------#
# Combining all of the crude metrics -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section combines all of the crude metrics into a single list,    #
# with additional columns for location, forecast period, and associated model. #
#------------------------------------------------------------------------------#

######################################
# Looping through individual metrics #
######################################
for(a in 1:length(crude.metrics.input.AM)){
  
  # Indexed crude metric name
  indexedMetricName <- names(crude.metrics.input.AM[a])
  
  ################################################
  # Pulling information from indexed metric file #
  ################################################
  
  # Model type 
  model <- strsplit(indexedMetricName, "[-]")[[1]][1]
  
  # Sub-setting location/group name 
  locationGroupName <- strsplit(indexedMetricName, "[-]")[[1]][2]
  
  # Forecast period for weekly or daily data 
  if(date.type.input.AM %in% c("week", "day")){
    
    # Determining the forecast period from the name
    forecastPeriod <- anytime::anydate(substring(indexedMetricName, regexpr("-", indexedMetricName) + (nchar(locationGroupName) + 2)))
    
    # Forecast period for yearly or time index data 
  }else{
    
    # Determining the forecast period from the name
    forecastPeriod <- as.numeric(strsplit(indexedMetricName, "[-]")[[1]][3])
    
  } 
  
  #######################################
  # Formatting the individual data file #
  #######################################
  formattedData <- crude.metrics.input.AM[[a]] %>%
    dplyr::mutate(model = model, # Model 
                  location = locationGroupName, # Location
                  date = forecastPeriod) # Forecast period 
  
  # Fixing names in file
  names(formattedData) <- c("MSE", "MAE", "PI", "WIS", "Model", "Location", "Date")
  
  #######################################
  # Combining the individual data files #
  #######################################
  allMetrics <- rbind(allMetrics, formattedData)
  
  } # End of loop combining data

# Removing the first row of NA
allMetrics <- allMetrics[-1,]

#------------------------------------------------------------------------------#
# Calculating the average metrics ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the average metrics, meaning, the each of the #
# four metrics averaged across forecast periods for each model, and location.  #
#------------------------------------------------------------------------------#

averageMetrics <- allMetrics %>%
  dplyr::group_by(Model, Location) %>% # Grouping by location and model
  dplyr::mutate(avgMSE = mean(MSE), # Avg. MSE
                avgMAE = mean(MAE), # Avg. MAE
                avgPI = mean(PI), # Avg. PI
                avgWIS = mean(WIS)) %>% # Avg. WIS
  dplyr::select(Model, Location, avgMSE, avgMAE, avgWIS, avgPI) %>% # Selecting needed variables 
  distinct(Model, Location, .keep_all = T) # Remove duplicate rows 

# Exporting the average metrics
return(averageMetrics)

}
  
  