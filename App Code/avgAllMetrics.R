#------------------------------------------------------------------------------#
#                                                                              #
#                     Calculating the Average Metrics                          #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This section prepares the average metrics based upon the forecasts    #
# conducted in the dashboard, and read into the dashboard. It then exports a   #
# data frame containing the average metrics for each model, performance type,  #
# location, calibration period, and forecasting horizon to the main dashboard. #
#------------------------------------------------------------------------------#
# By: Amanda Bleichrodt                                                        #
#------------------------------------------------------------------------------#

avgAllMetrics <- function(metricsCrude.input){
  
#------------------------------------------------------------------------------#
# Renaming the inputs ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the inputs to be referenced throughout the rest  #
# of the function.                                                             #
#------------------------------------------------------------------------------#
  
################################
# Reading in the crude metrics #
################################
crudeMetrics <- metricsCrude.input

#------------------------------------------------------------------------------#
# Determining the average metrics ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each metric indicated by the user, and     #
# then outputs the average of each metric, grouped by Model, performance       #
# metric type, location, calibration period, and forecasting horizon.          #
#------------------------------------------------------------------------------#

###################################
# Creating a list of metric names #
###################################
nameList <- c(colnames(crudeMetrics[,(7:ncol(crudeMetrics))]))

###############################################
# Setting up grouping for the average metrics #
###############################################
averageMetrics <- crudeMetrics %>%
  dplyr::group_by(Model, `Performance Metric Type`, Location, Calibration, Horizon) # Grouping variables 

########################################################
# Looping through the metrics to detremine the average #
########################################################
for(i in 1:length(nameList)){
  
  # Indexed metric name 
  indexedMetric <- nameList[i]
  
  # Calculating the average 
  averageMetrics <- averageMetrics %>%
    dplyr::mutate(nameHolder = round(mean(get(indexedMetric), na.rm = T),2))
  
  # Naming the column
  names(averageMetrics)[ncol(averageMetrics)] <- paste0("Avg. ", indexedMetric)

}

#------------------------------------------------------------------------------#
# Preparing the final data for export ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the final average data for export. It removes   #
# any columns that are no longer needed, reorders the columns, and rounds to   #
# two decimal points.                                                          #
#------------------------------------------------------------------------------#

#################################
# Preparing the data for export #
#################################
finalData <- averageMetrics %>%
  dplyr::distinct(Model, `Performance Metric Type`, Location, Calibration, Horizon, .keep_all = T) %>% # Removing repeat rows
  dplyr::select(-nameList, -Date) # Removing unneeded variables 

}
