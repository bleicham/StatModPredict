#------------------------------------------------------------------------------#
#                                                                              #
#           Calculating the Skill Scores - ARIMA, GLM, GAM and Prophet         #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function calculates the skill scores for the ARIMA, GLM, GAM, and       #
# Prophet models. The skill scores can be calculated for the crude metrics or  #
# average metrics as determined by the user. Additionally, skill scores can be #
# calculated for the forecast or fit metrics. Finally, the user can only       #
# select one benchmark model, but many comparison models.                      #
#------------------------------------------------------------------------------#
#                        Author: Amanda Bleichrodt                             #
#------------------------------------------------------------------------------#
skill.scores.AGGP <- function(averageIndicator, locationsFilter, CrudeMetrics,
                              benchModel, compModels, winklerScoresInput){
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to be used throughout the    #
# the rest of the code.                                                        #
#------------------------------------------------------------------------------#
  
  #############################
  # Average Metrics Indicator #
  #############################
  average.input <- averageIndicator 
  
  #########################
  # Location Input Filter #
  #########################
  location.input <- locationsFilter 

  ######################
  # Crude metrics data #
  ######################
  metrics.input <- CrudeMetrics 
  
  ############################
  # Benchmarking model input #
  ############################
  benchmark.input <- benchModel
  
  ##########################
  # Comparison model input #
  ##########################
  comparison.input <- compModels

  #######################
  # Winkler scores data #
  #######################
  winklerScores <- winklerScoresInput 

#------------------------------------------------------------------------------#
# Combining the Winkler Scores with the Metrics --------------------------------
#------------------------------------------------------------------------------#
# About: This section combines the performance metrics and Winkler scores for  #
# manipulation throughout the rest of function.                                #
#------------------------------------------------------------------------------#

####################################################
# Removing the date column from the winkler scores #
####################################################
winklerFinal <- winklerScores %>%
  dplyr::select(-Date)

###########################################
# Renaming Date in the metrics data frame #
###########################################
metricsFinal <- metrics.input %>%
  dplyr::rename("Forecast Date" = "Date")

#################################
# Combining the two data frames #
#################################
allMetrics <- merge(winklerFinal, metricsFinal) %>%
  dplyr::select(-mean95PI, -`Winkler Score`, -CalibrationIndicator)


#------------------------------------------------------------------------------#
# Preparing the data for skill scores calculations -----------------------------
#------------------------------------------------------------------------------#
# About: This section either calculates the average metrics, and then prepares #
# the data for calculating skill scores below or jumps straight to preparing   #
# the data (i.e., crude metrics).                                              #
#------------------------------------------------------------------------------#

#######################
# For average metrics #
#######################
if(average.input){
  
  metricsForScores <- allMetrics %>%
    dplyr::group_by(Model, Location) %>% # Grouping by location and model
    dplyr::mutate(meanMSE = mean(meanMSE), # Avg. MSE
                  meanMAE = mean(meanMAE), # Avg. MAE
                  `Avg. Winkler` = mean(`Avg. Winkler`), # Avg. PI
                  meanWIS = mean(meanWIS)) %>% # Avg. WIS
    dplyr::filter(Model %in% c(benchmark.input, comparison.input)) %>% # Filtering to include the needed models
    dplyr::filter(Location %in% c(location.input)) %>% # Filtering to include the selected locations 
    tidyr::pivot_longer(-c(Location, Model, `Forecast Date`), names_to = "Metric", values_to = "Value") %>%
    tidyr::pivot_wider(names_from = "Model", values_from = "Value")
 
#####################
# For crude metrics #
#####################
}else{
  
  metricsForScores <- allMetrics %>%
    dplyr::filter(Model %in% c(benchmark.input, comparison.input)) %>% # Filtering to include the needed models
    dplyr::filter(Location %in% c(location.input)) %>% # Filtering to include the selected locations 
    tidyr::pivot_longer(-c(Location, Model, `Forecast Date`), names_to = "Metric", values_to = "Value") %>%
    tidyr::pivot_wider(names_from = "Model", values_from = "Value")
}

 
#------------------------------------------------------------------------------#
# Calculating skill scores -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the skill scores based on what the user chose #
# for the benchmark model and comparison models. Skills scores are calculated  #
# for MSE, MAE, WIS, and Winkler Scores.                                       #
#------------------------------------------------------------------------------#

########################
# Benchmark model data #
########################
benchMarkData <- metricsForScores[[benchmark.input]]

#################################
# Loop calculating skill scores #
#################################
for(i in 1:length(comparison.input)){
  
  # Indexed comparison model
  comparisonData <- metricsForScores[[comparison.input[i]]]
  
  # Calculating the skill score
  skillScore <- (benchMarkData - comparisonData)/benchMarkData
  
  # Adding the score to the main data
  metricsForScores[,(ncol(metricsForScores) + 1)] <- skillScore
  
  # Adding name for the new column
  names(metricsForScores)[ncol(metricsForScores)] <- paste0(benchmark.input, "-", comparison.input[i])
  
}

#------------------------------------------------------------------------------#
# Preparing the data for exporting ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the data for exporting. For example, it removes #
# unneeded variables and it is returned to the main dashboard.                 #
#------------------------------------------------------------------------------#

#############################################
# Selecting variables, and re-oriented data #
#############################################
finalSS <- metricsForScores[, c(1:3, (ncol(metricsForScores) - length(comparison.input) + 1):ncol(metricsForScores))] %>%
  dplyr::mutate(Metric = ifelse(Metric == "Avg. Winkler", "Winkler Score",
                                ifelse(Metric == "meanMSE", "MSE",
                                       ifelse(Metric == "meanMAE", "MAE", "WIS")))) %>%
  tidyr::pivot_longer(-c(Location, `Forecast Date`, Metric), names_to = "Comparison", values_to = "Value") %>%
  tidyr::pivot_wider(names_from = Metric, values_from = Value)


##################################
# Renaming and ordered variables #
##################################
finalSS <- separate(finalSS, Comparison, into = c("Benchmark Model", "Comparison Model"), sep = "-") %>%
  dplyr::select(Location, `Forecast Date`, `Benchmark Model`, `Comparison Model`, MSE, MAE, `Winkler Score`, WIS)

############################################################
# Removing the date column if working with average metrics #
############################################################
if(average.input){
  
  finalSS <- finalSS %>%
    dplyr::select(-`Forecast Date`)
  
}

######################
# Returning the data #
######################
return(finalSS)

}                                         

