#------------------------------------------------------------------------------#
#                                                                              #
#                Calculating the Skill Scores - Model Comparison               #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function calculates the skill scores for both the dashboard models and  #
# other models read into the dashboard. Skill scores provide a measure of      #
# comparison between two models, thus allowing the user to determine the       #
# degree of improvement one model (comparison) may have over a baseline model. #
# The formula for skill scores was obtained from [1].                          #
#                                                                              #
# [1] Hyndman, R.J. & Athanasopoulos, G. 2018 Forecasting: principles and      #
#     practice. OTexts.                                                        #
#------------------------------------------------------------------------------#
#                        Author: Amanda Bleichrodt                             #
#------------------------------------------------------------------------------#
skillScoresOther <- function(winkler.input, averageMetrics.input, 
                             crudeMetrics.input, averageIndicator.input, 
                             baseline.input, compare.input,  filter.input,
                             performance.input, location.input, 
                             calibration.input, horizon.input) {
  
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to be used throughout the    #
# the rest of the code.                                                        #
#------------------------------------------------------------------------------#
  
  #################################
  # Reading in the Winkler Scores #
  #################################
  winklerScores <- winkler.input
  
  ##################################
  # Reading in the average metrics #
  ##################################
  averageMetrics <- averageMetrics.input
  
  ################################
  # Reading in the crude metrics #
  ################################
  crudeMetrics <- crudeMetrics.input
  
  #################################
  # Indicator for average metrics #
  #################################
  averageMetricIndicator <- averageIndicator.input
  
  #####################
  # Baseline model(s) #
  #####################
  baseline <- baseline.input
  
  #######################
  # Comparison model(s) #
  #######################
  comparison <- compare.input
  
  ###################
  # Filtering input #
  ###################
  filteringIndicator <- filter.input
  
  #############################
  # Performance metrics input #
  #############################
  performanceMetrics <- performance.input
  
  #############
  # Locations #
  #############
  locationInput <- location.input
  
  ######################
  # Calibration period #
  ######################
  calibrationInput <- calibration.input
  
  ##################
  # Horizon length #
  ##################
  horizonInput <- horizon.input
  
#------------------------------------------------------------------------------#
# Determining when to use the specified filtering ------------------------------
#------------------------------------------------------------------------------#
# About: Based upon the filtering indicator, this section determines if the    #
# metrics should be filtered or not.                                           #
#------------------------------------------------------------------------------#
  
  ##########################
  # Not filtering the data #
  ##########################
  if(filteringIndicator == 0){
    
    # Baseline model(s) 
    baseline <- c(unique(crudeMetrics$Model))
    
    # Comparison model(s) 
    comparison <- c(unique(crudeMetrics$Model))
    
    # Performance metrics input 
    performanceMetrics <- c(unique(crudeMetrics$`Performance Metric Type`))
    
    # Locations 
    locationInput <- c(unique(crudeMetrics$Location))
    
    # Calibration period 
    calibrationInput <- c(unique(crudeMetrics$Calibration))
    
    # Horizon length 
    horizonInput <- c(unique(crudeMetrics$Horizon))
    
  ######################
  # Filtering the data #
  ######################
  }else{
    
    # Baseline model(s) 
    baseline <- baseline
    
    # Comparison model(s) 
    comparison <- comparison
    
    # Performance metrics input 
    performanceMetrics <- performanceMetrics
    
    # Locations 
    locationInput <- locationInput
    
    # Calibration period 
    calibrationInput <- calibrationInput
    
    # Horizon length 
    horizonInput <- horizonInput
    
  }
  

#------------------------------------------------------------------------------#
# Preparing the average metrics ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the average metrics data set for later          #
# skill scores calculations.                                                   #
#------------------------------------------------------------------------------#

  ################################
  # Preparing the Winkler Scores #
  ################################
  if(!is.null(winklerScores)){
    
    avgWinklerScores <- winklerScores %>%
      dplyr::group_by(Location, Model, Calibration, Horizon) %>% # Grouping to calculate average metrics 
      dplyr::mutate(avgWinkler = mean(`Winkler Score`, na.rm = T)) %>% # Calculating the average Winkler Score
      dplyr::select(`Performance Metric Type`, Model, Location, Calibration, Horizon, avgWinkler) # Reordering the variables
    
  }
  
  ###################################
  # Cleaning up the average metrics #
  ###################################
  averageMetrics <- averageMetrics %>%
    dplyr::mutate(Calibration = as.numeric(Calibration), # Changing the calibration to numeric 
                  Horizon = as.numeric(Horizon)) # Changing the horizon to numeric 
  
  #########################################################
  # Combining the Winkler Scores with the average metrics #
  #########################################################
  if(!is.null(winklerScores)){
    
    averageMetricsALL <- merge(averageMetrics, avgWinklerScores)
    
  }else{
    
    averageMetricsALL <- averageMetrics
    
  }
  
#------------------------------------------------------------------------------#
# Preparing the crude metrics --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the crude metrics data set for later            #
# skill scores calculations.                                                   #
#------------------------------------------------------------------------------#
  
  ################################
  # Preparing the Winkler Scores #
  ################################
  if(!is.null(winklerScores)){
    
    crudeWinklerScores <- winklerScores %>%
      dplyr::select(`Performance Metric Type`, Model, Location, Calibration, Horizon, `Forecast Date`, `Winkler Score`) %>% # Reordering the variables 
      dplyr::rename('Date' = `Forecast Date`)
    
  }
  
  #################################
  # Cleaning up the crude metrics #
  #################################
  crudeMetrics <- crudeMetrics %>%
    dplyr::mutate(Calibration = as.numeric(Calibration), # Changing the calibration to numeric 
                  Horizon = as.numeric(Horizon)) # Changing the horizon to numeric 
  
  #######################################################
  # Combining the Winkler Scores with the crude metrics #
  #######################################################
  if(!is.null(winklerScores)){
    
    crudeMetricsALL <- merge(crudeMetrics, crudeWinklerScores)
     
  }else{
    
    crudeMetricsALL <- crudeMetrics
  }
  
#------------------------------------------------------------------------------#
# Determining which metric data set to use for the skill scores ----------------
#------------------------------------------------------------------------------#
# About: This section determines which metrics to use to calculate the skill   #
# scores: average or crude.                                                    #
#------------------------------------------------------------------------------#
  
  #############################
  # Using the average metrics #
  #############################
  if(averageMetricIndicator){
    
    # Metrics to use 
    finalMetrics <- averageMetricsALL
    
    # Wide-to-long data
    longMetrics <- pivot_longer(finalMetrics, -c(`Performance Metric Type`, Model, Location, Calibration, Horizon), names_to = "Metric", values_to = "Value")
    
  ###########################
  # Using the crude metrics #
  ###########################
  }else{
    
    # Metrics to use 
    finalMetrics <- crudeMetricsALL
    
    # Wide-to-long data
    longMetrics <- pivot_longer(finalMetrics, -c(`Performance Metric Type`, Model, Location, Calibration, Horizon, Date), names_to = "Metric", values_to = "Value")
    
  }
  
#------------------------------------------------------------------------------#
# Calculating the skill scores -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the skill scores based upon the baseline and  #
# comparison models selected by the user.                                      #
#------------------------------------------------------------------------------#
  
  ##################################################
  # Preparing the empty data sets for skill scores #
  ##################################################
  if(averageMetricIndicator){
    
    # Empty data frame that will be exported later 
    skillScoresExport <- data.frame(`Performance Metric Type` = NULL, 
                                    Location = NULL,
                                    Calibration = NULL, 
                                    Horizon = NULL,
                                    Metric = NULL,
                                    Baseline = NULL,
                                    Comparison = NULL,
                                    `Skill Scores` = NULL)
  # For crude metrics   
  }else{
    
    # Empty data frame that will be exported later 
    skillScoresExport <- data.frame(`Performance Metric Type` = NULL, 
                                    Location = NULL,
                                    Calibration = NULL, 
                                    Horizon = NULL,
                                    Metric = NULL,
                                    Baseline = NULL,
                                    Comparison = NULL,
                                    `Skill Scores` = NULL,
                                    Date = NULL)
    
  }
  
  ###########################
  # Loop for baseline model #
  ###########################
  for(i in 1:length(baseline)){
    
    # Indexed baseline model
    indexBaseline <- baseline[i]
    
    # Baseline data
    baselineData <- longMetrics %>%
      dplyr::filter(Model == indexBaseline) %>% # Filtering for baseline data 
      dplyr::rename("Baseline" = Model, # Relabeling the baseline model
                    "baseValue" = Value) # Relabeling the value column 
      
    
    #############################
    # Loop for comparison model #
    #############################
    for(j in 1:length(comparison)){
      
      # Indexed comparsion model
      indexCompare <- comparison[j]
      
      # Compare data
      compareData <- longMetrics %>% 
        dplyr::filter(Model == indexCompare) %>% # Filtering for comparison data 
        dplyr::rename("Comparison" = Model, # Relabeling the comparison model
                      "compareValue" = Value) # Relabeling the value column 
      
      ##############################################################
      # Determining if baseline and comparison models are the same #
      ##############################################################
      if(indexBaseline == indexCompare){
        
        # Skipping to the next loop iteration 
        next
        
      } # End of if statement checking if models match
      
      #############################
      # Merging the two data sets #
      #############################
      merged <- merge(baselineData, compareData)
      
      #####################################################
      # Removing repeat rows if working with average data #
      #####################################################
      if(averageMetricIndicator){
        
        # Keeping 'distinct' rows 
        mergedFinal <- merged %>%
          dplyr::distinct(`Performance Metric Type`, Location, Calibration, Horizon, Metric, Baseline, Comparison, .keep_all = T)
        
      ###################################################
      # Removing repeat rows if working with crude data #
      ###################################################
      }else{
        
        # Keeping 'distinct' rows 
        mergedFinal <- merged %>%
          dplyr::distinct(`Performance Metric Type`, Location, Calibration, Date, Horizon, Metric, Baseline, Comparison, .keep_all = T)
        
      }
      
      ################################
      # Calculating the skill scores #
      ################################
      skillScoresTEMP <- mergedFinal %>%
        dplyr::mutate(`Skill Scores` = round((baseValue - compareValue)/baseValue, 2)) %>% # Calculating Skill Scores
        dplyr::filter(Metric != "Avg. Coverage.95.PI") # Removing 95% PI 
      
      #######################
      # Finalizing the data #
      #######################
      formatSkillScores <- skillScoresTEMP %>%
        dplyr::select(-baseValue, -compareValue) %>% # Removing not needed columns 
        dplyr::mutate(Metric = ifelse(Metric == "avgWinkler", "Avg. Winkler", Metric))
      
      ###############################
      # Merging with the final data #
      ###############################
      skillScoresExport <- rbind(skillScoresExport, formatSkillScores)
      
    }
  }
  
#------------------------------------------------------------------------------#
# Filtering the skill scores ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the skill scores based on performance metrics,   #
# location, calibration, horizon, and metric.                                  #
#------------------------------------------------------------------------------#
  
  #########################################
  # Filtering and cleaning the final data #
  #########################################
  finalData <- skillScoresExport %>%
    dplyr::filter(`Performance Metric Type` %in% c(performanceMetrics),
                  Location %in% c(locationInput),
                  Calibration %in% c(calibrationInput),
                  Horizon %in% c(horizonInput)) 
  
  # Removing empty lines
  finalData <- na.omit(finalData)

  ############################
  # Returning the final data #
  ############################
  return(finalData)
  
} # End of function 
    
    
    
    
    