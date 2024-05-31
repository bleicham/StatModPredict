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
skillScoresMain <- function(averageIndicator, locationsFilter, CrudeMetrics,
                              benchModel, compModels, winkler.input,
                            filterIndicator.input){
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to be used throughout the    #
# the rest of the code.                                                        #
#------------------------------------------------------------------------------#
  
  #######################
  # Winkler scores data #
  #######################
  winklerScores <- winkler.input 
  
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
  
  ####################
  # Filter Indicator #
  ####################
  filteringIndicator <- filterIndicator.input
  
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
    baseline <- c(unique(metrics.input$Model))
    
    # Comparison model(s) 
    comparison <- c(unique(metrics.input$Model))
    
    # Locations 
    locationInput <- c(unique(metrics.input$Location))
    
  ######################
  # Filtering the data #
  ######################
  }else{
    
    # Baseline model(s) 
    baseline <- benchmark.input
    
    # Comparison model(s) 
    comparison <- comparison.input
    
    # Locations 
    locationInput <- location.input
    
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
  avgWinklerScores <- winklerScores %>%
    dplyr::group_by(Location, Model) %>% # Grouping to calculate average metrics 
    dplyr::mutate(avgWinkler = mean(`Winkler Score`, na.rm = T)) %>% # Calculating the average Winkler Score
    dplyr::select(Model, Location, avgWinkler) %>% # Reordering the variables
    dplyr::distinct(Model, Location, .keep_all = T) # Removing repeat rows 
  
  #########################
  # Preparing the metrics #
  #########################
  averageMetrics <- metrics.input %>%
    dplyr::group_by(Location, Model) %>% # Grouping variables
    dplyr::mutate(`Avg. MSE` = mean(MSE, na.rm = T), # Average MAE
                  `Avg. MAE` = mean(MAE, na.rm = T), # Average MSE
                  `Avg. 95% PI` = mean(`95%PI`, na.rm = T), # Average 95% PI
                  `Avg. WIS` = mean(WIS, na.rm = T)) %>%
    dplyr::select(Location, Model, `Avg. MSE`, `Avg. MAE`, `Avg. 95% PI`, `Avg. WIS`) %>% # Needed variables
    dplyr::distinct(Location, Model, .keep_all = T) # Removing repeat rows 
  
  #########################################################
  # Combining the Winkler Scores with the average metrics #
  #########################################################
  averageMetricsALL <- merge(averageMetrics, avgWinklerScores)
  
#------------------------------------------------------------------------------#
# Preparing the crude metrics --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the crude metrics data set for later            #
# skill scores calculations.                                                   #
#------------------------------------------------------------------------------#
  
  ################################
  # Preparing the Winkler Scores #
  ################################
  crudeWinklerScores <- winklerScores %>%
      dplyr::select(Location, Model, `Forecast Date`, `Winkler Score`) 
    

  #################################
  # Cleaning up the crude metrics #
  #################################
  
  # Daily or weekly data
  if(nchar(metrics.input$Date[1]) > 4){
    
    crudeMetrics <- metrics.input %>%
      dplyr::mutate(Date = anytime::anydate(Date)) %>% # Changing date to a date format
      dplyr::rename("Forecast Date" = Date) # Renaming the date column 
  
  # Yearly or index data  
  }else{
    
    crudeMetrics <- metrics.input %>%
      dplyr::mutate(Date = as.numeric(Date)) %>% # Changing date to a date format
      dplyr::rename("Forecast Date" = Date) # Renaming the date column 
    
  }
    
  #######################################################
  # Combining the Winkler Scores with the crude metrics #
  #######################################################
  crudeMetricsALL <- merge(crudeMetrics, crudeWinklerScores)
  
  
#------------------------------------------------------------------------------#
# Determining which metric data set to use for the skill scores ----------------
#------------------------------------------------------------------------------#
# About: This section determines which metrics to use to calculate the skill   #
# scores: average or crude.                                                    #
#------------------------------------------------------------------------------#
  
  #############################
  # Using the average metrics #
  #############################
  if(average.input){
    
    # Metrics to use 
    finalMetrics <- averageMetricsALL
    
    # Wide-to-long data
    longMetrics <- pivot_longer(finalMetrics, -c(Model, Location), names_to = "Metric", values_to = "Value")
    
  ###########################
  # Using the crude metrics #
  ###########################
  }else{
    
    # Metrics to use 
    finalMetrics <- crudeMetricsALL
    
    # Wide-to-long data
    longMetrics <- pivot_longer(finalMetrics, -c(Model, Location, `Forecast Date`), names_to = "Metric", values_to = "Value")
    
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
  if(average.input){
    
    # Empty data frame that will be exported later 
    skillScoresExport <- data.frame(Location = NULL,
                                    Metric = NULL,
                                    Baseline = NULL,
                                    Comparison = NULL,
                                    `Skill Scores` = NULL)
  # For crude metrics   
  }else{
    
    # Empty data frame that will be exported later 
    skillScoresExport <- data.frame(Location = NULL,
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
      if(average.input){
        
        # Keeping 'distinct' rows 
        mergedFinal <- merged %>%
          dplyr::distinct(Location, Metric, Baseline, Comparison, .keep_all = T)
        
      ###################################################
      # Removing repeat rows if working with crude data #
      ###################################################
      }else{
        
        # Keeping 'distinct' rows 
        mergedFinal <- merged %>%
          dplyr::distinct(Location, `Forecast Date`, Metric, Baseline, Comparison, .keep_all = T)
        
      }
      
      ################################
      # Calculating the skill scores #
      ################################
      skillScoresTEMP <- mergedFinal %>%
        dplyr::mutate(`Skill Scores` = round((baseValue - compareValue)/baseValue, 2)) %>% # Calculating Skill Scores
        dplyr::filter(Metric != "95%PI") # Removing 95% PI 
      
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
# About: This section filters the skill scores based on location.              #
#------------------------------------------------------------------------------#
  
  #########################################
  # Filtering and cleaning the final data #
  #########################################
  finalData <- skillScoresExport %>%
    dplyr::filter(Location %in% c(locationInput)) 
  
  # Removing empty lines
  finalData <- na.omit(finalData)
  
  ############################
  # Returning the final data #
  ############################
  return(finalData)  
  
}                                         

