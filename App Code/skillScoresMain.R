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
# calculated for the forecast or fit metrics.                                  #
#                                                                              #
# Skill scores are a calculation that allows up to see how much one model      #
# (i.e., comparison) improves over a baseline model. The equation is given     #
# by:                                                                          #
#                                                                              #
#                  (Baseline - Comparison)/Baseline * 100                      #
#                                                                              #
#                                                                              #
# Additional information can be found in:                                      #
# https://otexts.com/fpp3/distaccuracy.html.                                   #
#                                                                              #                                                                          
#------------------------------------------------------------------------------#
#                        Author: Amanda Bleichrodt                             #
#------------------------------------------------------------------------------#
skillScoresMain <- function(averageIndicator, CrudeMetrics, winkler.input,
                            metricPage.input){
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the function inputs to be used throughout the    #
# the rest of the code.                                                        #
#------------------------------------------------------------------------------#
  
  #######################
  # Winkler scores data #
  #######################
  winklerScores <<- winkler.input 
  
  #############################
  # Average Metrics Indicator #
  #############################
  average.input <<- averageIndicator 
  
  ######################
  # Crude metrics data #
  ######################
  metrics.input <<- CrudeMetrics 

  ##################
  # Metric to show #
  ##################
  metricToShow <<- metricPage.input
  
#------------------------------------------------------------------------------#
# Combining the Winkler Scores and Metrics Data --------------------------------
#------------------------------------------------------------------------------#
# About: This section compares the metrics data and the Winkler scores.        #
#------------------------------------------------------------------------------#
  
  ##################################
  # Editing the Winkler Score Date #
  ##################################
  winklerScores <- winklerScores %>%
    dplyr::rename("Date" = `Forecast Date`) %>%
    dplyr::select(Location, Model, Date, Calibration, `Winkler Score`) 
  
  #########################################
  # Fixing the dates in the metrics input #
  #########################################
  if(nchar(metrics.input$Date)[1] == 10){
    
    crudeDateFix <- metrics.input %>%
      dplyr::mutate(Date = anytime::anydate(Date))
    
  }else{
    
    crudeDateFix <- metrics.input %>%
      dplyr::mutate(Date = as.numeric(Date))
    
  }
  
  ###############################
  # Merging the two data frames #
  ###############################
  crudeDataMerged <- merge(crudeDateFix, winklerScores, by = c("Location", "Model", "Date", "Calibration"), all = T)
  
  ########################
  # Cleaning up the data #
  ########################
  
  # Possible variable names
  possibleNames <- c("Location", "Model", "Calibration", "Date", "MSE", "MAE", "WIS", "PI", "AICc", "AIC", "BIC", "Winkler Score")
  
  # Selecting the needed variables 
  crudeDataFinal <- crudeDataMerged %>%
    dplyr::select(one_of(possibleNames))

#------------------------------------------------------------------------------#
# Creating the Average metrics data --------------------------------------------
#------------------------------------------------------------------------------#
# About: Users also have the option to calculate skill scores based upon the   #
# average metrics value. Therefore, this section calculates the average        #
# metrics over the forecast dates.                                             #
#------------------------------------------------------------------------------#
  
  # Data frame to fill
  averageDataFinal <- data.frame(Location = crudeDataFinal$Location,
                                 Model = crudeDataFinal$Model,
                                 Calibration = crudeDataFinal$Calibration,
                                 Date = crudeDataFinal$Date)

  #########################################
  # Loop to calculate the average metrics #
  #########################################
  for(i in 5:ncol(crudeDataFinal)){
    
    # Creating a temp file with crude data 
    tempData <- data.frame(Location = crudeDataFinal$Location,
                           Model = crudeDataFinal$Model,
                           Calibration = crudeDataFinal$Calibration,
                           Date = crudeDataFinal$Date,
                           tempCrude = crudeDataFinal[[i]])
    
    # Temp file with the average data 
    avgData <- tempData %>%
      dplyr::group_by(Location, Model, Calibration) %>%
      dplyr::mutate(tempAvg = mean(tempCrude, na.rm = T)) 
    
    # Naming the average data column 
    colnames(avgData)[6] <- paste0("Avg. ", colnames(crudeDataFinal[i]))
    
    # Adding the new column to the final data frame 
    averageDataFinal[,i] <- avgData[6]
    
  }
  

#------------------------------------------------------------------------------#
# Determining which metrics to use ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if we should be using the crude metrics for   #
# skill scores calculations or the average metrics. The user indicates this    #
# in the dashboard.                                                            #
#------------------------------------------------------------------------------#
  
  ########################
  # Using the crude data #
  ########################
  if(average.input){
    
    finalData <- averageDataFinal
    
  }else{
    
    finalData <- crudeDataFinal
    
  }

#------------------------------------------------------------------------------#
# Calculating the Skill Scores -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the skill scores for every possible           #
# combination of models. Skills scores are calculated as the                   #
# baseline - comparison/baseline model * 100. The scores will be filtered at   #
# a later step.                                                                #
#------------------------------------------------------------------------------#
  
  # Possible model choices 
  modelChoices <- unique(finalData$Model)
  
  # Data to fill
  winklerData <- data.frame(Location = NULL,
                            Calibration = NULL,
                            Date = NULL,
                            Metric = NULL,
                            `Baseline Model` = NULL, 
                            `Comparison Model` = NULL, 
                            `Skill Score`= NULL)
  
  #########################
  # Comparison model loop #
  #########################
  for(i in 1:length(modelChoices)){
    
    #######################
    # Baseline model loop #
    #######################
    for(g in 1:length(modelChoices)){
      
      #######################################
      # Checking if the models are the same #
      #######################################
      if(modelChoices[i] == modelChoices[g]){
        
        # Skipping to next loop iteration 
        next
        
      } # End of 'if' for model checker
      
      #######################################
      # Preparing the data for calculations #
      #######################################
      tempFilter <- finalData %>%
        dplyr::filter(Model %in% c(modelChoices[i], modelChoices[g])) %>%
        tidyr::pivot_longer(cols = -c(Location, Calibration, Date, Model), names_to = "Metric", values_to = "Score") %>%
        tidyr::pivot_wider(names_from = Model, values_from = Score) %>%
        na.omit() 
      
      ################################
      # Calculating the skill scores #
      ################################
      skillScoresTemp <- tempFilter %>%
        dplyr::mutate(`Skill Score` = round(((tempFilter[[5]] - tempFilter[[6]])/tempFilter[[5]])*100, 2),
                      `Baseline Model` = modelChoices[g],
                      `Comparison Model` = modelChoices[i]) %>%
        dplyr::select(Location, Calibration, Date, Metric, `Baseline Model`, `Comparison Model`, `Skill Score`)
      
     
      #################################################
      # Creating the final data with all skill scores #
      #################################################
      winklerData <- rbind(skillScoresTemp, winklerData)
      
    }
  }
  
#------------------------------------------------------------------------------#
# Preparing the final data for export ------------------------------------------
#------------------------------------------------------------------------------#
# About: This sections prepares the data set for final export. It determines   #
# if the date column should be included.                                       #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Determining if the date column should show #
  ##############################################
  if(average.input){
    
    toExport <- winklerData %>%
      dplyr::select(-Date)
    
  }else{
    
    toExport <- winklerData 
    
  }
  

  ############################
  # Returning the final data #
  ############################
  return(toExport)  
  
}                                         

