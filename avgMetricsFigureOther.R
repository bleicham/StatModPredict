#------------------------------------------------------------------------------#
#                                                                              #
#                 Tile Plots for Average Metrics (Model Comparison)            #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This section plots the average metrics calculated from the dashboard  #
# models, and from the performance metrics read into the dashboard. It plots   #
# multiple panels (grouped by horizon and calibration period), each having     #
# individual figures for each metric. The locations of interest are on the     #
# y-axis, and the models on the x-axis. Users have the option to customize     #
# colors, and if the plot is shown in log-10 or normal scale.                  #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
avgMetricsFigureOther <- function(avgData.input, scale.y.input, lowColor.input,
                                  highColor.input, outlineColor.input, textColor.input){
  
#------------------------------------------------------------------------------#
# Renaming the input data ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the inputs to the function, and creates the      #
# empty list names to fill in with figures later on.                           #
#------------------------------------------------------------------------------#
  
  #######################
  # Average metric data #
  #######################
  avgMetric.input <- avgData.input
  
  #######################
  # Scale of the y-axis #
  #######################
  scaleY <- scale.y.input
  
  #############
  # Low color #
  #############
  lowColor <- lowColor.input
  
  ##############
  # High color #
  ##############
  highColor <- highColor.input
  
  #################
  # Outline color #
  #################
  outlineColor <- outlineColor.input
  
  ##############
  # Text color #
  ##############
  textColor <- textColor.input
  
  #######################################
  # Empty list for the panel of metrics #
  #######################################
  panelFigure <- list()
  
  ##############################
  # Empty list for all figures #
  ##############################
  finalList <- NULL
  
  
#------------------------------------------------------------------------------#
# Switching data to long format ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section switches the data from wide to long format, where the    #
# long component is the metric and their values.                               #
#------------------------------------------------------------------------------#
  
  #############
  # Long data #
  #############
  data <- pivot_longer(data = avgMetric.input, -c(`Performance Metric Type`, Model, Location, Calibration, Horizon), names_to = "Metric", values_to = "Value") %>%
    dplyr::mutate(Value = ifelse(Value == "Inf", NA, Value))
  
#------------------------------------------------------------------------------#
# Full alphabet to pull letters for the figure titles --------------------------
#------------------------------------------------------------------------------#
# About: This section creates the vector containing the full alphabet. This is #
# used when labeling the panels of the metrics. As an unknown number of        #
# metrics will be entered, the full alphabet will be available.                #
#------------------------------------------------------------------------------#
  
  #######################
  # Vector with letters #
  #######################
  alphabetLabels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
                      "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
                      "W", "X", "Y", "Z")
  
  
#------------------------------------------------------------------------------#
# Creating lists for loops -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the lists for the calibration, performance type, #
# and horizon loops.                                                           #
#------------------------------------------------------------------------------#
  
  ####################
  # Calibration loop #
  ####################
  calibrationList <- c(unique(data$Calibration))
  
  #########################
  # Performance Type loop #
  #########################
  performanceList <- c(unique(data$`Performance Metric Type`))
  
  ################
  # Horizon loop #
  ################
  horizonLoop <- c(unique(data$Horizon))
  
#------------------------------------------------------------------------------#
# Setting up the loops ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the loops for calibration periods,               #
# performance types, and horizons. From this list, panels of graphs will be    #
# created.                                                                     #
#------------------------------------------------------------------------------#
  
  #################################
  # Creating the calibration loop #
  #################################
  for(b in 1:length(calibrationList)){
    
    ######################################
    # Creating the performance type loop #
    ######################################
    for(c in 1:length(performanceList)){
      
      #############################
      # Creating the horizon loop #
      #############################
      for(d in 1:length(horizonLoop)){
  
#------------------------------------------------------------------------------#
# Preparing the data to be used for a single panel -----------------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtered data to be used for creating each   #
# of the panels. It is filtered by the indexes of the above loops.             #
#------------------------------------------------------------------------------#
        
    #################################################
    # Filtering the data based on the loop location #
    #################################################
    dataForGraph <- data %>%
      dplyr::filter(Calibration == calibrationList[b],
                    `Performance Metric Type` == performanceList[c],
                    Horizon == horizonLoop[d])
        
    # Skipping to next loop iteration if `dataForGraph` is empty
    if(nrow(dataForGraph) == 0){
          
      next 
          
    }
    
#------------------------------------------------------------------------------#
# Adjusting the scale of the y-axis --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adjusts the scale of the y-axis based upon the user's    #
# choice or lack of choice in the main dashboard. The default setting is the   #
# is the original scale. However, multiple options are available.              #
#------------------------------------------------------------------------------#
    
    ###########################
    # No data transformations #
    ###########################
    if(is.null(scaleY) || any(scaleY %in% c("None"))){
      
      # Determining the min, max, and breaks 
      dataForGraph <- dataForGraph %>%
        dplyr::mutate(metricVar = Value) %>%
        na.omit() %>%
        dplyr::group_by(Metric) %>%
        dplyr::mutate(letterIndex = cur_group_id(),
                      letterLabel = paste0(alphabetLabels[letterIndex]),
                      metricLabel = "")
      
    ########################
    # Log-10 Transformation #
    #########################
    }else{
      
      # Determining the min, max, and breaks 
      dataForGraph <- dataForGraph %>%
        dplyr::mutate(metricVar = ifelse(Metric %in% c(scaleY), log10(Value + 1), Value)) %>%
        na.omit() %>%
        dplyr::group_by(Metric) %>%
        dplyr::mutate(letterIndex = cur_group_id(),
                      letterLabel = paste0(alphabetLabels[letterIndex]),
                      metricLabel = ifelse(Metric %in% c(scaleY), "(Log10)", ""))
      
    }
    
#------------------------------------------------------------------------------#
# Setting up to create the individual figures ----------------------------------
#------------------------------------------------------------------------------#
# About: This section sets up the data and other formatting needed to create   #
# the plots for each of the metrics.                                           #
#------------------------------------------------------------------------------#
    
  ###################################
  # Empty list to fill with figures #
  ###################################
  listOfFigures <- list()
    
  #####################################################
  # Looping through letter to create individual plots #
  #####################################################
    
  # Creating a unique list of letters to loop through 
  letterUnique <- sort(unique(dataForGraph$letterLabel))
    
  # Looping through letters 
  for(i in 1:length(letterUnique)){
      
    ###################################################
    # Filtering the data for the indexed letter label #
    ###################################################
      dataFilter <- dataForGraph %>%
        dplyr::filter(letterLabel == letterUnique[i]) 
      
      
#------------------------------------------------------------------------------#
# Plotting the data ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the individual plots based on the options above. #
#------------------------------------------------------------------------------#
      
      plot <- ggplot(data = dataFilter, aes(x = Location, y = Model, fill = metricVar)) +
        geom_tile(color = outlineColor) +
        scale_fill_gradient(low = lowColor, high = highColor, n.breaks = 7) +
        geom_text(data = dataFilter,aes(x = Location, y = Model,label=round(metricVar, 2)), color = textColor) +
        theme_bw() + 
        coord_fixed() +
        labs(fill = unique(dataFilter$metricLabel), 
             y = "",
             x = "") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
      
    
      ############################################
      # Adding the plots to the list for merging #
      ############################################
      listOfFigures[[i]] <- plot
      
      # Adding their labels
      names(listOfFigures)[i] <- paste0(performanceList[c], "-Calibration-", calibrationList[b], "-Horizon-", horizonLoop[d], "-Metric-", unique(dataFilter$Metric))
  }
  
  
#------------------------------------------------------------------------------#
# Adding the figures to the list(s) to be exported -----------------------------
#------------------------------------------------------------------------------#
# About: This section adds the figures and their labels to the lists to        #
# be exported.                                                                 #
#------------------------------------------------------------------------------#
  
  #######################################
  # Adding the plot to the horizon list #
  #######################################
  finalList <- c(listOfFigures, finalList)
  
  
      } # End of horizon loop 
      
    } # End of performance loop 
    
  } # End of calibration loop 
  

#------------------------------------------------------------------------------#
# Exporting the final list of figures ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the final list of individual plots to the main   #
# dashboard.                                                                   #
#------------------------------------------------------------------------------#
  
  ######################
  # Returning the list #
  ######################
  return(finalList)
  
} 




