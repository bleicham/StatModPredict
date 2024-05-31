#------------------------------------------------------------------------------#
#                                                                              #
#             Time series Panels for Crude Metrics (Model Comparison)          #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This section plots the crude metrics as time series plots, with the   #
# y-axis being the metric and the x-axis is the dates. Each panel will contain #
# the number of plots corresponding the number of different metrics.           #
# Each plot will have lines corresponding to each model and each location has  #
# its own panel. If their is only one forecast period selected, a bar chart    #
# will be plotted instead of a line.                                           #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
crudeMetricsFigureOther <- function(crudedataforfigure.input, date.Type.input, date.break.input,
                                    scale.y.input, start.y.input){


#------------------------------------------------------------------------------#
# Renaming the input data ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the inputs to the function, and creates the      #
# empty list names to fill in with figures later on.                           #
#------------------------------------------------------------------------------#

  #####################
  # Crude metric data #
  #####################
  crudeMetric.input <- crudedataforfigure.input
  
  ###################
  # Date type input #
  ###################
  date.Figure <- date.Type.input
  
  #########################
  # Number of date breaks #
  #########################
  dateBreaks <- date.break.input
  
  #######################
  # Scale of the y-axis #
  #######################
  scaleY <- scale.y.input
  
  ###################
  # Start of y-axis #
  ###################
  startYAxis <- start.y.input  
  
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
  data <- pivot_longer(data = crudeMetric.input, -c(`Performance Metric Type`, Model, Location, Calibration, Horizon, Date), names_to = "Metric", values_to = "Value") %>%
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
# About: This section creates the lists for the location, calibration,         #
# performance type, and horizon loops.                                         #
#------------------------------------------------------------------------------#

  #################
  # Location loop #
  #################
  locationList <- c(unique(data$Location))

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
# About: This section creates the loops for locations, calibration periods,    #
# performance types, and horizons. From this list, panels of graphs will be    #
# created.                                                                     #
#------------------------------------------------------------------------------#

  ###############################
  # Creating the locations loop #
  ###############################
  for(a in 1:length(locationList)){
    
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
    dplyr::filter(Location == locationList[a],
                  Calibration == calibrationList[b],
                  `Performance Metric Type` == performanceList[c],
                  Horizon == horizonLoop[d])
  
  # Skipping to next loop iteration if `dataForGraph` is empty
  if(nrow(dataForGraph) == 0){
    
    next 
    
  }

#------------------------------------------------------------------------------#
# Cleaning up, and preparing the dates -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the dates for individual figure. It allows      #
# users to select the number of breaks in the date labels.                     #
#------------------------------------------------------------------------------#    

  ################################################
  # Handling dates in the forecast files - Weeks #
  ################################################
  if(date.Figure %in% c("week")){
    
    # Dates on x-axis
    dataForGraph <- dataForGraph %>%
      mutate(Dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data
    
    # Breaks
    if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
      
      breaksLabel <- paste0(1, " week")
      
    }else{
      
      breaksLabel <- paste0(dateBreaks, " weeks")
      
    }
    
    # X-axis breaks 
    xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(dataForGraph$Dates)), max(anydate(dataForGraph$Dates)), by = breaksLabel))  # X-axis breaks
    
  ###############################################
  # Handling dates in the forecast files - Days #
  ###############################################
  }else if(date.Figure %in% c("day")){
  
    # Dates on x-axis
    dataForGraph <- dataForGraph %>%
      mutate(Dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data
    
    
    # Breaks
    if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
      
      breaksLabel <- paste0(1, " day")
      
    }else{
      
      breaksLabel <- paste0(dateBreaks, " days")
      
    }
    
    # X-axis breaks 
    xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(dataForGraph$Dates)), max(anydate(dataForGraph$Dates)), by = breaksLabel))  # X-axis breaks
  
  ##############################################################
  # Handling dates in the forecast files - years or time index #
  ##############################################################
  }else{
  
    # Dates on x-axis
    dataForGraph <- dataForGraph %>%
      mutate(Dates = as.numeric(Date)) # Changing years and time index to numeric 
    
    # Breaks
    if(dateBreaks < 1 || is.na(dateBreaks) || is.null(dateBreaks)){
      
      breaksLabel <- 1
      
    }else{
      
      breaksLabel <- as.numeric(dateBreaks)
      
    }
    
    # X-axis breaks
    xAxisBreaks <- scale_x_continuous(breaks = seq(min(dataForGraph$Dates, na.rm = T), max(dataForGraph$Dates, na.rm = T), by = breaksLabel))  # X-axis breaks
    
  }



#------------------------------------------------------------------------------#
# Adjusting the scale of the y-axis --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adjusts the scale of the y-axis based upon the user's    #
# choice or lack of choice in the main dashboard. The default setting is the   #
# is the original scale. However, multiple options are available.              #
#------------------------------------------------------------------------------#

  print(scaleY)
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
                    metricLabel = Metric, 
                    maxValue = max(metricVar, na.rm = T),
                    minValue = min(metricVar, na.rm = T),
                    breaks.graph = ifelse(maxValue < 0.50, 0.1,
                                          ifelse(maxValue < 1, 0.25, 
                                            ifelse(maxValue < 2, 0.5, 
                                                   ifelse(maxValue < 3, 0.75,
                                                          ifelse(maxValue/10 < 1, 1, floor(maxValue/6)))))))

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
                    metricLabel = ifelse(Metric %in% c(scaleY), paste0(Metric, " (Log10)"), Metric),
                    maxValue = max(metricVar, na.rm = T),
                    minValue = min(metricVar, na.rm = T),
                    breaks.graph = ifelse(maxValue < 0.50, 0.1,
                                          ifelse(maxValue < 1, 0.25, 
                                                 ifelse(maxValue < 4, 0.5, 
                                                        ifelse(maxValue/10 < 1, 1, floor(maxValue/6))))))
    
  }

  
#------------------------------------------------------------------------------#
# Creating the legend indicator ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a unique indicator for the number of levels in   #
# each metric. This will be used later to indicate if a legend should be       #
# created.                                                                     #
#------------------------------------------------------------------------------#
  
  ##########################
  # Creating the indicator #
  ##########################
  dataForGraph <- dataForGraph %>%
    dplyr::group_by(metricLabel) %>% # Grouping by metric 
    dplyr::mutate(UniqueCount = n()) %>% # Counting unique instances
    dplyr::ungroup() # Ungrouping by metric 
  
  #################
  # Max indicator #
  #################
  maxIndicatorLegend <- max(unique(dataForGraph$UniqueCount))
  
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
# Determining where to start the y-axis ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if the y-axis should start at zero or at the  #
# minimum value for each plot. This is controlled by the user, and the option  #
# selected applies to all panel plots.                                         #
#------------------------------------------------------------------------------#
   
   ###############################
   # Starting the y-axis at zero #
   ###############################
   if(startYAxis == "0" || is.null(startYAxis) || is.na(startYAxis)){
     
     # Start value
     dataFilter$start <- 0
     
   ################################################################
   # Runs if the user does not select to start the y-axis at zero #
   ################################################################
   }else{
     
     # Start value
     dataFilter$start <- dataFilter$minValue
     
   }
   
#------------------------------------------------------------------------------#
# Determining which axis should have labels ------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines which of the x-axis should have labels and    #
# tick labels.                                                                 #
#------------------------------------------------------------------------------#
   
   #############################################################
   # Determining how many letters to include in the bottom row #
   #############################################################
   
   # Pulling the last two letters if only four letters total 
   if(length(letterUnique) == 4){
     
    lastLetters <- letterUnique[(length(letterUnique)-1):length(letterUnique)]
    
   # Pulling the last three letters if more than four letters total 
   }else{
     
     lastLetters <- letterUnique[(length(letterUnique)-2):length(letterUnique)]
     
   }
   
   #####################################################
   # Checking if the indexed letter is in the last row #
   #####################################################
   if(letterUnique[i] %in% c(lastLetters)){
     
     # If only one date in data set
     if(length(unique(dataFilter$Dates)) == 1){
       
       
       xAxisLocation <- element_text()
     
     # If more than one date in the data set 
     }else{
       
       xAxisLocation <- element_text(angle = 90, vjust = 0.5, hjust=1)
       
     }
    
   # Removing x-axis ticks  
   }else{
     
     # X-axis missing
     xAxisLocation <- element_blank()
     
   }
   
#------------------------------------------------------------------------------#
# Determining when to remove legend --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines when to remove legends. In some cases, we     #
# may get duplicate legends, and this determines if it should be removed.      #
#------------------------------------------------------------------------------#
   
   ######################################################
   # If the indexed data has the maximum number of rows #
   ######################################################
   if(unique(dataFilter$UniqueCount) == maxIndicatorLegend){
     
     # Creating the legend 
     legendPosition <- NULL
     
   }else{
     
     # Removing the legend
     legendPosition <- "none"
     
   }
   
#------------------------------------------------------------------------------#
# Plotting the data ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the individual plots based on the options above. #
# If there is only one date, it returns bar plots. If there is more than one   #
# date it returns a line plot.                                                 #
#------------------------------------------------------------------------------#
   
   ######################################
   # Plot if only one date is available #
   ######################################
   if(length(unique(dataFilter$Dates)) == 1){
     
     plot <- ggplot(data = dataFilter, aes(x = Date, y = metricVar, fill = Model)) + # General parameters for plot
       geom_col(position = "dodge") + # Side-by-side bar chart
       scale_fill_brewer(palette = "Paired") + # Colors 
       xAxisBreaks + # Handling dates 
       scale_y_continuous(breaks = seq(0, unique(dataFilter$maxValue) + 2*unique(dataFilter$breaks.graph), by = unique(dataFilter$breaks.graph)), # Y-axis breaks
                          limits = c(0, unique(dataFilter$maxValue))) +  # Y-axis limits
       theme_classic() + # General theme 
       labs(x = "", # Removing x-axis label 
            y = unique(dataFilter$metricLabel), # Set y-axis label 
            title = bquote(bold(.(paste0(letterUnique[i]))))) + # Title letter
       theme(axis.text.x = xAxisLocation, # Formatting the dates
             legend.position = legendPosition) # Handling the legend 
     

     
   ###########################################
   # Plot if more than one date is available #
   ###########################################
   }else{
     
    plot <- ggplot(data = dataFilter, aes(x = Date, y = metricVar, color = Model, linetype = Model)) + # General parameters for plot
      geom_line(size = 1) + # Line plot
      scale_color_brewer(palette = "Paired") + # Colors 
      xAxisBreaks + # Handling dates 
      scale_y_continuous(breaks = seq(unique(dataFilter$start), unique(dataFilter$maxValue) + 3*unique(dataFilter$breaks.graph), by = unique(dataFilter$breaks.graph)), # Y-axis breaks
                         limits = c(unique(dataFilter$start), unique(dataFilter$maxValue))) +  # Y-axis limits
      theme_classic() + # General theme 
      labs(x = "", # Removing x-axis label 
           y = unique(dataFilter$metricLabel), # Set y-axis label 
           title = bquote(bold(.(paste0(letterUnique[i]))))) + # Title letter
      theme(axis.text.x = xAxisLocation, # Formatting the dates
            legend.position = legendPosition) # Handling the legend 
     
   }

   ############################################
   # Adding the plots to the list for merging #
   ############################################
   listOfFigures[[i]] <- plot
  
 }
 
#------------------------------------------------------------------------------#
# Creating the panel figures ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the panel figures that will be later outputted   #
# to the main dashboard.                                                       #
#------------------------------------------------------------------------------#
 
 #####################################
 # Determining the number of columns #
 #####################################
 if(length(letterUnique) == 4){
   
   columnsPlot <- 2
   
 }else{
   
   columnsPlot <- 3
   
 }
 
 ###############################
 # Creating the combined plots #
 ###############################
 panelFigure <- list(wrap_plots(plotlist = listOfFigures, ncol = columnsPlot, guides = 'collect'))
   
#------------------------------------------------------------------------------#
# Adding the figures to the list(s) to be exported -----------------------------
#------------------------------------------------------------------------------#
# About: This section adds the panel figures and their labels to the lists to  #
# be exported. Due to the nested nature of the loops, the plot will be added   #
# to the first list, and then lists will be added to lists.                    #
#------------------------------------------------------------------------------#
 
 ##############################################
 # Creating the label for the list of figures #
 ##############################################
 names(panelFigure)[1] <- paste0(performanceList[c], "-", locationList[a], "-Calibration-", calibrationList[b], "-Horizon-", horizonLoop[d])
 
 #######################################
 # Adding the plot to the horizon list #
 #######################################
 finalList <- c(panelFigure, finalList)
 
 
        } # End of horizon loop 
        
      } # End of performance loop 
      
    } # End of calibration loop 
    
  } # End of location loop 
 
  
#------------------------------------------------------------------------------#
# Exporting the final list of panel plots --------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the final list of panel plots to the main        #
# dashboard.                                                                   #
#------------------------------------------------------------------------------#
  
  ######################
  # Returning the list #
  ######################
  return(finalList)
  
} 
 


  
