#------------------------------------------------------------------------------#
#                                                                              #
#                          Plotting Panel Figures                              #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file reads in the formatted forecasts created during an earlier step in #
# the toolbox and the date type of the orignal data. It then merges all of the #
# data together and forms panel plots for the avaliable data.                  #
#------------------------------------------------------------------------------#
#                         By: Amanda Bleichrodt                                #
#------------------------------------------------------------------------------#
panel.forecast.figures <- function(formatted.forecast.input, date.type.input){
  
#------------------------------------------------------------------------------#
# Reading in inputs from the main script ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names for manipulation throughout the remainder of the code.                 #
#------------------------------------------------------------------------------#
  
  ###########################
  # Formatted Forecast list #
  ###########################
  formatted.forecast.Figure <- formatted.forecast.input
  
  ############################################
  # Reading in the other formatted forecasts #
  ############################################
  other.formatted.forecasts <- formatted.forecast.Other.input
  
  #############
  # Date type #
  #############
  date.Figure <- data.type.input
  
  ###########################
  # Creating the empty list #
  ###########################
  figureListLocationLoop <- list()
  
  #####################################
  # Creating the empty list for final #
  #####################################
  finalList <- list()
  
  #################################
  # Creating the empty data frame #
  #################################
  allData <- data.frame()
  
  ####################################
  # Creating and empty list for data #
  ####################################
  listData <- list()
  
  
#------------------------------------------------------------------------------#
# Merging data frames ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section merges together the other files read into the dashboard  #
# and the existing models run within the dashboard. Pertinant information is   #
# pulled from the files, and added to the data for ease of use later on.       #
#------------------------------------------------------------------------------#
  
  # Creating the empty data frame for merging - Other Forecasts
  allOtherForecasts <- NA
  
  #####################################
  # Looping through the read-in files #
  #####################################
  for(i in 1:length(other.formatted.forecasts)){
    
    # Determining the name of the indexed forecast
    indexedOtherName <- names(other.formatted.forecasts[i])
    
    ##########################################
    # Pulling information from the file name #
    ##########################################
    
    # Model Abbr. 
    modelAbbr <- qdapRegex::ex_between(indexedOtherName, "", "-")[[1]][1]
    
    # Sub-setting location/group name 
    subModelAbbr <- qdapRegex::ex_between(indexedOtherName, paste0(modelAbbr, "-"), "-")[[1]][1]
    
    # Horizon
    horizon <- qdapRegex::ex_between(indexedOtherName, "horizon-", "-")[[1]][1]
    
    # Calibration period 
    calibration <- qdapRegex::ex_between(indexedOtherName, "calibration-", "-")[[1]][1]
    
    # Location
    location <- qdapRegex::ex_between(indexedOtherName, paste0("calibration-", calibration, "-"), "-")[[1]][1]
    
    # Determining the forecast period from the name
    forecastPeriod <- qdapRegex::ex_between(indexedOtherName, paste0(location, "-"), ".csv")[[1]][1]
    
    # Creating the full model name
    if(subModelAbbr == ""){
      
      model <- modelAbbr
      
    }else{
      
      model <- paste0(modelAbbr, "-", subModelAbbr)
      
    }
    
    ############################################
    # Adding the information to the data frame #
    ############################################
    data.temp <- other.formatted.forecasts[[i]] %>%
      dplyr::mutate(Model = model,
                    Horizon = horizon,
                    Calibration = calibration,
                    Location = location,
                    ForecastDate = forecastPeriod)
    
    #################################################
    # Merging with the rest of the read-in forcasts #
    #################################################
    allOtherForecasts <- rbind(allOtherForecasts, data.temp)
    
  }
  
  # Creating the empty data frame for merging - Toolbox forecasts
  allToolboxForecasts <- NA
  

for(i in 1:length(formatted.forecast.Figure)){
    
    # Determining the name of the indexed forecast
    nameIndex <- names(formatted.forecast.Figure[i])
    
    # Model type 
    model.Figure <- strsplit(nameIndex, "[-]")[[1]][1]
    
    # Sub-setting location/group name 
    locationGroupName <- strsplit(nameIndex, "[-]")[[1]][2]
    
    # Forecast period for weekly or daily data 
    if(date.Figure %in% c("week", "day")){
      
      # Determining the forecast period from the name
      forecastPeriod <- substring(nameIndex, regexpr("-", nameIndex) + (nchar(locationGroupName) + 2))
      
      # Forecast period for yearly or time index data 
    }else{
      
      # Determining the forecast period from the name
      forecastPeriod <- strsplit(nameIndex, "[-]")[[1]][3]
      
    }
    
    ###############################################
    # Handling the NAs in the data - ARIMA models #
    ###############################################
    if(model.Figure == "ARIMA"){
      
      # Data for plot 
      data.for.plot <- formatted.forecast.Figure[[i]] %>% # Re-naming the orginal data 
        dplyr::mutate(median = ifelse(is.na(median), data, median), # Handling NAs for the median model fit
                      LB = ifelse(is.na(LB), data, LB), # Handling the NAs for the LB model fit
                      UB = ifelse(is.na(UB), data, UB)) # Handling the NAs for the UB model fit
      
      ###############################################
      # Handling the NAs in the data - Other models #
      ###############################################
    }else{
      
      # Renaming the data 
      data.for.plot <- formatted.forecast.Figure[[i]]
      
    }
    
    ########################################################
    # Handling dates in the forecast files - weeks or days #
    ########################################################
    if(date.Figure %in% c("week", "day")){
      
      # Dates on x-axis
      data.for.plot <- data.for.plot %>%
        mutate(dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data
      
      ##############################################################
      # Handling dates in the forecast files - years or time index #
      ##############################################################
    }else{
      
      # Dates on x-axis
      data.for.plot <- data.for.plot %>%
        mutate(dates = as.numeric(Date)) # Changing years and time index to numeric 
      
    }
    
    ##########################################
    # Final data to merge with the main list #
    ##########################################
    final.data <- data.for.plot %>%
      dplyr::mutate(forecastPeriod = forecastPeriod,
                    model = model.Figure,
                    location = locationGroupName)
    
    # Handling NAs
    if(any(final.data$UB == Inf)){
      
      final.data <- NA
      
    }
    
    # Saving the data in a list
    listData[[i]] <- final.data
    
  } # End of loop for figure data 
  
  ##########################################
  # Combining list of data frames into one #
  ##########################################
  allData <- as.data.frame(do.call(rbind, listData))
  
  
  
  #------------------------------------------------------------------------------#
  # Creating the panel figures ---------------------------------------------------
  #------------------------------------------------------------------------------#
  # About: This section takes the main data frame created above, and forms       #
  # multiple panel figures, faceting by model. One panel corresponds to one      #
  # location and forecast period. There can be up to four figures in one panel.  #
  #------------------------------------------------------------------------------#
  
  #############################################
  # Looping through possible forecast periods #
  #############################################
  
  # Creating the list of unique forecast periods
  forecastPeriodsUnique <- unique(allData$forecastPeriod)
  
  ###########################
  # Starting the outer loop #
  ###########################
  for(d in 1:length(forecastPeriodsUnique)){
    
    # Indexed forecast period
    indexedForecastPeriod <- forecastPeriodsUnique[d]
    
    # Filtering data to include only indexed forecast period
    dataFilteredPeriod <- allData %>%
      dplyr::filter(forecastPeriod == indexedForecastPeriod) # Filtering by forecast period 
    
    ######################################
    # Looping through possible locations #
    ######################################
    
    # Creating a list of unique locations
    locationsUnqiue <- unique(allData$location)
    
    #######################
    # Starting inner loop #
    #######################
    for(l in 1:length(locationsUnqiue)){
      
      # Indexed location 
      indexedLocation <- locationsUnqiue[l]
      
      # Filtering data by location
      dataFilteredFinal <- dataFilteredPeriod %>%
        dplyr::filter(location == indexedLocation) # Filtering by locations 
      
      ####################################
      # Setting up for graphing the data #
      ####################################
      
      # Adjusting the y-axis
      maxValue <- max(dataFilteredFinal[,-c(1, 6:9)], na.rm = T)
      
      # Determining the breaks in the y-axis
      breaks.graph <- ifelse(maxValue/10 + 5 == 0, 1, floor(maxValue/10 + 5))
      
      ########################################################
      # Handling dates in the forecast files - weeks or days #
      ########################################################
      if(date.Figure %in% c("week", "day")){
        
        # Dates on x-axis
        dataFilteredFinal <- dataFilteredFinal 
        
        # Vertical line
        breakLine <- anytime::anydate(indexedForecastPeriod)
        
        # X-axis breaks 
        xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(dataFilteredFinal$dates)), max(anydate(dataFilteredFinal$dates)), by = 7))  # X-axis breaks
        
        # Checking for large number of dates
        if(length(xAxisBreaks) > 10){
          
          xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(dataFilteredFinal$dates)), max(anydate(dataFilteredFinal$dates)), by = "2 weeks"))  # X-axis breaks
          
        }
        
        ##############################################################
        # Handling dates in the forecast files - years or time index #
        ##############################################################
      }else{
        
        # Dates on x-axis
        dataFilteredFinal <- dataFilteredFinal 
        
        # Vertical line
        breakLine <- as.numeric(indexedForecastPeriod)
        
        # X-axis breaks
        xAxisBreaks <- scale_x_continuous(breaks = seq(min(dataFilteredFinal$dates), max(dataFilteredFinal$dates), by = 1))  # X-axis breaks
        
      }
      
      ##############
      # Plot title #
      ##############
      title2 <- paste0(indexedLocation, "-", indexedForecastPeriod)
      
      ###########################
      # Creating the plot panel #
      ###########################
      panel <- ggplot(dataFilteredFinal, aes(x = dates, y = median, text = paste('Date: ', dates, '<br>Median:', round(as.numeric(median), 2)), group = 1)) +
        facet_grid(~model) +
        geom_ribbon(aes(ymin = LB, ymax = UB), fill = "grey90") + # 95% PI ribbon
        geom_line(aes(x = dates, y = UB, text = paste('Date: ', dates, '<br>UB:', round(as.numeric(UB), 2)), group = 1), linetype = "dashed", size = 0.65) + # UB
        geom_line(aes(x = dates, y = LB, text = paste('Date: ', dates, '<br>LB:', round(as.numeric(LB), 2)), group = 1), linetype = "dashed", size = 0.65) + # LB
        geom_line(color = "red", size = 0.9) + # Median line
        geom_point(aes(x = dates, y = data, text = paste('Date: ', dates, '<br>Count:', data)), color = "black", shape = 1, size = 2) +  # Data points
        geom_vline(xintercept = breakLine, linetype = "dashed") + # Vertical line
        xAxisBreaks + # X axis breaks (i.e., dates)
        scale_y_continuous(breaks = seq(0, maxValue + breaks.graph, by = breaks.graph), # Y-axis breaks
                           limits = c(0, maxValue)) +  # Y-axis limits
        labs(title = "", # Title
             y = "Counts")  + # Y-axis labels
        theme_classic() + # Base theme
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Switching x-axis labels horizontal
              plot.title = element_text(hjust = 0.5, face = "bold", size = 10), # Plot title
              axis.title.y = element_text(size = 10), # Y-axis label
              axis.title.x=element_blank(), # Removing the x-axis label
              panel.grid.major = element_line(color = "grey95"))
      
      
      ####################################
      # Saving the plot in the main list #
      ####################################
      figureListLocationLoop[[l]] <- panel
      
      # Adding name to list element
      names(figureListLocationLoop)[l] <- title2
      
    } # End of inner loop 
    
    # Final list to export 
    finalList <- c(figureListLocationLoop, finalList)
    
  } # End of outer loop
  
  ############################
  # Returning the final list #
  ############################
  
  return(finalList)
  
} # End of function