#------------------------------------------------------------------------------#
#                                                                              #
#                     Plotting Panel Figures - All Models                      #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function reads in the other model's forecast read in the dashboard, and #
# combines it with the produced ARIMA/GLM/GAM/Prophet forecasts. After         #
# cleaning up the data, the function then merges the all forecasts together.   #
# Next, it creates a panel of figures for a given location, forecast period    #
# date, horizon, and calibration length. The list of figures are then returned #
# as a product of the function.                                                #
#------------------------------------------------------------------------------#
#                         By: Amanda Bleichrodt                                #
#------------------------------------------------------------------------------#
other.panel.forecast.figures <- function(formatted.forecast.input, 
                                         formatted.forecast.Other.input,
                                         date.type.input){
  
#------------------------------------------------------------------------------#
# Reading in inputs from the main script ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names for manipulation throughout the remainder of the code.                 #
#------------------------------------------------------------------------------#
  
  ###########################
  # Formatted Forecast list #
  ###########################
  formatted.forecast.Figure <<- formatted.forecast.input
  
  ############################################
  # Reading in the other formatted forecasts #
  ############################################
  other.formatted.forecasts <<- formatted.forecast.Other.input
  
  #############
  # Date type #
  #############
  date.Figure <<- date.type.input
  
  ###############################################################
  # Creating and empty list for figures - Forecast period dates #
  ###############################################################
  listData <- list()
  
  ###################################################
  # Creating and empty list for figures - Locations #
  ###################################################
  listData1 <- list()
  
  #############################################################
  # Creating and empty list for figures - Calibration periods #
  #############################################################
  listData2 <- list()
  
  ###########################################################
  # Creating and empty list for figures - Forecast horizons #
  ###########################################################
  listData3 <- list()
  
  #########################
  # Final list of figures #
  #########################
  finalList <- list()
  
  
#------------------------------------------------------------------------------#
# Cleaning the other models ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the other forecasts read into the dashboard for #
# later merging with the ARIMA/GLM/GAM/Prophet models. It creates a single     #
# data frame of forecasts that can be used to produce panel figures.           #
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
    
    if(date.Figure %in% c("week", "day")){
      
    ############################################
    # Adding the information to the data frame #
    ############################################
    data.temp <- other.formatted.forecasts[[i]] %>%
      dplyr::mutate(Model = model,
                    Horizon = horizon,
                    Calibration = calibration,
                    Location = location,
                    ForecastDate = anytime::anydate(forecastPeriod),
                    Date = anytime::anydate(Date))
    
    }else{
      
      ############################################
      # Adding the information to the data frame #
      ############################################
      data.temp <- other.formatted.forecasts[[i]] %>%
        dplyr::mutate(Model = model,
                      Horizon = horizon,
                      Calibration = calibration,
                      Location = location,
                      ForecastDate = as.numeric(forecastPeriod),
                      Date = as.numeric(Date))
      
    }
    
    #################################################
    # Merging with the rest of the read-in forecasts #
    #################################################
    allOtherForecasts <- rbind(allOtherForecasts, data.temp)
    
  }
  
  #################################
  # Removing the first row of NAs #
  #################################
  allOtherForecasts <- allOtherForecasts[-1,]
  
#------------------------------------------------------------------------------#
# Cleaning the ARIMA/GLM/GAM/Prophet models ------------------------------------
#------------------------------------------------------------------------------#
# About: This section makes sure that the ARIMA/GLM/GAM/Prophet models are     #
# merged into a list that is the same format as the "Other" models forecast.   #
# This allows for merging in the next step, and later plotting of forecast     #
# panels.                                                                      #
#------------------------------------------------------------------------------#
  
  # Creating the empty data frame for merging - ARIMA/GLM/GAM/Prophet Forecasts
  allForecasts <- NA
  
  ###############################################################
  # Running only if ARIMA/GLM/GAM/Prophet forecasts are entered #
  ###############################################################
  if(!is.null((formatted.forecast.Figure))){
  
  #########################################################
  # Cleaning up the ARIMA, GLM, GAM and Prophet forecasts #
  #########################################################
  for(i in 1:length(formatted.forecast.Figure)){
    
    # Determining the name of the indexed forecast
    nameIndex <- names(formatted.forecast.Figure[i])
    
    # Model type 
    model <- strsplit(nameIndex, "[-]")[[1]][1]
    
    # Sub-setting location/group name 
    location <- strsplit(nameIndex, "[-]")[[1]][2]
    
    # Forecast period for weekly or daily data 
    if(date.Figure %in% c("week", "day")){
      
      # Determining the forecast period from the name
      forecastPeriod <- substring(nameIndex, regexpr("-", nameIndex) + (nchar(location) + 2))
      
      # Forecast period for yearly or time index data 
    }else{
      
      # Determining the forecast period from the name
      forecastPeriod <- strsplit(nameIndex, "[-]")[[1]][3]
      
    }
    
    ###############################################
    # Handling the NAs in the data - ARIMA models #
    ###############################################
    if(model == "ARIMA"){
      
      # Data for plot 
      data.for.plot <- formatted.forecast.Figure[[i]] %>% # Re-naming the orginal data 
        dplyr::mutate(median = ifelse(is.na(median), data, median), # Handling NAs for the median model fit
                      LB = ifelse(is.na(LB), data, LB), # Handling the NAs for the LB model fit
                      UB = ifelse(is.na(UB), data, UB), # Handling the NAs for the UB model fit
                      data = ifelse(is.na(data), median, data)) # Handling the NAs for the data
      
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
      
      # Forecast horizon
      horizon <- nrow(data.for.plot[data.for.plot$dates > anytime::anydate(forecastPeriod),])
      
    ##############################################################
    # Handling dates in the forecast files - years or time index #
    ##############################################################
    }else{
      
      # Dates on x-axis
      data.for.plot <- data.for.plot %>%
        mutate(dates = as.numeric(Date)) # Changing years and time index to numeric 
      
      # Forecast horizon
      horizon <- nrow(data.for.plot[data.for.plot$dates > as.numeric(forecastPeriod),])
      
    }
    
    #############################
    # Calibration period length #
    #############################
    calibration <- nrow(data.for.plot) - as.numeric(horizon)
    
    ##########################################
    # Final data to merge with the main list #
    ##########################################
    final.data <- data.for.plot %>%
      dplyr::mutate(Date = dates,
                    ForecastDate = forecastPeriod,
                    Model = model,
                    Location = location,
                    Horizon = horizon,
                    Calibration = calibration) %>%
      dplyr::select(Date, data, median, LB, UB, Model, Horizon, Calibration, Location, ForecastDate)
      
    #############################
    # Saving the data in a list #
    #############################
    allForecasts <- rbind(allForecasts, final.data)
    
  } # End of loop for figure data 
    
  #################################
  # Removing the first rows of NA #
  #################################
  allForecasts <- allForecasts[-1, ]
  
  } # End of if-statement
  
#------------------------------------------------------------------------------#
# Merging data frames ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section merges together the other files read into the dashboard  #
# and the existing models run within the dashboard.                            #  
#------------------------------------------------------------------------------#
  
  ###############################
  # Merging the two data frames #
  ###############################
  plotData <<- rbind(allOtherForecasts, allForecasts)
  
#------------------------------------------------------------------------------#
# Unique Values ----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates unique list of horizons, calibration periods,    #
# locations, and forecast period dates. This is then used to group forecasts   #
# for plotting in the next code chunk.                                         #
#------------------------------------------------------------------------------#
  
  #########################
  # Forecast horizon list #
  #########################
  horizonList <- c(unique(plotData$Horizon))
                   
  ###########################
  # Calibration period list #
  ###########################
  calibrationList <- c(unique(plotData$Calibration))
  
  #################
  # Location list #
  #################
  locationList <- c(unique(plotData$Location))
  
  ######################
  # Forecast Date List #
  ######################
  forecastDateList <- sort(c(unique(plotData$ForecastDate)))
  
#------------------------------------------------------------------------------#
# Creating the panel figures data ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the loops that will be used to form the figure  #
# including, filtering the data by the grouping variables.                     #
#------------------------------------------------------------------------------#
  
  #####################################
  # Looping through forecast horizons #
  #####################################
  for(h in 1:length(horizonList)){
    
    # Indexed forecast horizon
    indexedHorizon <- horizonList[h]
    
    #######################################
    # Looping through calibration periods #
    #######################################
    for(c in 1:length(calibrationList)){
      
      # Indexed calibration 
      indexedCalibration <- calibrationList[c]
      
      ######################################
      # Looping through forecast locations #
      ######################################
      for(l in 1:length(locationList)){
        
        # Indexed location
        indexedLocation <- locationList[l]
        
        #########################################
        # Looping through forecast period dates #
        #########################################
        for(f in 1:length(forecastDateList)){
          
          # Indexed forecast date
          indexedForecastDate <- forecastDateList[f] 
          
          ################################################################
          # Filtering the date for the indexed set of grouping variables #
          ################################################################
          if(date.Figure %in% c("week", "day")){
            
          dataFiltered <- plotData %>%
            dplyr::mutate(Date = anytime::anydate(Date)) %>%
            dplyr::filter(Horizon == indexedHorizon, # Filtering horizon
                          Calibration == indexedCalibration, # Filtering calibration
                          Location == indexedLocation, # Filtering location
                          ForecastDate == indexedForecastDate) # Filtering forecast date
           
          }else{
            
            dataFiltered <- plotData %>%
              dplyr::mutate(Date = as.numeric(Date)) %>%
              dplyr::filter(Horizon == indexedHorizon, # Filtering horizon
                            Calibration == indexedCalibration, # Filtering calibration
                            Location == indexedLocation, # Filtering location
                            ForecastDate == indexedForecastDate) # Filtering forecast date
          }
          
          #########################################################
          # Handling when a combination of factors DOES NOT exist #
          #########################################################
          if(nrow(dataFiltered) == 0){
            
            listData[f] <- NA # Replacing with NA
            listData[l] <- NA # Replacing with NA
            listData[c] <- NA # Replacing with NA
            listData[h] <- NA # Replacing with NA
            
            # Skipping to next loop iteration 
            next
            
          }
            
#------------------------------------------------------------------------------#
# Preparing for plotting -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the y-axis, x-axis, and vertical line for each  #
# of the individual figures included in the panel.                             #
#------------------------------------------------------------------------------#
          
          ####################################
          # Setting up for graphing the data #
          ####################################
          
          # Adjusting the y-axis
          maxValue <- max(dataFiltered[,-c(1, 6:10)], na.rm = T)
          
          # Determining the breaks in the y-axis
          breaks.graph <- ifelse(maxValue/10 + 5 == 0, 1, floor(maxValue/10 + 5))
          
          ########################################################
          # Handling dates in the forecast files - weeks or days #
          ########################################################
          if(date.Figure %in% c("week", "day")){
            
            # Vertical line
            breakLine <- anytime::anydate(dataFiltered$ForecastDate)
            
            # X-axis breaks 
            xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anytime::anydate(dataFiltered$Date)), max(anytime::anydate(dataFiltered$Date)), by = 7))  # X-axis breaks
            
            # Checking for large number of dates
            if(length(xAxisBreaks) > 5){
              
              xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anytime::anydate(dataFiltered$Date)), max(anytime::anydate(dataFiltered$Date)), by = "2 weeks"))  # X-axis breaks
              
            }
            
          ##############################################################
          # Handling dates in the forecast files - years or time index #
          ##############################################################
          }else{

            # Vertical line
            breakLine <- as.numeric(dataFiltered$ForecastDate)
            
            # X-axis breaks
            xAxisBreaks <- scale_x_continuous(breaks = seq(min(dataFiltered$Date), max(dataFiltered$Date), by = 1))  # X-axis breaks
            
          }
          
          ##############
          # Plot Title #
          ##############
          title1  <- paste0(dataFiltered[1,9], "-", dataFiltered[1,10])
          
          ############################
          # Handling the ARIMA Model #
          ############################
          dataFiltered <- dataFiltered %>%
            dplyr::mutate(median = ifelse(Model == "ARIMA", data, median),
                          LB = ifelse(Model == "ARIMA" & is.na(LB), data, LB),
                          UB = ifelse(Model == "ARIMA" & is.na(UB), data, UB))
          
#------------------------------------------------------------------------------#
# Plotting the panel figure ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the above data to plot the panel of forecast        #
# figures corresponding to the given location, calibration period, horizon,    #
# and forecast date. It then is saved to the list of figures that is           #
# outputted to the main dashboard.                                             #
#------------------------------------------------------------------------------#
          
  panel <- ggplot(dataFiltered, aes(x = Date, y = median)) +
            facet_wrap(~Model, scales = "free_y") +
            geom_ribbon(aes(ymin = LB, ymax = UB), fill = "grey90") + # 95% PI ribbon
            geom_line(aes(x = Date, y = UB), linetype = "dashed", size = 0.65) + # UB
            geom_line(aes(x = Date, y = LB), linetype = "dashed", size = 0.65) + # LB
            geom_line(color = "red", size = 0.9) + # Median line
            geom_point(aes(x = Date, y = data, text = paste('Date: ', Date, '<br>Count:', data)), color = "black", shape = 1, size = 2) +  # Data points
            geom_vline(xintercept = breakLine, linetype = "dashed") + # Vertical line
            xAxisBreaks + # X axis breaks (i.e., Date)
            scale_y_continuous(breaks = seq(0, maxValue + breaks.graph, by = breaks.graph), # Y-axis breaks
                               limits = c(0, maxValue)) +  # Y-axis limits
            labs(title = title1, # Title
                 y = "Counts")  + # Y-axis labels
            theme_classic() + # Base theme
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Switching x-axis labels horizontal
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 10), # Plot title
                  axis.title.y = element_text(size = 10), # Y-axis label
                  axis.title.x=element_blank(), # Removing the x-axis label
                  panel.grid.major = element_line(color = "grey95"))
          
          #############################################
          # Adding Forecast date loop figures to list #
          #############################################
          listData[f] <- list(panel)
          
          # Adding label to list element
          names(listData)[f] <- paste0(title1, "-",indexedCalibration, "-", indexedHorizon)   
          
          } # End of loop for forecast dates 
        
        ###############################################################
        # Adding forecast date loop figures and location loop figures #
        ###############################################################
        listData1 <- c(listData1, listData)
        
        } # End of loop for locations
      
      #######################################################
      # Adding location and calibration period figure lists #
      #######################################################
      listData2 <- c(listData2, listData1)
      
      } # End of calibration loop 
    
    ##########################
    # Final list combination #
    ##########################
    listData3 <- c(listData3, listData2)
    
  } # End of horizon loop 
  
  ###############################
  # Removing NAs for final list #
  ###############################
  finalList <- listData3[!is.na(listData3)]
  
  # Removing unique rows
  ListtoExport <- unique(finalList)
  
  ################
  # Adding Names #
  ################
  for(i in 1:length(ListtoExport)){
    
    # Update figure name
    names(ListtoExport)[i] <- ListtoExport[[i]][["labels"]][["title"]]
    
  }

  ############################
  # Returning the final list #
  ############################
  return(ListtoExport)
}
  
  
  
  
      
      
    