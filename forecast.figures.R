#------------------------------------------------------------------------------#
#                                                                              #
#                            Plotting Figures                                  #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file reads in the formatted forecasts created during an earlier step in #
# the toolbox, after first inputting the model type of interest, the indexed   #
# forecasting horizon, the process of interest, the type of data, the date     #
# composition of the data (i.e., week, day, year, time index), the indexed     #
# calibration period and a smoothing indicator. It then plots each of the      #
# applicable forecasts, and outputs a '.tiff' figure to the appropriate folder #
# in the working directory.                                                    #
#------------------------------------------------------------------------------#
#                         By: Amanda Bleichrodt                                #
#------------------------------------------------------------------------------#
forecast.figures <- function(formatted.forecast.input, data.type.input){

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

#############
# Date type #
#############
date.Figure <- data.type.input

###########################
# Creating the empty list #
###########################
figureList <- list()

#------------------------------------------------------------------------------#
# Looping through formatted forecasts for plotting -----------------------------
#------------------------------------------------------------------------------#
# About: This section loops through forematted forecasts and creates a list of #
# of forecast figures.                                                         #
#------------------------------------------------------------------------------#
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
  

  ################################
  # Handling the NAs in the data #
  ################################
  if(model.Figure == "ARIMA"){
    
    # Data for plot 
    data.for.plot <- formatted.forecast.Figure[[i]] %>%
      dplyr::mutate(median = ifelse(is.na(median), data, median), # Handling NAs for the median model fit
                    LB = ifelse(is.na(LB), data, LB), # Handling the NAs for the LB model fit
                    UB = ifelse(is.na(UB), data, UB)) # Handling the NAs for the UB model fit
    
  }else{
    
    # Data for plot
    data.for.plot <- formatted.forecast.Figure[[i]]
    
  }
  
  ####################################
  # Setting up for graphing the data #
  ####################################
  
  # Adjusting the y-axis
  maxValue <- max(data.for.plot[,-c(1,6)], na.rm = T)
  
  # Determining the breaks in the y-axis
  breaks.graph <- ifelse(maxValue/10 + 5 == 0, 1, floor(maxValue/10 + 5))
  
  # Handling INF
  if(maxValue == Inf){
    
    # Saving an NA
    figureList[[i]] <- NA
    
    # Adding name to list element
    names(figureList)[i] <- nameIndex
    
    # Next loop
    next
    
  }

  ########################################################
  # Handling dates in the forecast files - weeks or days #
  ########################################################
  if(date.Figure %in% c("week", "day")){
    
    # Dates on x-axis
    data.for.plot <- data.for.plot %>%
      mutate(dates = anytime::anydate(Date)) # Handling dates if working with weekly and daily data
    
    # Vertical line
    breakLine <- anytime::anydate(forecastPeriod)
    
    # X-axis breaks 
    xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anydate(data.for.plot$dates)), max(anydate(data.for.plot$dates)), by = 7))  # X-axis breaks
    
    ##############################################################
    # Handling dates in the forecast files - years or time index #
    ##############################################################
    }else{

      # Dates on x-axis
      data.for.plot <- data.for.plot %>%
        mutate(dates = as.numeric(Date)) # Changing years and time index to numeric 
      
      # Vertical line
      breakLine <- as.numeric(forecastPeriod)
      
      # X-axis breaks
      xAxisBreaks <- scale_x_continuous(breaks = seq(min(data.for.plot$dates, na.rm = T), max(data.for.plot$dates, na.rm = T), by = 1))  # X-axis breaks

      }
  

  ##############
  # Plot title #
  ##############
  title <- nameIndex
  
  
  ######################
  # Plotting the graph #
  ######################
  individual.figure <- ggplot(data.for.plot, aes(x = dates, y = median, text = paste('Date: ', dates, '<br>Median:', round(as.numeric(median), 2)), group = 1)) +
    geom_ribbon(aes(ymin = LB, ymax = UB), fill = "grey90")+ # 95% PI ribbon
    geom_line(aes(x = dates, y = UB, text = paste('Date: ', dates, '<br>UB:', round(as.numeric(UB), 2)), group = 1), linetype = "dashed", size = 0.65) + # UB
    geom_line(aes(x = dates, y = LB, text = paste('Date: ', dates, '<br>LB:', round(as.numeric(LB), 2)), group = 1), linetype = "dashed", size = 0.65) + # LB
    geom_line(color = "red", size = 0.9) + # Median line
    geom_point(aes(x = dates, y = data, text = paste('Date: ', dates, '<br>Count:', data)), color = "black", shape = 1, size = 2) + # Data points
    geom_vline(xintercept = breakLine, linetype = "dashed") + # Vertical line
    xAxisBreaks + # X axis breaks (i.e., dates)
    scale_y_continuous(breaks = seq(0, maxValue + breaks.graph, by = breaks.graph), # Y-axis breaks 
                       limits = c(0, maxValue)) + # Y-axis limits
    labs(title = title, # Title
         y = "Counts") + # Y-axis labels
    theme_classic() + # Base theme
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Switching x-axis labels horizontal
          plot.title = element_text(hjust = 0.5, face = "bold", size = 10), # Plot title
          axis.title.y = element_text(size = 10), # Y-axis label
          axis.title.x=element_blank(), # Removing the x-axis label
          panel.grid.major = element_line(color = "grey95"))

  
  ####################################
  # Saving the plot in the main list #
  ####################################
  figureList[[i]] <- individual.figure
  
  # Adding name to list element
  names(figureList)[i] <- title
  
  } # End of loop

######################
# Returning the list #
######################
return(figureList)

} # End of function


