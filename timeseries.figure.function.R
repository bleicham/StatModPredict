#------------------------------------------------------------------------------#
#                                                                              #
#                     Producing the time-series figures                        #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This function takes in the crude data and user selected locations. It #
# then determines the date composition, and filters the data to include the    #
# locations selected by the user. The function then outputs a time series      #
# figure.                                                                      #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#
timeseries.figure.function <- function(timeseries.input, location.input,
                                       dateType.input, forecastLineShow,
                                       forecastDatesStart, forecastDatesEnd){
  
#------------------------------------------------------------------------------#
# Reading in the inputs --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the function inputs and saves them under new    #
# names to be used for the function.                                           #
#------------------------------------------------------------------------------#

#########################
# Timeseries data input #
#########################
timeseries.data <- timeseries.input

#######################
# Location data input #
#######################
locations <- location.input

###################
# Date Type input #
###################
dateType <- dateType.input

###############################################
# Indicator if forecast lines should be shown #
###############################################
lineIndicator <- forecastLineShow

#########################
# Start forecast period #
#########################
startForecastPeriod <- forecastDatesStart

#######################
# End forecast period #
#######################
EndForecastPeriod <- forecastDatesEnd

####################
# List for figures #
####################
TimseriesList <- list()

#------------------------------------------------------------------------------#
# Built in error ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns an error if no locations are chosen. It prompts  #
# the user to enter a location prior to running the forecasts again.           #
#------------------------------------------------------------------------------#

if(length(locations) == 0){
  
  # Error to return 
  return("No location has been selected. Please select at least one location prior to running forecasts.")
  
}

#------------------------------------------------------------------------------#
# Filtering data based on selected locations -----------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the crude data to only choose the locations      #
# selected by the user.                                                        #
#------------------------------------------------------------------------------#

##################################
# Setting a standard date header #
##################################
names(timeseries.data)[1] <- "Dates"

##########################
# Data from wide to long #
##########################
data.for.plot <- timeseries.data %>%
  pivot_longer(-Dates, names_to = "Location", values_to = "Count")

#------------------------------------------------------------------------------#
# Setting up for producing the figures -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the y- and x- axis breaks for the figure. The   #
# purpose of this section is to make the plot as flexible as possible to all   #
# types of data.                                                               #
#------------------------------------------------------------------------------#

######################################################
# Adjusting the y-axis - determining the max y value #
######################################################
maxValue <- max(data.for.plot[,-c(1,2)], na.rm = T)

########################################
# Determining the breaks in the y-axis #
########################################
breaks.graph <- ifelse(maxValue/10 + 5 == 0, 1, floor(maxValue/10 + 5))

###################################
# Handling dates - Weeks and Days #
###################################
if(dateType %in% c("week", "day")){
  
  # Dates on x-axis
  data.for.plot <- data.for.plot %>%
    mutate(Dates = anytime::anydate(Dates)) # Handling dates if working with weekly and daily data
  
  # X-axis breaks 
  xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(data.for.plot$Dates), max(data.for.plot$Dates), by = 7))  # X-axis breaks
  
  # Creating the vector of line breaks
  lineBreaksVector <- seq.Date(anytime::anydate(startForecastPeriod), anytime::anydate(EndForecastPeriod), by = 7)
    
  ##########################################
  # Handling dates - Years or Time Indexes #  
  ##########################################
  }else{
    
    # Dates on x-axis
    data.for.plot <- data.for.plot %>%
      mutate(Dates = as.numeric(Dates)) # Changing years and time index to numeric 
    
    # X-axis breaks 
    xAxisBreaks <- scale_x_continuous(breaks = seq(min(data.for.plot$Dates), max(data.for.plot$Dates), by = 1))  # X-axis breaks
    
    # Creating the vector of line breaks
    lineBreaksVector <- seq(as.numeric(startForecastPeriod), as.numeric(EndForecastPeriod), by = 1)
    
  }

#------------------------------------------------------------------------------#
# Determining if the line breaks for forecast periods should be shown ----------
#------------------------------------------------------------------------------#
# About: This section determines if the line breaks should be shown on the     #
# timeseries figure, based on the users input.                                 #
#------------------------------------------------------------------------------#

######################
# Plotting the lines #
######################
if(lineIndicator == T){
  
  plottedLines <- geom_vline(xintercept  = lineBreaksVector, color = "black", linetype = "dashed") 
  
  ##########################
  # Not plotting the lines #
  ##########################
  }else{
  
  plottedLines <- NULL
  
}

#------------------------------------------------------------------------------#
# Plotting the time series -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the data from above to plot the time series data    #
# for the locations selected by the user.                                      #
#------------------------------------------------------------------------------#
figure <- ggplot(data.for.plot, aes(x = Dates, y = Count, color = Location)) +
  plottedLines + 
  geom_line(aes(group= 1, text = paste('Date: ', Dates,
                             '<br>Count:', Count))) +
  
  xAxisBreaks + # X axis breaks (i.e., dates)
  scale_y_continuous(breaks = seq(0, maxValue + breaks.graph, by = breaks.graph), # Y-axis breaks
                     limits = c(0, maxValue)) + # Y-axis limits
  labs(color = "") + 
  theme_classic() + # Base theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Switching x-axis labels horizontal
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10), # Plot title
        axis.title.y = element_text(size = 10), # Y-axis label
        axis.title.x=element_blank()) # Removing the x-axis label

# GGPLOT version 
figure1 <- ggplot(data.for.plot, aes(x = Dates, y = Count, color = Location)) +
  plottedLines + 
  geom_line() +
  xAxisBreaks + # X axis breaks (i.e., dates)
  scale_y_continuous(breaks = seq(0, maxValue + breaks.graph, by = breaks.graph), # Y-axis breaks
                     limits = c(0, maxValue)) + # Y-axis limits
  labs(color = "") + 
  theme_classic() + # Base theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Switching x-axis labels horizontal
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10), # Plot title
        axis.title.y = element_text(size = 10), # Y-axis label
        axis.title.x=element_blank()) # Removing the x-axis label

TimseriesList <- list(figure, figure1)

# Returning the list
return(TimseriesList)


}