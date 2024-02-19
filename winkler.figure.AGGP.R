#------------------------------------------------------------------------------#
#                                                                              #
#                      Plotting the Winkler Scores                             #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function plots the average Winkler scores created during an earlier     #
# part of the dashboard.                                                       #
#------------------------------------------------------------------------------#
#                     Author: Amanda Bleichrodt                                #
#------------------------------------------------------------------------------#
winkler.figure.AGGP <- function(scoresFigure, locationWinklerFig,
                                modelsWinklerFig){
#------------------------------------------------------------------------------#
# Reading in the inputs from the main dashboard --------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in inputs related to the Winkler scores, models,   #
# and locations/groups. The inputs are then saved under a new name.            #
#------------------------------------------------------------------------------#

##################
# Winkler scores #
##################
winkler.input <- scoresFigure

#############
# Locations #
#############
locations.input <- locationWinklerFig

############
# Model(s) #
############
models.input <- modelsWinklerFig

#------------------------------------------------------------------------------#
# Preparing the data for plotting ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the Winkler Scores input for plotting.          #
#------------------------------------------------------------------------------#

###############################################
# Runs if no locations or models are selected #
###############################################
if(is.null(locations.input) | length(locations.input) == 0 | is.null(models.input) | length(models.input) == 0){
  
  # Returning NULL
  return(NULL)
  
###################################################
# Runs if there are locations AND models selected #
###################################################
}else{
  
  # Filtering the Winkler Scores
  winklerFiltered <- winkler.input %>%
    dplyr::filter(Location %in% c(locations.input), # Location filtering 
                  Model %in% c(models.input)) # Model filtering
  
}

#------------------------------------------------------------------------------#
# Preparing for plotting -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares for later plotting. Specifically, it sets the   #
# x-axis breaks for the forecast period dates.                                 #
#------------------------------------------------------------------------------#

##############################################
# Handling the dates - Weekly and Daily data #
##############################################
if(nchar(as.character(winklerFiltered[1,3])) > 4){
  
  xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anytime::anydate(winklerFiltered$`Forecast Date`)), max(anytime::anydate(winklerFiltered$`Forecast Date`)), by = 7))  # X-axis breaks
  
  ##############################################
  # Handling the dates - Yearly and time index #
  ##############################################
}else{
  
  xAxisBreaks <- scale_x_continuous(breaks = seq(min(winklerFiltered$`Forecast Date`), max(winklerFiltered$`Forecast Date`), by = 1))  # X-axis breaks
  
}

#------------------------------------------------------------------------------#
# Plotting the winkler scores --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the averaged winkler score for each forecast       #
# forecast period date. The figure is then outputted to the main dashboard.    #
#------------------------------------------------------------------------------#

# Determining if there should be a point or line
if(length(unique(winklerFiltered$`Forecast Date`)) == 1){
  
  geomType <- geom_point(size = 5)
  
}else{
  
  geomType <- geom_line()
  
}

# Figure
figure <- ggplot(data = winklerFiltered, aes(x = `Forecast Date`, y = `Avg. Winkler`, color = Model)) +
  geomType +
  facet_wrap(~Location, scales = "free_y") +
  xAxisBreaks +
  scale_color_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
  labs(y = "Avg. Winker Score") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey95"))
                               

# Returning the figure
return(figure)
    
}