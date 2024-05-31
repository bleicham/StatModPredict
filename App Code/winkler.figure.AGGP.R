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
winkler.figure.AGGP <- function(scoresFigure){
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
  

#------------------------------------------------------------------------------#
# Preparing for plotting -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares for later plotting. Specifically, it sets the   #
# x-axis breaks for the forecast period dates. This only runs if the data      #
# is forecast-period specific. Otherwise, there is no need for the date        #
# breaks.                                                                      #
#------------------------------------------------------------------------------#

  ########################################################
  # Runs if the data contains the `Forecast Date` column #
  ########################################################
  if(colnames(winkler.input)[3] == "Forecast Date"){
    
    ##############################################
    # Handling the dates - Weekly and Daily data #
    ##############################################
    if(nchar(as.character(winkler.input[1,3])) > 4){
      
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anytime::anydate(winkler.input$`Forecast Date`)), max(anytime::anydate(winkler.input$`Forecast Date`)), by = 7))  # X-axis breaks
      
    ##############################################
    # Handling the dates - Yearly and time index #
    ##############################################
    }else{
      
      xAxisBreaks <- scale_x_continuous(breaks = seq(min(winkler.input$`Forecast Date`), max(winkler.input$`Forecast Date`), by = 1))  # X-axis breaks
      
    }
    
  }

#------------------------------------------------------------------------------#
# Plotting the forecast-period specific Winkler Scores -------------------------
#------------------------------------------------------------------------------#
# About: This sections plots the forecast-period specific Winkler Scores.      #
#------------------------------------------------------------------------------#
  
  ########################################################
  # Runs if the data contains the `Forecast Date` column #
  ########################################################
  if(colnames(winkler.input)[3] == "Forecast Date"){

    ###########################################################
    # Determining if the graph should include a point or line #
    ###########################################################
    if(length(unique(winkler.input$`Forecast Date`)) == 1){
      
      # Exports a scatter plot 
      geomType <- geom_point(size = 5)
      
    }else{
      
      # Exports a line plot
      geomType <- geom_line()
      
    }
    
    #######################
    # Creating the figure #
    #######################
    figure <- ggplot(data = winkler.input, aes(x = `Forecast Date`, y = `Winkler Score`, color = Model)) +
      geomType +
      facet_wrap(~Location, scales = "free_y") +
      xAxisBreaks +
      scale_color_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
      labs(y = "Winker Score",
           x = "") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey95"))
    
#------------------------------------------------------------------------------#
# Plotting the average Winkler Scores ------------------------------------------
#------------------------------------------------------------------------------#
# About: This sections plots the average Winkler Scores.                       #
#------------------------------------------------------------------------------#
  }else{
    
    #######################
    # Creating the figure #
    #######################
    figure <- ggplot(data = winkler.input, aes(x = Model, y = `Avg. Winkler`, fill = Model)) +
      geom_col(position="Dodge") + 
      facet_wrap(~Location, scales = "free_y") +
      scale_fill_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
      labs(x = "") + 
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey95"))
    
    
  }
  
# Returning the figure
return(figure)
    
}