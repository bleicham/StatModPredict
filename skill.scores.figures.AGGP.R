#------------------------------------------------------------------------------#
#                                                                              #
#                           Plotting the Skill Scores                          #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function plots the skill scores calculated for the fit or forecast      #
# metrics produced for the ARIMA, GLM, GAM, or Prophet models. Different       #
# figures are plotted dependent on if skill scores are requested for the crude #
# or average metrics. The resulting figures are then ouputted.                 #
#------------------------------------------------------------------------------#
#                         Author: Amanda Bleichrodt                            #
#------------------------------------------------------------------------------#
skill.scores.figures.AGGP <- function(skillScores){

#------------------------------------------------------------------------------#
# Preparing for plotting -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renames the skill scores input and creates the empty     #
# list to fill with crude skill scores.                                        #
#------------------------------------------------------------------------------#

  #######################
  # Skill scores inputs #
  #######################
  skillScores.input <<- skillScores
  
  ################################
  # List of skill scores figures #
  ################################
  skillScoresFigs <- list()

#------------------------------------------------------------------------------#
# Pulling information from data to determine figure type -----------------------
#------------------------------------------------------------------------------#
# About: This section examines the skill scores data to determine how many     #
# locations are included. Additionally, it determines the main geom to be used #
# dependent on if the metrics are the average scores or the crude scores and   #
# the number of locations included. Finally, it determines the number of       #
# forecast period dates included in the skill scores.                          #
#------------------------------------------------------------------------------#

  #####################################################
  # Determining if the skill scores are avg. or crude #
  #####################################################

  # Names of data
  dataNames <- names(skillScores.input)
  
  ################
  # Crude scores #
  ################
  if("Forecast Date" %in% c(dataNames)){
    
    crudeScores <- 1
  
  ##################
  # Average scores #
  ##################
  }else{
    
    crudeScores <- 0
    
  }

  #######################################
  # Determining the number of locations #
  #######################################
  locationsCount <- length(unique(skillScores.input$Location))
  
  ###################################################
  # Determining the number of forecast period dates #
  ###################################################
  if(crudeScores == 1){
    
    datesCount <- length(unique(skillScores.input$`Forecast Date`))
    
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
  if(crudeScores == 1){
    
    if(nchar(as.character(skillScores.input[1,2])) > 4){
      
      xAxisBreaks <- scale_x_continuous(breaks = seq.Date(min(anytime::anydate(skillScores.input$`Forecast Date`)), max(anytime::anydate(skillScores.input$`Forecast Date`)), by = 7))  # X-axis breaks
      
    ##############################################
    # Handling the dates - Yearly and time index #
    ##############################################
    }else{
      
      xAxisBreaks <- scale_x_continuous(breaks = seq(min(skillScores.input$`Forecast Date`), max(skillScores.input$`Forecast Date`), by = 1))  # X-axis breaks
      
    }
    
  }

#------------------------------------------------------------------------------#
# Crude figures plots ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section either plots a line plot or dot plot based on the number #
# of forecast period dates included in the skill scores data set. This section #
# only runs if the skill scores are on the crude metrics.                      #
#------------------------------------------------------------------------------#

  #############################################
  # Determining if figures should be produced #
  #############################################
  if(crudeScores == 1){
    
    ################################
    # Determining the type of geom #
    ################################
    if(datesCount == 1){
      
      # Geom type for one locations 
      geomType <- geom_point(size = 5)
      
    }else{
      
      # Geom type for multiple locations
      geomType <- geom_line()
      
    }
    
    ###################################
    # Plotting the skill scores - MSE #
    ###################################
    
    # Filtering the data
    skillMSE <- skillScores.input %>%
      dplyr::filter(Metric == MSE) 
    
    ggplot(data = skillScores.input, aes(x = `Forecast Date`, y = `Skill Scores`, color = `Comparison`)) +
      geom_col(stat = "identity")
    
    # Plotting the data 
    MSE <- ggplot(data = skillScores.input, aes(x = `Forecast Date`, y = `Skill Scores`, color = `Comparison`)) +
      geomType +
      facet_wrap(~Location, scales = "free_y") +
      xAxisBreaks +
      scale_color_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
      labs(title = "Mean Squared Error (MSE)",
           y = "") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey97"))
    
    ###################################
    # Plotting the skill scores - MAE #
    ###################################
    MAE <- ggplot(data = skillScores.input, aes(x = `Forecast Date`, y = MAE, color = `Comparison`)) +
      geomType +
      facet_wrap(~Location, scales = "free_y") +
      xAxisBreaks +
      scale_color_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
      labs(title = "Mean Absolute Error (MAE)",
           y = "") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey97"))
    
    ###################################
    # Plotting the skill scores - WIS #
    ###################################
    WIS <- ggplot(data = skillScores.input, aes(x = `Forecast Date`, y = WIS, color = `Comparison`)) +
      geomType +
      facet_wrap(~Location, scales = "free_y") +
      xAxisBreaks +
      scale_color_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
      labs(title = "Weighted Interval Scores (WIS)",
           y = "") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey97"))
    
    ##############################################
    # Plotting the skill scores - Winkler Scores #
    ##############################################
    Winkler <- ggplot(data = skillScores.input, aes(x = `Forecast Date`, y = `Winkler Score`, color = `Comparison`)) +
      geomType +
      facet_wrap(~Location, scales = "free_y") +
      xAxisBreaks +
      scale_color_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
      labs(title = "Winkler Scores",
           y = "") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey97"))
    
    #######################################
    # Adding the figures to the main list #
    #######################################
    skillScoresFigs <- list(MSE, MAE, WIS, Winkler)
  
  
  #------------------------------------------------------------------------------#
  # Average figures plots --------------------------------------------------------
  #------------------------------------------------------------------------------#
  # About: This section either plots a bar chart or heat map dependent on if     #
  # there is one location selected or multiple locations. This section only runs #
  # if the skill scores are on the average metrics.                              #
  #------------------------------------------------------------------------------#
    }else{
    
    ################################
    # Determining the type of geom #
    ################################
    if(locationsCount == 1){
      
      ###################################
      # Plotting the skill scores - MSE #
      ###################################
      MSE <- ggplot(data = skillScores.input, aes(x = `Comparison`, y = MSE, fill = `Comparison`)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_fill_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
        labs(title = bquote(bold("A")~"  Mean Squared Error (MSE)"),
             y = "Skill Score",
             x = "") + 
        theme_bw() +
        theme(axis.text.x = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey97"),
              legend.position = "none")
      
      ###################################
      # Plotting the skill scores - MAE #
      ###################################
      MAE <- ggplot(data = skillScores.input, aes(x = `Comparison`, y = MAE, fill = `Comparison`)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_fill_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
        labs(title = bquote(bold("B")~"  Mean Absolute Error (MAE)"),
             y = "",
             x = "")+ 
        theme_bw() +
        theme(axis.text.x = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey97"),
              legend.position = "none")
      
      ###################################
      # Plotting the skill scores - WIS #
      ###################################
      WIS <- ggplot(data = skillScores.input, aes(x = `Comparison`, y = WIS, fill = `Comparison`)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_fill_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
        labs(title = bquote(bold("C")~"  Weighted Interval Score (WIS)"),
             y = "Skill Score") + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey97"),
              legend.position = "none")
      
      ##############################################
      # Plotting the skill scores - Winkler Scores #
      ##############################################
      Winkler <- ggplot(data = skillScores.input, aes(x = `Comparison`, y = `Winkler Score`, fill = `Comparison`)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_fill_manual(values = c("#999999", "#56B4E9","#D55E00","#009E73")) +
        labs(title = bquote(bold("D")~"  Winkler Scores"),
             y = "Skill Score") + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey97"),
              legend.position = "none")
     
    }else{
    
    ###################################
    # Plotting the skill scores - MSE #
    ###################################
  
    MSE <- ggplot(data = skillScores.input, aes(x = `Comparison`, y = Location, fill = MSE)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "grey70", high = "blue") +
      geom_text(data=skillScores.input, aes(x = `Comparison`, y = Location,label = round(MSE, 2)), color = "grey90", size = 10) +
      labs(title = bquote(bold("A")~"  Mean Squared Error (MSE)"),
           y = "",
           x = "") + 
      theme_bw() +
      theme(axis.text.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey97"),
            legend.position = "none")
    
    ###################################
    # Plotting the skill scores - MAE #
    ###################################
    MAE <- ggplot(data = skillScores.input, aes(x = `Comparison`, y = Location, fill = MAE)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "grey70", high = "blue") +
      geom_text(data=skillScores.input, aes(x = `Comparison`, y = Location,label = round(MAE, 2)), color = "grey90", size = 10) +
      labs(title = bquote(bold("B")~"  Mean Absolute Error (MAE)"),
           y = "",
           x = "") + 
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey97"),
            legend.position = "none")
    
    ###################################
    # Plotting the skill scores - WIS #
    ###################################
    WIS <- ggplot(data = skillScores.input, aes(x = `Comparison`, y = Location, fill = WIS)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "grey70", high = "blue") +
      geom_text(data=skillScores.input, aes(x = `Comparison`, y = Location,label = round(WIS, 2)), color = "grey90", size = 10) +
      labs(title = bquote(bold("C")~"  Weighted Interval Score (WIS)"),
           y = "") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey97"),
            legend.position = "none")
    
    ##############################################
    # Plotting the skill scores - Winkler Scores #
    ##############################################
    Winkler <- ggplot(data = skillScores.input, aes(x = `Comparison`, y = Location, fill = `Winkler Score`)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "grey70", high = "blue") +
      geom_text(data=skillScores.input, aes(x = `Comparison`, y = Location,label = round(`Winkler Score`, 2)), color = "grey90", size = 10) +
      labs(title = bquote(bold("D")~"  Winkler Scores"),
           y = "") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text.y = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey97"),
            legend.position = "none")
    
    } # End of multi-location figures
      
    } # End of 'else' for average skill scores

#------------------------------------------------------------------------------#
# Combining individual plots into one figure -----------------------------------
#------------------------------------------------------------------------------#
# About: This section combines the individual plots from above into one panel. #
#------------------------------------------------------------------------------#
  
  ##################
  # Combined plots #
  ##################
  panelFig <- MSE + MAE + WIS + Winkler 
  
#------------------------------------------------------------------------------#
# Returning the figure(s) ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns either the list of crude skill scores or the     #
# corresponding figure panel for the average skill scores.                     #
#------------------------------------------------------------------------------#
  
  #################################
  # Returning the list of figures #
  #################################
  if(crudeScores == 1){
    
   test <-  skillScoresFigs
    
  ##############################
  # Returning the panel figure #
  ##############################
  }else{

    test <- panelFig
    
  }
  
  return(test)
  
} # End of function
