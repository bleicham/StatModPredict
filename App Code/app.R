#------------------------------------------------------------------------------#
#                                                                              #
#             ARIMA, GAM, GLM, Prophet Forecasting Toolbox - BETA              #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This is the main script included as part of the ARIMA, GLM, GAM, Prophet     #
# toolbox. The toolbox allows users to fit and forecast using common           #
# statistical models, employing a multitude of specifications. The only inputs #
# needed for the toolbox is the data of interest, formatted with one column    #
# of dates and the remaining corresponding to groups of interest. The toolbox  #
# works with yearly, daily, weekly, and time index data and does not require   #
# a specific date format. In addition to forecasting and fitting using the     #
# toolbox models, the user can also evaluate their forecasts and model fits    #
# utilizing mean squared error, mean absolute error, prediction interval       #
# coverage, and weighted interval scores. Finally, other models, pending they  #
# are formatted correctly, can be read into the toolbox and compared against   #
# the ARIMA, GLM, GAM, and Prophet models, Please refer to the documentation   #
# for full toolbox details.                                                    #
#------------------------------------------------------------------------------#
#             Amanda Bleichrodt, Ruiyan Luo, Alexander Kirpich,                #
#                      Gerardo Chowell and Amelia Phan                         #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Creating the 'not-in' function -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the 'not-in' function. Therefore, `%!in%` now    #
# can be used as the inverse of the built-in `%in%` function.                  #
#------------------------------------------------------------------------------#

`%!in%` <- function(x, y) {
  
  !(x %in% y)
  
}

#------------------------------------------------------------------------------#
# Loading needed functions -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section loads the functions needed for the various options the   #
# toolbox provides for modeling. Each function complete different tasks, and   #
# are called based on user selections.                                         #
#------------------------------------------------------------------------------#

source("date.type.function.R") 
source("forecast.period.dates.function.R") 
source("calibration.period.function.R") 
source("timeseries.figure.function.R")
source("ARIMA.R") 
source("formatted.forecast.function.R") 
source("filteringFormattedForecasts.R")
source("forecast.figures.R") 
source("GAM.R")
source("GLM.R") 
source("panel.forecast.figures.R") 
source("Prophet.R") 
source("modelFitMetrics.R") 
source("CrudeMetricsFigure.R")
source("forecastingMetrics.R")
source("averageMetrics.R")
source("AverageMetricsPanel.R")
source("Otherforecast.figures.R")
source("other.panel.forecast.figures.R")
source("winkler.scores.AGGP.R") 
source("winkler.figure.AGGP.R")
source("skillScoresMain.R")
source("skill.scores.figures.AGGP.R")
source("combiningAllMetrics.R")
source("filterOtherMetrics.R")
source("crudeMetricsFigureOther.R")
source("avgAllMetrics.R")
source("avgMetricsFigureOther.R")
source("Winkler.Scores.Model.Comparison.R")
source("filterOtherWinkler.R")
source("filteringFormattedIndivFigs.R")
source("filteringQuantileForecasts.R")
source("filterMetrics.R")
source("filterMetricsCRUDE.R")
source("skillScoresOther.R")

#------------------------------------------------------------------------------#
#                             Needed Packages                                  #
#------------------------------------------------------------------------------#
pacman::p_load(MASS, shiny, shinydashboard, shinyWidgets, bslib, plotly, anytime,
               shinyalert, shinyjs, shinybusy, editData, shinyBS, DT, stringr,
               tidyverse, forstringr, mgcv, processx, ggpubr, forecast, 
               prophet, zip, glue, shinyjqui, patchwork, ggplot2, zoo, gridExtra,
               viridis, qdapRegex, RColorBrewer)

#------------------------------------------------------------------------------#
#                            User Interface                                    #
#------------------------------------------------------------------------------#

ui <- dashboardPage(skin = "black", 
                    
           
#------------------------------------------------------------------------------#      
# Dashboard Header -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets up the top of the app.                              #
#------------------------------------------------------------------------------#
                    
dashboardHeader(

 #########################################
 # Adding the Small Header - Left Header #
 #########################################
                      
 title = span("StatModPredict",  # Title 
              style = "font-family: Arial; # Setting font-type
                       font-size: 18px;" # Setting font size 
              ) 

 ), # End of dashboard header 
                    
       
#------------------------------------------------------------------------------#
# Dashboard Sidebar ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the dashboard sidebar that shows dependent on    #
# which page is selected.                                                      #
#------------------------------------------------------------------------------#

dashboardSidebar(  
  
  ######################
  # Selecting the page #
  ######################
  pickerInput(
    "my_picker",
    label = "Page: ", 
    choices = c("Forecasting", "Model Metrics", "Model Comparison"),
    width = "200px"  # Adjust the width as needed
  ),
  
  ##########################
  # Page One - Forecasting #
  ##########################
  conditionalPanel(
    
    # Condition that must be met to produce the sidebar menu for page 1
    condition = "input.my_picker == 'Forecasting'", 
    
    ######################
    # Producing sidebars #
    ######################
    sidebarMenu(id = "sidebar",
                
#------------------------------------------------------------------------------#
# Data Specification Tab -------------------------------------------------------
#------------------------------------------------------------------------------#
                
#######################################
# Menu option for data specifications #
#######################################
menuItem("Data Options", 
         
         # Menu ID
         tabName = "dataOptions",
         
         # Icon for sidebar dropdown 
         icon = icon("chart-line"), 
         
         ######################
         # Uploading Data-set #
         ######################
         fileInput("dataset", # ID of UI input
                   label = tags$span("Upload time-series data file", # Shown label
                                     # Creating the info circle 
                                     tags$i(class = "glyphicon glyphicon-info-sign",
                                            style = "color:#FFFFFF;",
                                            title = "Upload a '.csv' file of your data.")
                   ),
                   
                   multiple = FALSE), # End of file input
         
         ######################
         # Location selection #
         ######################
         uiOutput("location.selection"), 
         
         ##################
         # Smoothing Type #
         ##################
         uiOutput("smoothing"),
         
         # Starting the menu expanded 
         startExpanded = T
         
), # End of Menu Option


#------------------------------------------------------------------------------#
# Forecasting specifications tab -----------------------------------------------
#------------------------------------------------------------------------------#

###########################################
# Menu option for forecast specifications #
###########################################
menuItem("Forecasting Specifications", 
         
         # Menu ID
         tabName = "forecastOptions",
         
         # Icon to show for tab 
         icon = icon("chart-line"),
         
         ####################################
         # Choosing the forecasting periods #
         ####################################
         uiOutput("forecast.period"),
         
         #########################################
         # Choosing the model calibration period #
         #########################################
         uiOutput("calibration.period"),
         
         ####################################
         # Choosing the forecasting horizon #
         ####################################
         numericInput("forecastHorizon", # Choosing the forecasting horizon
                      label = tags$span("Forecasting Horizon:", # Input label
                                        tags$i(class = "glyphicon glyphicon-info-sign",
                                               style = "color:#FFFFFF;",
                                               title = "Indicate the length of forecast you wish to produce.")),
                      value = 1, # Initial starting period
                      min = 1 # Setting the minimum possible horizon to 1
         ),
         
         #####################################
         # Creating the picker input to show #
         #####################################
         pickerInput("quantileSelection", # Drop-down ID
                     label = tags$span("Prediction Interval to Show:", # Input label
                                       tags$i(class = "glyphicon glyphicon-info-sign",
                                              style = "color:#FFFFFF;",
                                              title = "Indicate the prediction interval to show for the formatted forecasts and figures.")),
                     choices = c(seq(10, 90, by = 10), 95, 98), # Adding the choices
                     selected = 95, # Pre-selected choices
                     multiple = F), # Allowing for multiple locations/groups to be selected
        
         
         # Starting with the menu closed 
         startExpanded = T
         
), # End of Menu item


#------------------------------------------------------------------------------#
# Model specifications tab -----------------------------------------------------
#------------------------------------------------------------------------------#

########################################
# Menu option for model specifications #
########################################
menuItem("Model Specifications", 
         
         # Menu ID
         tabName = "modelSpecifications",
         
         # Icon for tab
         icon = icon("chart-line"),
         
         ####################
         # Choosing a model #
         ####################
         pickerInput("modelType", # Choosing a model
                     label = tags$span("Select a Model: ", # Input label
                                       tags$i(class = "glyphicon glyphicon-info-sign",
                                              style = "color:#FFFFFF;",
                                              title = "More than one model may be chosen.")
                     ),
                     selected = NULL,
                     choices = c("ARIMA" = "ARIMA",
                                 "GLM" = "GLM",
                                 "GAM" = "GAM",
                                 "Prophet" = "Prophet"),
                     multiple = T), # Choosing more than one model
         
         ############################################
         # Conditional on Model Type Chosen - ARIMA #
         ############################################
         conditionalPanel(
           
           # Condition
           condition = "input.modelType.includes('ARIMA')",
           
           # Title of options 
           h3("ARIMA Model Settings:", style = "font-size: 15px; color: white;"),
           
           # Seasonal vs non-seasonal
           uiOutput("ARIMA.seasonality"), 
           
           # Rows of parameters
           fluidRow(
             
             # Row One - p parameter 
             splitLayout(
               
               # Width of cells
               cellWidths = c("49%", "49%"),
               
               # Min P 
               uiOutput("pMin"),
               
               # Max P
               uiOutput("pMax")
             ),
             
             # Row two - P parameter 
             splitLayout(
               
               # Width of cells
               cellWidths = c("49%", "49%"),
               
               # Min P 
               uiOutput("PMin"),
               
               # Max P
               uiOutput("PMax")
             ),
             
             # Row three - q Parameter 
             splitLayout(
               
               # Width of cells 
               cellWidths = c("49%", "49%"),
               
               # Min Q
               uiOutput("qMin"),
               
               # Max Q
               uiOutput("qMax")
             ),
             
             # Row Four - Q Parameter 
             splitLayout(
               
               # Width of cells 
               cellWidths = c("49%", "49%"),
               
               # Min Q
               uiOutput("QMin"),
               
               # Max Q
               uiOutput("QMax")
             )
             
             
             ), # End of Fluid Row 
           
           # Differences parameter - Non Seasonal  
           uiOutput("differences"),
           
           # Differences parameter - Seasonal  
           uiOutput("Seasonaldifferences")
           
         ), # End of ARIMA options 
         
         ##########################################
         # Conditional on Model Type Chosen - GLM #
         ##########################################
         conditionalPanel(
           
           # Condition 
           condition = "input.modelType.includes('GLM')",
           
           # Title of panel 
           h3("GLM Model Settings:", style = "font-size: 15px; color: white;"),
           
           # Error term
           uiOutput("errorTermGLM")
           
         ), # End of GLM section 
         
         ##########################################
         # Conditional on Model Type Chosen - GAM #
         ##########################################
         conditionalPanel(
           
           # Condition 
           condition = "input.modelType.includes('GAM')",
           
           # Title of panel 
           h3("GAM Model Settings:", style = "font-size: 15px; color: white;"),
           
           # Number of basis functions 
           uiOutput("numBasis"),
           
           # Smoothing term
           uiOutput("smoothingTerm"),
           
           # Error term
           uiOutput("errorTerm")
           
         ), # End of GAM section 
         
         ##############################################
         # Conditional on Model Type Chosen - Prophet #
         ##############################################
         conditionalPanel(
           
           # Condition
           condition = "input.modelType.includes('Prophet')",
           
           # Title 
           h3("Prophet Model Settings:", style = "font-size: 15px; color: white;"),
           
           # Growth type 
           uiOutput("growthTrend"),
           
           # Seasonality
           uiOutput("prophetSeasonality"), 
           
           # Holidays 
           airDatepickerInput(
             "holidays",
             "Select Holidays:",
             multiple = T
           )
           
         ), # End of Prophet selection 
         
         # Starting the panel collapsed 
         startExpanded = F
         
         ), # End of menu option for model specifications 

#############################
# Creating the 'Run' button #
#############################

column(
  width = 1, 
  shiny::actionButton("run",
                      label = "Run Forecasts",
                      icon = icon("play", class="fa-regular fa-play"),
                      width = 170)
)


) # End of first sidebar menu 

), # End of conditional panel for forecasting 

##########################
# Page 2 - Model Metrics #
##########################
conditionalPanel(
  
  # Condition that must be met to produce the sidebar menu for page 1
  condition = "input.my_picker == 'Model Metrics'",
  
  # Creating the drop-down for forecasting vs model fit
  pickerInput("metricsToShow", # Choosing a metric
              label = tags$span("Select metrics to show: ", # Input label
                                tags$i(class = "glyphicon glyphicon-info-sign",
                                       style = "color:#FFFFFF;",
                                       title = "Either model fit statistics or forecasting statistics will show.")
              ),
              selected = "Model Fit",
              choices = c("Model Fit", "Forecasts"),
              multiple = F) # Allowing only one choice
  
  
  ), # End of conditional panel for page 2

###############################
# Page Two - Model Comparison #
###############################
conditionalPanel(
  
  # Condition that must be met to produce the sidebar menu for page 1
  condition = "input.my_picker == 'Model Comparison'", 
  
  ######################
  # Producing sidebars #
  ######################
  sidebarMenu(id = "sidebar",
              
              ############################
              # Reading in the Forecasts #
              ############################
              fileInput("dataset2", # ID of UI input
                        label = tags$span("Upload Forecast Files", # Shown label
                                          # Creating the info circle 
                                          tags$i(class = "glyphicon glyphicon-info-sign",
                                                 style = "color:#FFFFFF;",
                                                 title = "Upload '.csv' file(s).")
                        ),
                        
                        multiple = TRUE),
              
              
              ################################
              # Reading in the other metrics #
              ################################
              fileInput("metricsOther", # ID of UI input
                        label = tags$span("Upload Performance Metrics", # Shown label
                                          # Creating the info circle 
                                          tags$i(class = "glyphicon glyphicon-info-sign",
                                                 style = "color:#FFFFFF;",
                                                 title = "Upload '.csv' files.")
                        ),
                        
                        multiple = TRUE)
            
  )
  
)
              
), # End of sidebar menu 


      
                    
# UI Body ----------------------------------------------------------------------

dashboardBody(
  
  #################
  # Hiding errors #
  #################
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ), 
  
  ###########################
  # Adding a loading circle #
  ###########################
  add_busy_spinner(spin = "fading-circle"),
           
  ##################################           
  # Adjusting the dashboard length #
  ##################################
  tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))), 
  
#------------------------------------------------------------------------------#
# Creating Page 1: Forecasting -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates all of the information shown on page one of the  #
# dashboard. Additionally, it creates the UI inputs located within each box    #
# of page 1.                                                                   #
#------------------------------------------------------------------------------#
conditionalPanel(
  
  # Condition indicating the need for the 'forecasting' page to be selected 
  condition = "input.my_picker == 'Forecasting'", 
          
  ################################################################        
  # First row of the dashboard - Formatted forecasts and figures #   
  ################################################################
  fluidRow(
    
    # Width of row 
    width = 12, 
    
    # Setting a column to keep everything align
    column(width = 12, 
           
           ######################################################
           # Box that contains the 'Forecasts Figures and Data' #
           ######################################################
           box(
             
             # Title of box
             title = "Formatted Forecasts",
             
             # Width of box 
             width = 12, 
             
             ########################################
             # Creating a title for the panel plots #
             ########################################
             fluidRow(
               
               # Width of row
               width = 12,
               
               # Alignment column 
               column(
                 
                 # Width 
                 width = 12, 
               
                 ##############################
                 # Rendering title for panels #
                 ##############################
                 textOutput("panelForecastTitle")
                 
               ) # End of column 
                 
             ), # End of title row
             
             ###################################
             # Creating the forecast figure(s) #
             ###################################
             fluidRow(
               
               # Width of row
               width = 12,
               
               # Creating column to keep figure in line
               column(
                 
                 # Width of column
                 width = 12, 
                 
                 # Plotting the time series plot 
                 plotlyOutput("Forecast.Figure")
                 
                 ) # End of column 
               
               ), # First row that always shows up
             
             #############################################################
             # Row of user-buttons for manipulating the data and figures #
             #############################################################
             fluidRow(
               
               # Width of row
               width = 12,
               
               ################################################
               # Column to keep the non-arrow buttons aligned #
               ################################################
               column(
                 
                 # Width of column
                 width = 10,
                 
                 # Main DIV style for the left-hand buttons
                 div(style = "display:flex; vertical-aline: top",
                     
                     ################################
                     # Creating the download button #
                     ################################
                     div(actionButton("forecastFigure", "Download Forecast Figure(s)", icon = icon("download"), style = "margin-right: 10px;")),
                     
                     #################################################
                     # Creating the action button for figure options #
                     #################################################
                     div(style = "margin-right: 10px;", actionButton("editForecastFigures", "Figure Options")),
                     
                     ##########################################################
                     # Conditional Panel: Creating the See Filter Data Button #
                     ##########################################################
                     conditionalPanel(
                       
                       # Condition
                       condition = "!input.panelModelsForecasts",
                       
                       # Creating the underlying data check box
                       
                       div(style = "margin-right: 10px;", actionButton("filterFormattedForecasts", "Filtering Options")),
                       
                     ),
                
                     #################################
                     # Creating the panel check mark #
                     #################################
                     div(style = "margin-left: 10px;",
                         
                         checkboxInput("panelModelsForecasts", "See Panels", value = F, width = NULL)),
                     
                     ###################################################
                     # Conditional Panel: Creating the See Data Button #
                     ###################################################
                     conditionalPanel(
                       
                       # Condition
                       condition = "!input.panelModelsForecasts",
                       
                       # Creating the underlying data check box
                       
                       div(style = "margin-left: 10px;",
                           checkboxInput("foremattedForecastCheck", "See Data", value = F, width = NULL))
                       
                     )
                     
                 ) # End of left-hand button style 
                 
               ), # End of the first column of the bottom row 
               
               ######################################
               # Creating the left and right arrows #
               ######################################
               column(2,
                      div(style = "display: flex; justify-content: flex-end; align-items: center;",
                          actionButton(inputId = "PreviousFigure", label = icon("arrow-left")),
                          actionButton(inputId = "NextFigure", label = icon("arrow-right"))))
               
             ), # End of fluid-row for individual/panel figure related options 
             
             ###########################################
             # Conditional Panel: Data related options #
             ###########################################
             conditionalPanel(
               
               # Condition
               condition = "input.foremattedForecastCheck & !input.panelModelsForecasts",
               
               ##############################################
               # Creating the table for formatted forecasts #
               ##############################################
               fluidRow(
                 
                 # Width of row
                 width = 12,
                 
                 # Creating a column to have everything aligned
                 column(
                   
                   # Width of column
                   width = 12,
                   
                   # Title for box
                   textOutput("Formatted.ForecastTitle"),
                   
                   # Outputting the forecast table
                   dataTableOutput("Formatted.Forecast")
                   
                 ) # End of column
                 
               ), # End of fluid row
               
               ############################################################
               # Creating the download button for the formatted forecasts #
               ############################################################
               fluidRow(
                 
                 # With of row
                 width = 12,
                 
                 # Creating the download button
                 column(
                   
                   # Width of column
                   width = 12,
                   
                   # Creating and formatting button
                   div(style = "display: flex; justify-content: flex-start; align-items: center;",
                       downloadButton("download_FormatttedForecasts", "Download Forecast Data"))
                   
                 ) # End of button column
                 
               ) # End of fluid row for buttons 
               
             ) # End of conditional panel for data options 
             
           ) # End of top row box
           
           ) # End of main column 
    
    ), # End of fluid row 


#------------------------------------------------------------------------------#
# Second Row: Quantile Forecasts ----------------------------------------------
#------------------------------------------------------------------------------#

###############################
# Row Two: Quantile Forecasts #
###############################
fluidRow(
  
  # Width of row 
  width = 12, 
  
  # Setting a column to keep everything align
  column(width = 12, 
         
         ############################################
         # Box that contains the quantile forecasts #
         ############################################
         box(
           
           # Title
           title = "Quantile Forecasts",
           
           # Width of box
           width = 12,
           
           ################
           # Row 1: Title #
           ################
           fluidRow(
             
             # Width of row
             width = 12,
             
             ####################
             # Alignment Column #
             ####################
             column(
               
               # Width 
               width = 12, 
               
               # Rendering the title 
               textOutput("quantileTitle")
               
             ) # End of column 
             
           ), # End of title row 
           
          ##############################################################
          # Row 2: Creating the row to fill with the quantile data row #
          ##############################################################
          fluidRow(
            
            # Width of row
            width = 12,
            
            ####################
            # Alignment Column #
            ####################
            column(
              
              # Width of column 
              width = 12,
              
              # Rendering the data 
              style = "overflow-x: auto;", dataTableOutput("quantileForecasts")
              
              ) # End of alignment column 
            
            ), # End of row creating data 
          
          #################################################################
          # Creating the options - Download button, locations, and arrows #
          #################################################################
          fluidRow(
            
            ####################
            # Alignment column #
            ####################
            column(
              
              # Width of column 
              width = 10,
              
              ########################
              # Overall style of row #
              ########################
              div(style = "display:flex; vertical-aline: top",
                  
                  # Rendering the download button 
                  div(downloadButton("download_quantile_forecasts", "Download Quantile Forecasts"), style = "margin-right:10px"),
                  
                  div(style = "margin-right: 10px;", actionButton("filterQuantileForecasts", "Filtering Options"))
                  
              ) # End of main style
              
            ), # End of column for left-aligned buttons
            
            ##########################################
            # Creating the previous and next buttons #
            ##########################################
            column(
              
              # Width 
              width = 2,
              
              # Creating the buttons 
              div(style = "display: flex; justify-content: flex-end; align-items: center;",
                  actionButton(inputId = "PreviousQuan", label = icon("arrow-left")),
                  actionButton(inputId = "NextQuan", label = icon("arrow-right"))))
            
          ) # End of 'fluidRow' creating the buttons 
          
         ) # End of box creating the quantile forecast
         
  ) # End of column
  
), # End of "quantile" row
            
#------------------------------------------------------------------------------#
#  Row 3: Timeseries Plot ------------------------------------------------------
#------------------------------------------------------------------------------#
fluidRow(
  
  # Width
  width = 12,
  
  # Alingment Column
  column(
    
    # Width 
    width = 12,
    
    ##############################################################
    # Row 1: Rendering the panel and individual forecast figures #
    ##############################################################
    box(
      
      # Width of the box 
      width = 12, 
      
      # Title
      title = "Time-series", 
      
      #########################################
      # Working with the time series plot row #
      #########################################
      fluidRow(
        
        # Width of row
        width = 12,
        
        ####################
        # Alignment column #
        ####################
        column(
          
          # Width 
          width = 12, 
          
          # Plotting the time series plot 
          plotlyOutput("timeseriesPlot")
          
        ) # End of alignment column 
        
      ), 
      
      ###########################################
      # Row for Buttons: Download and CheckBoxs #
      ###########################################
      fluidRow(
        
        # Width 
        width = 12,
        
        ####################
        # Alignment column #
        ####################
        column(
          
          # Width
          width = 12,
          
          ########################
          # Overall style of row #
          ########################
          div(style = "display:flex; vertical-aline: top",
              
              # Download button for figure time-series
              div(style = "margin-right: 10px", actionButton("figureTimeseries", "Download Timeseries Figure", icon = icon("download"))),
              
              # Edit legend names 
              div(style = "margin-right: 10px", actionButton("editLegendLabels", "Figure Options")), 
              
              # Check-mark to see data
              div(style = "margin-right: 10px", checkboxInput("timeseriesCheckBox", "Show Underlying Data"))
              
          ) # End of main style row
          
        ) # End of alignment column
        
      ), # End of row for figure options
      
      #####################################################
      # Showing the data rather than the time series plot #
      #####################################################
      conditionalPanel(
        
        # Condition
        condition = "input.timeseriesCheckBox",
        
      fluidRow(
        
        # Width of row
        width = "100%",
        
        ####################
        # Alignment column #
        ####################
        column(
          
          # Width
          width = 12, 
          
          # Plotting the time series plot 
         dataTableOutput("timeseries") , 
          
          div(style = "display: flex; justify-content: flex-start; align-items: center;",
              uiOutput("downloadTimeseries")))
        
      )
        
      ) # End of fluidRow
      
    ) # End of tabbed box
    
  ) # End of alignment column
  
) # End of fluidRow 3
      

), # End of conditional panel for page one 

      
#------------------------------------------------------------------------------#
# Page 2: Forecasting and Model Fit Metrics ------------------------------------
#------------------------------------------------------------------------------#
# About: This section shows the model fit and forecasting metrics (MSE, MAE,   #
# WIS, 95% PI, Skill Scores and X.                                             #
#------------------------------------------------------------------------------#

#####################################
# Only runs if page two is selected #
#####################################
conditionalPanel(
  
  # Condition 
  condition = "input.my_picker == 'Model Metrics'", 
  
  ######################################################
  # Setting a standard column width for the whole page #
  ######################################################
  column(
    
    # Column width 
    width = 12,
    
    ###########################################
    # First Row of the page - Average Metrics #
    ###########################################
    fluidRow(

      #################################################
      # Creating a box for avgerage model fit metrics #
      #################################################
      box(

        # Setting the box width
        width = 12,
        
        # Title
        title = "Average Metrics",
    
        ############################################
        # First row of the box - Showing the Title #
        ############################################
        fluidRow(
          
          # Width of row
          width = 12,
          
          ####################
          # Alignment column #
          ####################
          column(
            
            # Width
            width = 12, 
          
            ##################################################
            # Conditional Panel: Show title for Average Data #
            ##################################################
            conditionalPanel(
              
              # Condition
              condition = "!input.AvgFigure",
              
              # Title for Box
              textOutput("AvgMetricsTitle")
              
            ), # End of conditional panel
            
            #####################################################
            # Conditional Panel: Show title for Average Figures #
            #####################################################
            conditionalPanel(
              
              # Condition
              condition = "input.AvgFigure",
              
              # Title for Box
              textOutput("AvgFigTitle")
              
            ) # End of conditional panel
            
          ) # Column alignment 
          
        ), # End of 'fluidRow'
        
        ########################################
        # Second Row of the box - Plot or Data #
        ########################################
        fluidRow(
          
          # Creating the column to keep everything align
          column(
            
            # Width of column
            width = 12,
            
            ################################################
            # Conditional Panel: Show Average Metrics Data #
            ################################################
            conditionalPanel(
              
              # Condition
              condition = "!input.AvgFigure",
              
              # Outputting the forecast table
              dataTableOutput("AvgMetricsData"),
              
            ), # End of conditional panel - Check not hit
            
            ##################################################
            # Conditional Panel: Show Average Metrics Figure #
            ##################################################
            conditionalPanel(
              
              # Condition
              condition = "input.AvgFigure",
              
              plotOutput("AvgMetricsFigure")
              
            ) # End of condition - Check hit
            
          ) # End of column
          
        ), # End of fluid row
        
        ###################################
        # Row 3: Showing the user options #
        ###################################
        fluidRow(
          
          # Width 
          width = 12,
          
          ####################
          # Alignment column #
          ####################
          column(
            
            # Width
            width = 10,
            
            ########################
            # Overall style of row #
            ########################
            div(style = "display:flex; vertical-aline: top",
                
                ####################################
                # Conditional Panel: Download data #
                ####################################
                conditionalPanel(
                  
                  # Condition
                  condition = "!input.AvgFigure",
                  
                  # Row style 
                  div(style = "display:flex; vertical-aline: top",
                  
                    # Creating the download button 
                    div(style = "margin-right: 10px",
                        downloadButton("download_AvgMetrics", "Download Average Metrics")),
                    
                    # Filtering data 
                    div(style = "margin-right: 10px;", actionButton("filterAvgMetrics", "Filtering Options"))
                    
                  )
                  
                ), # End of condition
                
                ######################################
                # Conditional Panel: Download Figure #
                ######################################
                conditionalPanel(
                  
                  # Condition
                  condition = "input.AvgFigure", 
                  
                  # Aligning buttons
                  div(style = "display:flex; vertical-aline: top",
                  
                    #Download Button
                    div(style = "margin-right: 10px",
                        actionButton("download_AvgmetricsFig", "Download Average Metrics Figure", icon = icon("download"))),
                    
                    # Edit figures button
                    div(style = "margin-right: 10px", actionButton("editAvgMetrics", "Figure Options")) 
                  
                  )
                  
                ), # End of condition
                
                #########################
                # Show figure check-box #
                #########################
                div(checkboxInput("AvgFigure", "Show Figure"))
                
            ) # End of main-style
            
          ) # End of alignment-column one 
          
        ) # End of 'fluidRow' for buttons 
        
      ) # End of 'column' for average metrics box 
      
    ), # End of fluid row for first row of page

#------------------------------------------------------------------------------#    
# Crude Metrics Box ------------------------------------------------------------
#------------------------------------------------------------------------------#

    fluidRow(
      
      ##############################################
      # Creating a box for crude model fit metrics #
      ##############################################
      box(
        
        # Title
        title = "Crude Metrics",
        
        # Setting the box width
        width = 12,
        
        ############################################
        # First row of the box - Showing the Title #
        ############################################
        fluidRow(
          
          # Width of row
          width = 12,
          
          ####################
          # Alignment Column #
          ####################
          column(
            
            # Column width 
            width = 12,
            
            #################################
            # Conditional panel: Data title #
            #################################
            conditionalPanel(
              
              # Condition
              condition = "input.crudeFigure",
              
              # Title for Box
              textOutput("CrudeMetricsTitle")
              
            ), # End of conditional panel
            
            ###################################
            # Conditional panel: Figure title #
            ###################################
            conditionalPanel(
              
              # Condition
              condition = "input.crudeFigure",
              
              # Title for Box
              textOutput("crudeFigTitle")
              
            ) # End of conditional panel
            
          ) # End of alignment column 
          
          ), # End of 'fluidRow' for title 
        
        ########################################
        # Second Row of the box - Plot or Data #
        ########################################
        fluidRow(
          
          # Creating the column to keep everything align
          column(
            
            # Width of column
            width = 12,
            
            ################################
            # Conditional Panel: Show Data #
            ################################
            conditionalPanel(
              
              # Condition
              condition = "!input.crudeFigure",
              
              # Outputting the forecast table
              dataTableOutput("CrudeMetricsData"),
              
            ), # End of conditional panel - Check not hit
            
            ##################################
            # Conditional Panel: Show Figure #
            ##################################
            conditionalPanel(
              
              # Condition
              condition = "input.crudeFigure",
              
              # Outputting the figure 
              plotOutput("CrudeMetricsFigure")
              
            ) # End of condition - Check hit
            
          ) # End of column
          
        ), # End of fluid row
        
        ###################################
        # Row 3: Showing the user options #
        ###################################
        fluidRow(
          
          # Width 
          width = 12,
          
          ####################
          # Alignment column #
          ####################
          column(
            
            # Width
            width = 10,
            
            ########################
            # Overall style of row #
            ########################
            div(style = "display:flex; vertical-aline: top",
                
                ############################################
                # Condition: Show Download Button for Data #
                ############################################
                conditionalPanel(
                  
                  # Condition
                  condition = "!input.crudeFigure",
                  
                  # Row column
                  div(style = "display:flex; vertical-aline: top",
                      
                    # Download Button
                    div(style = "margin-right: 10px",
                        downloadButton("download_metrics", "Download Crude Metrics")),
                        
                    # Filtering data 
                    div(style = "margin-right: 10px;", actionButton("filterCrudeMetrics", "Filtering Options"))    
                    
                  )
                  
                ), # End of condition
                
                ##############################################
                # Condition: Show Download Button for Figure #
                ##############################################
                conditionalPanel(
                  
                  # Condition
                  condition = "input.crudeFigure",
                  
                  # Aligning buttons
                  div(style = "display:flex; vertical-aline: top",
                  
                    # Download Button
                    div(style = "margin-right: 10px",
                        actionButton("download_metricsFig", "Download Crude Metrics Figure", icon = icon("download"))),
                    
                    # Edit figures button
                    div(style = "margin-right: 10px", actionButton("figOptCRUDEMetrics", "Figure Options"))
                    
                  )
                  
                ), # End of condition
                
                #########################
                # Show figure check-box #
                #########################
                div(checkboxInput("crudeFigure", "Show Figure"))
                
            ) # End of Main Style 
            
          ), # End of alignment for right-hand buttons
          
          ##########################################
          # Condition Panel: Figure Metrics Arrows #
          ##########################################
          conditionalPanel(
            
            # Condition
            condition = "input.crudeFigure",
            
            # Left and right arrows
            column(2,
                   div(style = "display: flex; justify-content: flex-end; align-items: center;",
                       actionButton(inputId = "PreviousMetricFigure", label = icon("arrow-left")),
                       actionButton(inputId = "NextMetricFigure", label = icon("arrow-right"))))
            
          ) # End of conditional panel
          
        ) # End of 'fluidRow'
        
      ) # End of 'column' for crude box
      
    ), # End of crude metrics box
    
#------------------------------------------------------------------------------#
# Box 3: Winkler Scores Box ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the box to host the Winkler Scores data, along   #
# with the associated figure and user options.                                 #
#------------------------------------------------------------------------------#

##########################################
# Row 3 (Page) : Overall row of the page #
##########################################
fluidRow(

  ###########################################
  # Creating the box for Winkler Score Data #
  ###########################################
  tabBox(
    
    # Box title 
    title = "", 
    
    # ID of the box 
    id = "winklerMeasures",
    
    # Width of the box 
    width = 12,
    
    #####################################
    # Rendering the Winkler Scores Data #
    #####################################
    tabPanel(id = "winklerScoresData",
             
             # Title of box 
             title = "Winkler Scores", 
             
             ############################################
             # Row 1: Rendering the Winkler Scores Data #
             ############################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column(
                 
                 # Width of column 
                 width = 12, 
                 
                   # Rendering the data frame
                   dataTableOutput("winklerDataTableAGGP")
                 
               ) # End of alignment column 
               
             ), # End of first row of data tab 
             
             ####################################
             # Row 2: Creating the user options #
             ####################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column( 
                 
                 # Column width 
                 width = 12, 
                 
                 # Overall style for row 
                 div(style = "display:flex; vertical-aline: top",
                     
                     #######################################
                     # Creating the download button - Data #
                     #######################################
                     div(downloadButton("downloadWinkerData", "Download Scores", style = "margin-right: 10px")),
                     
                     ###################################
                     # Creating the data filter option #
                     ###################################
                     div(style = "margin-right: 10px", actionButton("filterWinklerDataMain", "Filtering Options")), 
                     
                     #########################################################
                     # Creating the check-mark to see average Winkler Scores #
                     #########################################################
                     div(checkboxInput("seeAverageWinklerMain", "Use Average Metrics"))
                     
                 ), # End of style for row 
                 
               ) # End of alignment column
               
             ) # End of Row 2
             
    ), # End of Winkler data tab 
    
    #######################################
    # Rendering the Winkler Scores Figure #
    #######################################
    tabPanel(id = "winklerScoresFigure",
             
             # Title of box 
             title = "Figure(s)", 
             
             ##############################################
             # Row 1: Rendering the Winkler Scores Figure #
             ##############################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column(
                 
                 # Width of column 
                 width = 12, 
                 
                 # Rendering the figure
                 plotOutput("winklerFigureAGGP")
                 
               ) # End of alignment column 
               
             ), # End of first row of data tab 
             
             ####################################
             # Row 2: Creating the user options #
             ####################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column( 
                 
                 # Column width 
                 width = 12, 
                 
                 # Overall style for row 
                 div(style = "display:flex; vertical-aline: top",
                     
                     #########################################
                     # Creating the download button - Figure #
                     #########################################
                     div(actionButton("downloadWinklerMainFig", "Download Figures", style = "margin-right: 10px"))
                     
                 ) # End of style for row 
                 
               ) # End of alignment column
               
             ) # End of Row 2
             
    ) # End of Winkler figure tab 
    
  ) # End of Winkler Box
  
),  # End of Row containing Winkler Scores

#------------------------------------------------------------------------------#
# Box 4: Skill Scores Box ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the box to host the Skill Scores data, along     #
# with the associated figure and user options.                                 #
#------------------------------------------------------------------------------#

##########################################
# Row 3 (Page) : Overall row of the page #
##########################################
fluidRow(
  
  #########################################
  # Creating the box for Skill Score Data #
  #########################################
  tabBox(
    
    # Box title 
    title = "", 
    
    # ID of the box 
    id = "SkillScoresMeasures",
    
    # Width of the box 
    width = 12,
    
    ###################################
    # Rendering the Skill Scores Data #
    ###################################
    tabPanel(id = "winklerScoresData",
             
             # Title of box 
             title = "Skill Scores", 
             
             ##########################################
             # Row 1: Rendering the Skill Scores Data #
             ##########################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column(
                 
                 # Width of column 
                 width = 12, 
                 
                 # Rendering the data frame
                 dataTableOutput("skillScoresAGGPData")
                 
               ) # End of alignment column 
               
             ), # End of first row of data tab 
             
             ####################################
             # Row 2: Creating the user options #
             ####################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column( 
                 
                 # Column width 
                 width = 12, 
                 
                 # Overall style for row 
                 div(style = "display:flex; vertical-aline: top",
                     
                     #######################################
                     # Creating the download button - Data #
                     #######################################
                     div(style = "margin-right: 10px", downloadButton("downloadSSMetrics", "Download Skill Scores")),
                     
                     ###################################
                     # Creating the data filter option #
                     ###################################
                     div(style = "margin-right: 10px", actionButton("filterSSDataMain", "Filtering Options")), 
                     
                     ##################################################################
                     # Creating the check-mark to use average metrics in skill Scores #
                     ##################################################################
                     div(checkboxInput("seeAvgSS", "Use Average Metrics"))
                     
                 ), # End of style for row 
                 
               ) # End of alignment column
               
             ) # End of Row 2
             
    ), # End of Skill Scores data tab 
    
    #####################################
    # Rendering the Skill Scores Figure #
    #####################################
    tabPanel(id = "SkillScoresFigure",
             
             # Title of box 
             title = "Figure(s)", 
             
             ############################################
             # Row 1: Rendering the Skill Scores Figure #
             ############################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column(
                 
                 # Width of column 
                 width = 12, 
                 
                 "This feature is not yet available.",
                 
                 # Rendering the figure
                 plotOutput("SSFigureAGGP")
                 
               ) # End of alignment column 
               
             ), # End of first row of data tab 
             
             ####################################
             # Row 2: Creating the user options #
             ####################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column( 
                 
                 # Column width 
                 width = 12, 
                 
                 # Overall style for row 
                 div(style = "display:flex; vertical-aline: top",
                     
                     #########################################
                     # Creating the download button - Figure #
                     #########################################
                     div(actionButton("downloadSSAGGP", "Download Figures", style = "margin-right: 10px")),
                     
                     ####################################
                     # Creating the edit figures button #
                     ####################################
                     div(style = "margin-right: 10px", actionButton("figureOptionsSSMain", "Figure Options")), 
                     
                     #######################################################################
                     # Creating the check-mark to use the average metrics for skill scores #
                     #######################################################################
                     div(checkboxInput("seeAverageSSMainFIG", "Use Average Metrics"))
                     
                 ) # End of style for row 
                 
               ) # End of alignment column
               
             ) # End of Row 2
             
    ) # End of Winkler figure tab 
    
  ) # End of Winkler Box
  
),  # End of Row containing Winkler Scores


  ) # Column alignment overall

), # End of Page 2 conditional panel 
             
              

#------------------------------------------------------------------------------#
# Page 3: Handling the other forecasts and metrics -----------------------------
#------------------------------------------------------------------------------#
# About: This section handles outside models and compares them against the     #
# calculated dashboard metrics.                                                #
#------------------------------------------------------------------------------#

#####################################
# Only runs if page two is selected #
#####################################
conditionalPanel(
  
  # Condition 
  condition = "input.my_picker == 'Model Comparison'", 
  

  ##############################################################
  # Row 1: Rendering the panel and individual forecast figures #
  ##############################################################
  tabBox(
    
    # Box title 
    title = NULL, 
    
    # ID of the box 
    id = "box1",
    
    # Width of the box 
    width = 12, 
    
    ####################################
    # Rendering the individual figures #
    ####################################
    tabPanel(id = "individualFigsOther",
             
             # Title of box 
             title = "Individual Figures", 
               
               ###################################################
               # Row 1: Rendering the individual figure forecast #
               ###################################################
               fluidRow(
                 
                 ####################
                 # Alignment column #
                 ####################
                 column(
                   
                 width = 12, 
                 
                   # Plot title
                   textOutput("OtherForecastTitle"), 
        
                   # Rendering the data frame
                   plotlyOutput("otherModelFigure")
                 
                 )
                
              ),
      
              ####################################
              # Row 2: Creating the user options #
              ####################################
              fluidRow(
                
                ####################
                # Alignment column #
                ####################
                column( 
                  
                  # Column width 
                  width = 12, 
                  
                  # Overall style for row 
                  div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                      
                      ##########################################
                      # Creating the download and edit buttons #
                      ##########################################
                      div(style = "display:flex; vertical-aline: top",
                          
                          # Download button for figure for panel forecasts - Other 
                          div(actionButton("downloadOtherForecastsFigs", "Download Figures", style = "margin-right: 10px")),
                          
                          # Edit legend names 
                          div(style = "margin-right: 10px", actionButton("editLegendLabelsOther", "Figure Options")), 
                          
                      ),
                      
                      #######################
                      # Creating the arrows #
                      #######################
                      div(
                        
                            style = "display: flex; justify-content: flex-end; align-items: center;",
                            actionButton(inputId = "otherFigsPrevious", label = icon("arrow-left")),
                            actionButton(inputId = "otherFigstNext", label = icon("arrow-right"))
                            
                            )
                      
                      ) # End of overall row style 
                  
                  ) # End of alignment column 
                
                ) # End of options row
             
             ), # End of tab one - individual figures
    
    ############################################
    # Rendering the individual forecast figure #
    ############################################
    tabPanel(id = "panelFigsOther",
             
             # Title for box
             title = "Panel Figures",
             
             ##############################################
             # Row 1: Rendering the panel figure forecast #
             ##############################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column(
                 
                 width = 12,
                 
                 # Rendering the title
                 textOutput("OtherPanelForecastTitle"),
                 
                 # Rendering the data frame
                 plotOutput("otherModelPanelFigure")
                 
               )
               
             ),
             
             ####################################
             # Row 2: Creating the user options #
             ####################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column( 
                 
                 # Column width 
                 width = 12, 
                 
                 # Overall style for row 
                 div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                     
                     ################################
                     # Creating the download button #
                     ################################
                     div(style = "display:flex; vertical-aline: top",
                         
                         # Download button for figure for panel forecasts - Other 
                         div(actionButton("downloadOtherForecastsPanels", "Download Figures", style = "margin-right: 10px")),
                         
                         # Edit legend names 
                         div(style = "margin-right: 10px", actionButton("editLegendLabelsOtherPanel", "Figure Options")), 
                         
                     ),
                     
                     #######################
                     # Creating the arrows #
                     #######################
                     div(
                       
                       style = "display: flex; justify-content: flex-end; align-items: center;",
                       actionButton(inputId = "otherFigsPanelsPrevious", label = icon("arrow-left")),
                       actionButton(inputId = "otherFigsPanelsNext", label = icon("arrow-right"))
                       
                     )
                     
                 ) # End of overall row style 
                 
               ) # End of alignment column 
               
             ) # End of options row
             
             ) # End of forecast figures panel
    
    ), # End of first tab box - forecasts
  
#------------------------------------------------------------------------------#
# Creating the average metrics box ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the metrics box that shows the average metrics   #
# along with filtering options and ability to create images.                   #  
#------------------------------------------------------------------------------#
  tabBox(
    
    # Box title 
    title = NULL, 
    
    # ID of the box 
    id = "box2",
    
    # Width of the box 
    width = 12, 
    
    #########################################
    # Creating the average metrics Data Tab #
    ########################################
    tabPanel(id = "averageMetricsOtherData",
             
             # Title for box
             title = "Average Metrics",
             
             #####################################
             # First row of the box - Data table #
             #####################################
             fluidRow(
               
               # Alignment column
               column(
                 
                 # Width of column
                 width = 12,
                 
                 # Rendering the data table
                 dataTableOutput("AverageMetricsOther")
                 
               )
               
             ),
             
             ##########################################################
             # Second row of box - Options for filtering, downloading #
             ##########################################################
             fluidRow(
               
               # Alignment column
               column(
                 
                 # Width of column
                 width = 12,
                 
                 # Overall style for row 
                 div(style = "display:flex; vertical-aline: top",
                     
                     ############################################
                     # Creating the download and filter buttons #
                     ############################################
                     
                     # Download button for the combined crude metrics 
                     div(downloadButton("downloadAverageMetrics", "Download Avg. Metrics", style = "margin-right: 10px")),
                     
                     # Filtering for the crude metrics 
                     div(style = "margin-right: 10px", actionButton("filterAvgCombinedMetrics", "Filtering Options")), 
                     
                 ) # End of overall style for row 
                 
               ) # End of alignment column 
               
             ) # End of row with filtering options 
             
    ), # End of 'tabPanel' for the crude metrics data 
    
    ############################################
    # Creating the average metrics figures tab #
    ############################################
    tabPanel(id = "AverageMetricsOtherFigure",
             
             # Title for box
             title = "Figure",
             
             #################################
             # First row of the box - Figure #
             #################################
             fluidRow(
               
               # Alignment column
               column(
                 
                 # Width of column
                 width = 12,
                 
                 # Rendering the title
                 textOutput("avgMetricOtherPanelTitle"),
                 
                 # Creating the plot
                 plotOutput("avgMetricOtherPanel")
                 
               )
               
             ),
             
             ##########################################################
             # Second row of box - Options for filtering, downloading #
             ##########################################################
             fluidRow(
               
               # Alignment column
               column(
                 
                 # Width of column
                 width = 12,
                 
                 #############################
                 # Overall style for the row #
                 #############################
                 div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;", 
                     
                 
                     ############################################
                     # Creating the download and filter buttons #
                     ############################################
                     div(style = "display:flex; vertical-aline: top",
                    
                     # Download button for the combined crude metrics figure
                     div(actionButton("downloadOtherCombinedMetricsFigAvg", "Download Figures", style = "margin-right: 10px")),
                     
                     # Filtering for the crude metrics 
                     div(style = "margin-right: 10px", actionButton("figOptCombAvgMetrics", "Figure Options"))
                     
                     ), # End of download and filter button
                     
                     #############################################
                     # Creating the forward and backwards arrows #
                     #############################################
                     div(
                       
                       style = "display: flex; justify-content: flex-end; align-items: center;",
                       actionButton(inputId = "otheravgMetricPanelsPrevious", label = icon("arrow-left")),
                       actionButton(inputId = "otheravgMetricPanelsNext", label = icon("arrow-right"))
                       
                     ) # End of arrow format
                     
                 ) # End of overall style for row 
                 
               ) # End of alignment column 
               
             ) # End of row with figure options 
             
    ), # End of 'tabPanel' for the crude metrics figure 
    
  ), # End of 'tabbox' for the crude metrics 
  
  
#------------------------------------------------------------------------------#
# Creating the crude metrics box -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the metrics box that shows the crude metrics     #
# along with filtering options and ability to create images.                   #  
#------------------------------------------------------------------------------#
tabBox(
  
  # Box title 
  title = NULL, 
  
  # ID of the box 
  id = "box2",
  
  # Width of the box 
  width = 12, 
  
  #######################################
  # Creating the Crude metrics Data Tab #
  #######################################
  tabPanel(id = "crudeMetricsOtherData",
           
           # Title for box
           title = "Crude Metrics",
           
           #####################################
           # First row of the box - Data table #
           #####################################
           fluidRow(
             
             # Alignment column
             column(
               
               # Width of column
               width = 12,
               
               # Rendering the data table
               dataTableOutput("crudeMetricsOther")
               
             )
             
             ),
           
           ##########################################################
           # Second row of box - Options for filtering, downloading #
           ##########################################################
           fluidRow(
             
             # Alignment column
             column(
               
               # Width of column
               width = 12,
               
               # Overall style for row 
               div(style = "display:flex; vertical-aline: top",
                   
                   ############################################
                   # Creating the download and filter buttons #
                   ############################################
                   
                   # Download button for the combined crude metrics 
                   div(downloadButton("downloadCrudeMetrics", "Download Crude Metrics", style = "margin-right: 10px")),
                   
                   # Filtering for the crude metrics 
                   div(style = "margin-right: 10px", actionButton("filterCrudeCombinedMetrics", "Filtering Options")), 
                   
                   ) # End of overall style for row 
               
               ) # End of alignment column 
             
             ) # End of row with filtering options 
           
           ), # End of 'tabPanel' for the crude metrics data 
  
  ##########################################
  # Creating the Crude metrics figures tab #
  ##########################################
  tabPanel(id = "crudeMetricsOtherFigure",
           
           # Title for box
           title = "Figure",
           
           #################################
           # First row of the box - Figure #
           #################################
           fluidRow(
             
             # Alignment column
             column(
               
               # Width of column
               width = 12,
               
               # Rendering the title
               textOutput("crudeMetricOtherPanelTitle"),
               
               # Creating the plot
               plotOutput("crudeMetricOtherPanel")
               
             )
             
           ),
           
           ##########################################################
           # Second row of box - Options for filtering, downloading #
           ##########################################################
           fluidRow(
             
             # Alignment column
             column(
               
               # Width of column
               width = 12,
               
               #############################
               # Overall style for the row #
               #############################
               div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;", 
                 
                 ############################################
                 # Creating the download and filter buttons #
                 ############################################
                 div(style = "display:flex; vertical-aline: top",
                     
                     # Download button for the combined crude metrics figure
                     div(actionButton("downloadOtherCombinedMetricsFig", "Download Figures", style = "margin-right: 10px")),
                     
                     # Filtering for the crude metrics 
                     div(style = "margin-right: 10px", actionButton("figOptCombCrudeMetrics", "Figure Options"))
                     
                 ),
               
                 #############################################
                 # Creating the forward and backwards arrows #
                 #############################################
                 div(
                   
                   style = "display: flex; justify-content: flex-end; align-items: center;",
                   actionButton(inputId = "otherCrudeMetricPanelsPrevious", label = icon("arrow-left")),
                   actionButton(inputId = "otherCrudeMetricPanelsNext", label = icon("arrow-right"))
                   
                 )
                 
               ) # End of style for row 
               
             ) # End of alignment column 
             
           ) # End of row with figure options 
           
  ), # End of 'tabPanel' for the crude metrics figure 
  
), # End of 'tabbox' for the crude metrics 

#------------------------------------------------------------------------------#
# Creating the Winkler Score and Skill Score Box -------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the skill scores and Winkler scores data.        #
#------------------------------------------------------------------------------#
tabBox(
  
  # Box title 
  title = NULL, 
  
  # ID of the box 
  id = "box3",
  
  # Width of the box 
  width = 12, 
  
  ########################################
  # Creating the Winkler Scores Data Box #
  ########################################
  tabPanel(id = "WinklerScoresOtherData",
           
           # Title for box
           title = "Winkler Scores",
           
           #####################################
           # First row of the box - Data table #
           #####################################
           fluidRow(
             
             # Alignment column
             column(
               
               # Width of column
               width = 12,
               
               # Rendering the data table
               dataTableOutput("winklerScoresOther")
               
             )
             
           ),
           
           ##########################################################
           # Second row of box - Options for filtering, downloading #
           ##########################################################
           fluidRow(
             
             # Alignment column
             column(
               
               # Width of column
               width = 12,
               
               # Overall style for row 
               div(style = "display:flex; vertical-aline: top",
                   
                   ############################################
                   # Creating the download and filter buttons #
                   ############################################
                   
                   # Download button for the combined crude metrics 
                   div(style = "margin-right: 10px", downloadButton("downloadWinklerMetrics", "Download Winkler Scores")),
                   
                   # Filtering for the crude metrics 
                   div(style = "margin-right: 10px", actionButton("filterWinklerOtherMetrics", "Filtering Options")), 
                   
                   # Check-mark for average Winkler scores
                   div(checkboxInput("winklerOtherAvg", "See Avg. Winkler Scores"))
                   
               ) # End of overall style for row 
               
             ) # End of alignment column 
             
           ) # End of row with filtering options 
           
  ), # End of 'tabPanel' for the crude metrics data 
  
  ######################################
  # Creating the Skill Scores Data Box #
  ######################################
  tabPanel(id = "skillScoresOtherData",
           
           # Title for box
           title = "Skill Scores",
           
           #####################################
           # First row of the box - Data table #
           #####################################
           fluidRow(
             
             # Alignment column
             column(
               
               # Width of column
               width = 12,
               
               # Rendering the data table
               dataTableOutput("skillScoresOtherOUTPUT")
               
             )
             
           ),
           
           ##########################################################
           # Second row of box - Options for filtering, downloading #
           ##########################################################
           fluidRow(
             
             # Alignment column
             column(
               
               # Width of column
               width = 12,
               
               # Overall style for row 
               div(style = "display:flex; vertical-aline: top",
                   
                   ############################################
                   # Creating the download and filter buttons #
                   ############################################
                   
                   # Download button skill scores 
                   div(style = "margin-right: 10px", downloadButton("downloadSSMetricsOther", "Download Skill Scores")),
                   
                   # Filtering for the skill scores
                   div(style = "margin-right: 10px", actionButton("filterSkillScoresOtherMetrics", "Filtering Options")), 
                   
                   # Check-mark for average skill scores
                   div(checkboxInput("seeAvgSSOther", "See Avg. Skill Scores"))
                   
               ) # End of overall style for row 
               
             ) # End of alignment column 
             
           ) # End of row with filtering options 
           
  ), # End of 'tabPanel' for the skill scores data 
  
) # End of 'tabbox' for the skill scores

               
               
                   
                   
) # End of other page 
    

) # End of dashboard body

) # End of user interface 








# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
#------------------------------------------------------------------------------#
# Clearing some settings -------------------------------------------------------
#------------------------------------------------------------------------------#
  
  ##############################################################
  # Resetting the check-box for Winkler scores when run is hit #
  ##############################################################
  observeEvent(input$run, {
 
    # Isolating the behavior to only when run is hit 
    isolate({
      
      # Updating the value of the check-box
      updateCheckboxInput(session, "WinklerFigure", value = FALSE)
      
    })
    
  }) # End of 'observeEvent'
  
#------------------------------------------------------------------------------#
# Error to appear when using smoothing and Prophet model -----------------------
#------------------------------------------------------------------------------#
# About: This section returns a message if smoothing data and have the model   #
# Prophet selected.                                                            #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    ###############################
    # Running if not error occurs #
    ###############################
    tryCatch({
      
      # Returning error if smoothing and Prophet model 
      if(input$smoothingInput > 1 & "Prophet" %in% c(input$modelType)){
        
        shinyalert("The data smoothing will not be applied to the calibration period
                   for the Prophet model. It will apply for any other selected model.", 
                   type = "info")
        
      }
      
    ###########################
    # Returns if error occurs #
    ###########################
    }, error = function(e){
      
      NULL
      
    })
    
  }) # End of tryCatch
   
#------------------------------------------------------------------------------#
# Reading in the data frame ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section reads in the user-selected data frame from the  #
# working directory. It then saves the data under the reactive element 'file'. #   
# The file is selected using the 'fileInput' picker.                           #
#------------------------------------------------------------------------------#
  
  ###################################
  # Reactive value to track changes #
  ###################################
  crudeDataChanger <- reactiveValues()
  
  file <- reactive({

    tryCatch({
    
    ############################################
    # Name of file from the 'fileInput' picker #
    ############################################
    file1 <- input$dataset
    
    # Saving in reactive value
    crudeDataChanger$data <- file1
    
    #########################################
    # Extracting the extension of file name #
    #########################################
    ext <- tools::file_ext(file1$datapath)
    
    ######################################################
    # Produces an error if a '.csv' file is not selected #
    ######################################################
    if (ext != "csv") {
      
      # Produced error
      showModal(modalDialog(
        title = "Error",
        "Please upload a '.csv' file. ",
        easyClose = TRUE
      ))
      
      # Return null so user has to re-upload
      return(NULL)
      
    }
    
    #######################
    # Reading in the data #
    #######################
    return(read.csv(file1$datapath, header = T, check.names=FALSE))
    
    },error = function(e){
      
      NULL
      
    })
    
  }) 
  

#------------------------------------------------------------------------------#
# Adding locations filter to the top of the page -------------------------------
#------------------------------------------------------------------------------#
# This section uses the data from above to determine the possible locations to #
# include in the drop down filter. Users can select more than one location,    #
# and the respective boxes will include information only for those locations.  #
#------------------------------------------------------------------------------#
  
  output$location.selection <- renderUI({
      
    #########################################
    # Fixing issue when no data is selected #
    #########################################
    tryCatch({
      
      if(!is.null(file())){
        
      ##################################################
      # Saving the user selected data under a new name #
      ##################################################
      data <- file()
      
      ###########################################
      # Grabbing the headers from the data file #
      ###########################################
      location.names <- colnames(data)[-1]

      ##########################
      # Creating the drop down #
      ##########################
      return(pickerInput("locations", # ID for calling picker 
                         label = "Location/Group", # Label for picker 
                         choices = c(location.names), # Location choices 
                         multiple = T)) # Allowing more than one choice 
      
      }
      
      ####################################################
      # Picker that pops up until a data set is selected #
      ####################################################
    }, error = function(e){
      
      # Picker input when there is no data selected 
      return(pickerInput("locations", # ID for calling picker
                         label = "Location/Group", # Label for picker 
                         choices = "Please Select a Data Set")) # Choice shown 
      
    }) # End of 'TryCatch' statement 
    
  }) # End of 'renderUI' statement 
  
  
#------------------------------------------------------------------------------#
# Determining the type of time data --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sub-sets the column related to the "time" make-up of the #
# time-series trajectory from the crude.data. It then uses the function        #
# `date.type.function` to determine if the user imported data is "weekly",     #
# "daily", "yearly" or uses a time index (i.e., 1, 2, 3..). Finally, it        #
# returns a string to the variable name `dateType` the corresponds to the time #
# make-up of the data. If there are missing times/dates, the function returns  #
# an error to check the dates for missing information.                         #
#------------------------------------------------------------------------------#
  
  # Initialize reactiveValues to store the dates
  dateValues <- reactiveValues(dates = NULL)
  
  #######################################################
  # Observe changes in the 'file()' reactive expression #
  #######################################################
  observe({
    
    ##############################
    # Runs if 'file' is not NULL #
    ##############################
    tryCatch({
      
      # Data file 
      data <- file()
      
      # Subsetting dates 
      crude.dates <- na.omit(data[, 1])
      
      ###################################
      # Function to determine date type #
      ###################################
      date.return <- date.type.function(dates.input = c(crude.dates))
      
      # Saving the output of the function in the reactive value 
      dateValues$dates <- date.return[1]
   
      ############################################################
      # Showing an error message if there is an issue with dates #
      ############################################################
      if(dateValues$dates %!in% c("year", "day", "index", "week")) {
        
        # Produced error
        showModal(modalDialog(
          title = "Error",
          paste0(dateValues$dates),
          easyClose = TRUE
        ))
        
        # Return null so user has to re-upload
        return(NULL)
      }
      
      ###############################################################
      # Prints NULL in the console if there is no file yet selected #
      ###############################################################
      }, error = function(e){
      
        # Returning NULL
        NULL
        
        }) # End of 'tryCatch' statement 
    
    }) # End of 'observe' statement 
  
  
#------------------------------------------------------------------------------#
# Creating the download button -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the download button for the time series data. It #
# only shows if the check-mark for data is hit. When it is not clicked, the    #
# button does not show up.                                                     #
#------------------------------------------------------------------------------#
  
  ###########################################
  # Observing changes in reactive variables #
  ###########################################
  observe({
    
      ########################################
      # Creating the button to download data #
      ########################################
      output$downloadTimeseries <- renderUI({
        
        # Download button 
        downloadButton("download_timeseries", "Download Timeseries Data")
        
      }) # End of render UI
      
    
  }) # End of observe statement 
  
  
#------------------------------------------------------------------------------#
# Pop-up for the download button for time series figures -----------------------
#------------------------------------------------------------------------------#
# About: This section creates the figure pop-up options for the time series    #
# button. In this pop-up, users have the option to format how the figure is    #
# saved, and the download button is created.                                   #
#------------------------------------------------------------------------------#
  
  #####################################################
  # Setting Figure Specifications - Pop up for figure #
  #####################################################
  observeEvent(input$figureTimeseries, {
    
    ################################
    # Figure specification options #
    ################################
    isolate({
      
      showModal(modalDialog(
        
        title = "Figure Specifications",
        numericInput("dpi", "Figure DPI:", value = 900),
        numericInput("width", "Figure Width:", value = 9),
        numericInput('height', 'Figure Height:', value = 5),
        pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
        pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
        downloadButton("downloadtimeseriesF", "Download Timeseries Figure"),
        easyClose = TRUE))
      
    }) # End of isolate
    
  }) # End of observe-event
  
  
#------------------------------------------------------------------------------#
# Preparing the time-series data -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the data to be shown as the time series data    #
# in the UI for users. The data shown is based on user-selected locations      #
# during an earlier step in work-flow.                                         #
#------------------------------------------------------------------------------#
  
  #############################################
  # Reactive value to save timeseries data in #
  #############################################
  timeseriesData <- reactiveValues()

  ####################################
  # Observing changing in the inputs #
  ####################################
  observe({

    ##############################
    # Runs if a file is selected #
    ##############################
    tryCatch({

      ################################
      # Reading in the original data #
      ################################
      observeEvent(input$run, {data1 <- file()
      
      #############################################################
      # Filtering the data shown based on user selected locations #
      #############################################################

      if(nrow(data1) > 0){
        
          data.for.table <- data1 %>%
            dplyr::select(names(data1)[1], all_of(input$locations))
          
      }else{
        
        data.for.table <- NULL
        
      }
      
      ############################################
      # Saving the results to the reactive value #
      ############################################
      isolate({ timeseriesData$dataList <- data.for.table })
      
      })

        
        
    ###############################
    # Error portion of 'tryCatch' #
    ###############################
    }, error = function(e){

      timeseriesData$dataList <- NULL

    }) # End of 'tryCatch'

  }) # End of 'observe'
  
#------------------------------------------------------------------------------#
# Clearing the time series data   ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the time series data if the original data set is  #
# changed.                                                                     #
#------------------------------------------------------------------------------#
  
  observeEvent(file(), {
    
    # Resetting the time series data
    timeseriesData$dataList <- NULL
    
  })

#------------------------------------------------------------------------------#
# Rendering the data table for the timeseries ----------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the data table if the 'check-mark' is hit to     #
# show the data frame.                                                         #
#------------------------------------------------------------------------------#
  
  
  # Render statement
  output$timeseries <- renderDataTable({timeseriesData$dataList})
  
#------------------------------------------------------------------------------#
# Downloading the time series data as a '.csv' ---------------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the time series data as a '.csv' file to the        #
# directory of their choosing.                                                 #
#------------------------------------------------------------------------------#
  
  output$download_timeseries <- downloadHandler(
    
    ####################################
    # Function to create the file-name #
    ####################################
    filename = function() {
      
      # File name 
      paste("time-series-data-", input$dataset, sep = "")
      
    },
    
    #############################
    # Function to save the file #
    #############################
    content = function(file) {
      
      # Saving the file
      write.csv(timeseriesData$dataList, file, row.names = FALSE)
      
    }
    
  ) # End of download button  
  
  
#------------------------------------------------------------------------------#
# Button to edit legend labels -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the legend labels in the     #
# 'plotly' figure.                                                             #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleY <- reactiveValues(logScale = "Original") 
  
  #######################################
  # Reactive value for the y-axis label #
  #######################################
  yAxisLab <- reactiveValues(lab = "Count")
  
  ################################################
  # Reactive value for the number of date breaks #
  ################################################
  dateBreaksReactive <- reactiveValues(breaksDate = "1")
  
  #############################################
  # Reactive value for showing forecast lines #
  #############################################
  forecastLinesReactive <- reactiveValues(indicator = F)
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editLegendLabels, {
    
    showModal(modalDialog(
      title = "Figure Options",
      pickerInput("logScale", "Scale for Y-Axis:", c("Original", "Log(Base 10)"), selected = scaleY$logScale), # Scale of y-axis
      textInput("yaxisTimeSeries", "Label for Y-Axis:", value = yAxisLab$lab), # Label for y-axis
      textInput("dateBreaksTS", "Number of Date Breaks (Label):", value = dateBreaksReactive$breaksDate), # Number of date breaks  
      checkboxInput("forecastLines", "Show Forecast Dates", value = forecastLinesReactive$indicator) # Show forecast lines 
    ))
    
  })
  
  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScale, {
    
    # Updating the scale
    scaleY$logScale <- input$logScale
    
  })
  
  ###############################################################
  # Update the reactive value - Y-axis for the timeseries label #
  ###############################################################
  observeEvent(input$yaxisTimeSeries, {
    
    # Updating the y-axis label
    yAxisLab$lab <- input$yaxisTimeSeries
    
  })
  
  #####################################################
  # Update the reactive value - Number of date breaks #
  #####################################################
  observeEvent(input$dateBreaksTS,{
    
    # Updating the number of date breaks
    dateBreaksReactive$breaksDate <- input$dateBreaksTS
    
  })
  
  ##########################################
  # Update the reactive value - Show dates #
  ##########################################
  observeEvent(input$forecastLines, {
    
    # Update the check box
    forecastLinesReactive$indicator <- input$forecastLines
    
  })

#------------------------------------------------------------------------------#
# Creating the time series figure ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the time series figure based on the data above.  #
# The time series figure shows the trajectory of the entire process of         #
# interest, for each of the selected locations/groups. Additionally, it allows #
# users to show the forecast period dates they selected in the side panel.     #
#------------------------------------------------------------------------------#
  
  #####################################
  # Reactive value to save figures in #
  #####################################
  timeseriesFigureList <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    ##############################
    # Running if no errors occur #
    ##############################
    tryCatch({
      
      #############################################
      # Function to produce the timeseries figure #
      #############################################
      timeseriesFigure <- timeseries.figure.function(timeseries.input = timeseriesData$dataList, # Timeseries data
                                                     location.input = c(input$locations), # Locations
                                                     dateType.input = dateValues$dates, # Type of data
                                                     forecastLineShow = input$forecastLines, # Show forecast lines
                                                     forecastDatesStart = input$forecast.period[1], # Start of slider
                                                     forecastDatesEnd = input$forecast.period[2], # End of slider
                                                     scaleYAxis = scaleY$logScale, # Scale for y-axis
                                                     yAxisLabel = input$yaxisTimeSeries, # Y-axis label 
                                                     dateBreaks = input$dateBreaksTS) # Number of date breaks
      
      # Saving figure list to reactive value variable - Plotly figure
      timeseriesFigureList$figureInteractive <- timeseriesFigure[[1]]
      
      # Saving the non-reactive ggplot figure to the second list
      timeseriesFigureList$figureStatic <- timeseriesFigure[[2]]
      
    ##############################
    # Running if an error occurs #
    ##############################
    }, error = function(e){
      
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'observe'
  
#------------------------------------------------------------------------------#
# Clearing the time series image   ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the time series image if the orignal data set is  #
# changed.                                                                     #
#------------------------------------------------------------------------------#
  
  observeEvent(file(), {
    
    # Resetting the interactive form of the image
    timeseriesFigureList$figureInteractive <- NULL
    
    # Resetting the static form of the image
    timeseriesFigureList$figureStatic <- NULL
    
  })
  
  
#------------------------------------------------------------------------------#
# Rendering the time-series figure ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the plotly time-series image to the main         #
# dashboard. Therefore, users can scroll over the figures and see values at    #
# specific points over the course of a process's trajectory.                   #
#------------------------------------------------------------------------------#
  
  # Reactive value to save y-axis title
  yAxisTitleObj <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    ##############################
    # Running if no errors occur #
    ##############################
    tryCatch({
      
      event_trigger <- reactive({
        
        list(timeseriesData$dataList, input$forecastLines, scaleY$logScale, input$yaxisTimeSeries, input$dateBreaksTS, file())
        
      })
      
      #################################
      # Running if "Run" has been hit #
      #################################
      observeEvent(ignoreInit = TRUE, event_trigger(), {
                   
          ################################################
          # Rendering a NULL object if the check is true #
          ################################################
          if(is.null(timeseriesFigureList$figureInteractive)){
            
            # Isolating behaviors until the button is clicked

              # Render statement
              output$timeseriesPlot <- NULL
              
              
          #######################################################
          # Rendering the timeseries figure if check is not hit #
          #######################################################
          }else{
          
            # Render statement
            output$timeseriesPlot <- renderPlotly({
              
              
              # Plot to return
              timeFig <- ggplotly(timeseriesFigureList$figureInteractive, tooltip = "text") 
              
              # Returning the figure
              return(timeFig)
              
            })
            
          } # End of NULL rendering
        
      }) # End of "run" observe event 
              
    ###########################################
    # Returns NULL the above code can not run #
    ###########################################
    }, error = function(e){
      
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'observe'


  
#------------------------------------------------------------------------------#
# Downloading the timeseries figure --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to the 'download' figures button. #
# It takes in inputs from the pop-up menu, and then allows users to save the   #
# time series figure in the location of the users choosing.                    #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Creating the option to download the figure #
  ##############################################
  output$downloadtimeseriesF<- downloadHandler(
    
    ####################################
    # Function to create the file-name #
    ####################################
    filename = function() {
      
      # Closing the figure specification 
      removeModal()
      
      # Removing '.csv' 
      fileName <- gsub('.csv', '', input$dataset)
      
      # File name 
      paste(fileName, "-timeseries.", input$extFig, sep = "")
      
    },
    
    #############################
    # Function to save the file #
    #############################
    content = function(file) {
        
        figure <- timeseriesFigureList$figureStatic 

      # Running with compression if using a '.tiff'
      if(input$extFig == 'tiff'){
        
        # Saving the file
        ggsave(file, plot = figure, 
               dpi = input$dpi,
               width = input$width, 
               height = input$height, 
               units = input$units,
               compression = "lzw")
        
        # Running without compression if not using a '.tiff'
      }else{
        
        # Saving the file
        ggsave(file, plot = figure, 
               dpi = input$dpi,
               width = input$width, 
               height = input$height, 
               units = input$units)
      }
      
    }) # End of saving the figure(s) 
  
  
#------------------------------------------------------------------------------#
# UI Input for Smoothing Data --------------------------------------------------
#------------------------------------------------------------------------------#
# About: If working with daily data, the option to smooth the data will appear #
# in the sidebar menu for users.                                               #
#------------------------------------------------------------------------------#

  ##########################
  # Creating the UI object #
  ##########################
  output$smoothing <- renderUI({

    ######################################################
    # Fixing the error that occurs until data is read in #
    ######################################################
    tryCatch({
      
    #################################
    # Reading in the 'type' of data #
    #################################
    dateType <- dateValues$dates
    
    ####################################################
    # Creating the UI input if working with daily data #
    ####################################################
    if(dateType == "day"){
      
      # Creating the UI object
      return(numericInput("smoothingInput",
                          "Data Smoothing:",
                          value = "1"))
      
      ###############################################
      # Returns NULL if not working with daily data #
      ###############################################
      }else{
        
        # Return NULL 
        return(NULL)
        
        }
    
    ###############################
    # Runs if no data is selected #
    ###############################
    }, error = function(e){
      
      NULL
      
      }) # End of 'error'
    
    }) # End of 'render' statement 
  

#------------------------------------------------------------------------------#
# UI Input for Forecast Period(s) ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines the numerical value that should go with the   #
# the determined date type, and then calculates the earliest possible          #
# calibration period and latest possible calibration period based upon the     #
# user inputted data, desired calibration period, and date type. It takes in   #
# an input of the date type determined above, and outputs the information for  #
# the forecast period picker.                                                  #
#------------------------------------------------------------------------------#
  
  output$forecast.period <- renderUI({
    
    ######################################
    # Runs if there is no missing inputs #
    ######################################
    tryCatch({
      
      ############################################################
      # Determining the number that is the sequence for the date #
      ############################################################
      dateSeq <- switch(as.character(dateValues$dates), # Calling the data type
                        "week" = 7, # Sequence forecast periods by seven if weekly data
                        "day" = 1, # Sequence forecast periods by seven if daily data
                        1) # Sequence forecast periods by one if daily, yearly, or time index
      
      ########################################################
      # Determining the min, max, and creating a date vector #
      ########################################################
      if(dateValues$dates %in% c("year", "index")){
        
        # Earliest possible date
        data_min_date <- as.numeric(min(na.omit(as.numeric(file()[,1]))))
        
        # Latest possible date
        data_max_date <- as.numeric(max(na.omit(as.numeric(file()[,1]))))
        
        ##########################################
        # Create the sliderInput for the UI Side #
        ##########################################
        return(sliderInput("forecast.period",
                           label = tags$span("Forecasting Date(s) ", # Input label
                                             tags$i(class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF;",
                                                    title = "The forecasting date corresponds the last week of data included in the calibration period (i.e., the week the forecast is conducted).")
                           ),
                           min = data_min_date,
                           max = data_max_date,
                           value = c(data_min_date, data_max_date),
                           step =  dateSeq,
                           sep = ""))
        
      }else{
        
        # Earliest possible date
        data_min_date <- min(na.omit(anytime::anydate(file()[, 1])))
        
        # Latest possible date
        data_max_date <- max(na.omit(anytime::anydate(file()[, 1])))
        
        return(sliderInput("forecast.period",
                           label = tags$span("Forecasting Date(s) ", # Input label
                                             tags$i(class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF;",
                                                    title = "The forecasting date corresponds the last week of data included in the calibration period (i.e., the week the forecast is conducted).")
                           ),
                           min = data_min_date,
                           max = data_max_date,
                           value = c(data_min_date, data_max_date),
                           step =  dateSeq))
        
      }
      
    #################################
    # Outputs if there is no inputs #
    #################################
    }, error = function(e){
      
      # Outputted slider input
      return(sliderInput("forecast.period",
                         label = tags$span("Forecasting Date(s) ", # Input label
                                           tags$i(class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF;",
                                                  title = "The forecasting date corresponds the last week of data included in the calibration period (i.e., the week the forecast is conducted).")
                         ),
                         min = 0,
                         max = 1,
                         value = c(0, 1),
                         step =  NULL))
      
    }) # End of 'tryCatch' statement 
    
  }) # End of 'renderUI' statement 
 

#------------------------------------------------------------------------------#
# UI Input for the Calibration Period ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines the possible lengths of the calibration       #
# period lengths for a user to use. It takes in an input of the user selected  #
# data fame, and outputs the information for the calibration period picker.    #
#------------------------------------------------------------------------------#
  
  output$calibration.period <- renderUI({
    
    ###################################
    # Runs if there is data available #
    ###################################
    tryCatch({
      
      #######################
      # Reading in the data #
      #######################
      data <- file()
      
      # Sub-setting the dates from the crude data 
      if(dateValues$dates %in% c("day", "week")){
        
        crude.dates <- anytime::anydate(data[, 1])
        
      }else{
        
        crude.dates <- as.numeric(data[, 1])
        
      }
      
      ##############################################################################
      # Determining what the length of the last possible calibration period can be #
      ##############################################################################
      last.calibration.period <- length(crude.dates[crude.dates <= input$forecast.period[1]])
      
      ###############################
      # Rendering the 'pickerInput' #
      ###############################
      pickerInput("calibrationPeriod", # ID
                  label = tags$span("Calibration period length:", # Input label
                                    tags$i(
                                      class = "glyphicon glyphicon-info-sign",
                                      style = "color:#FFFFFF;",
                                      title = "Indicate the length of data you are feeding into the model.")),
                  choices = c(seq(1,last.calibration.period, 1))) # Initial starting period 
      
    ########################
    # Runs if missing data #
    ########################
    }, error = function(e){
      
      ###############################
      # Rendering the 'pickerInput' #
      ###############################
      pickerInput(
        "calibrationPeriod", # Choosing the calibration period length
        label = tags$span("Calibration period length:", # Input label
                          tags$i(
                            class = "glyphicon glyphicon-info-sign",
                            style = "color:#FFFFFF;",
                            title = "Indicate the length of data you are feeding into the model.")),
        choices = NULL) # Initial starting period 
      
    }) # End of 'tryCatch' statement
    
  }) # End of 'renderUI' statement 
  

#------------------------------------------------------------------------------#
# Preparing the calibration periods --------------------------------------------
#------------------------------------------------------------------------------#
# About: Below creates the forecasting periods used throughout the remainder   #
# of the dashboard program. The forecasting, or calibration periods is a list  #
# of the data used to calibrated the different models at each forecasting      #
# period. Therefore, this section returns a list of data frames.               #
#------------------------------------------------------------------------------#

  ################################################
  # Initialize reactiveValues to store the dates #
  ################################################
  calibration.period.list <- reactiveValues(calibrations = NULL)
  
  ###################################
  # Observing differences in inputs #
  ###################################
  observe({
    
    ###############################
    # Runs if all data is entered #
    ###############################
    tryCatch({
      
      #######################################
      # Runs if the 'run' button is clicked #
      #######################################
      observeEvent(input$run, {
        
        ##################################
        # Loading the user selected data #
        ##################################
        data <- file()
        
        if(nrow(data) > 0 & !is.null(data)){
          
        # Extracting the date column
        dateHeader <- names(data)[1]
        
        # Determining the locations to keep
        locations <- input$locations
        
        # Sub-setting data to keep date column and locations of interest
        data <- data %>%
          dplyr::select(dateHeader, all_of(locations))

        #############################################
        # Calibration period input selected by user #
        #############################################
        caliPeriod <- input$calibrationPeriod
        
        ############################################################
        # Determining the number that is the sequence for the date #
        ############################################################
        dateSeq <- switch(as.character(dateValues$dates), # Calling the data type
                          "week" = 7, # Sequence forecast periods by seven if weekly data
                          "day" = 7,
                          1) # Sequence forecast periods by one if daily, yearly, or time index
        
        
        #########################
        # Forecast period range #
        #########################
        if(dateSeq == 1){
          
          # Working with yearly or time index data 
          forecastPeriodRange <- c(seq(input$forecast.period[1],  input$forecast.period[2], by = 1))
          
        #####################################
        # Working with daily or weekly data #
        #####################################
        }else{
          
          # List of forecast period dates
          forecastPeriodRange <- c(seq.Date(anytime::anydate(input$forecast.period[1]),  anytime::anydate(input$forecast.period[2]), by = 7)) # If working with daily or weekly data
          
        }
        
        #######################################################
        # Function that returns a list of calibration periods #
        #######################################################
        isolate({
          
          # Function of calibration/forecasting periods 
          calibrationPeriod.return <- calibration.period.function(crude.data.input = data, 
                                                                  calibration.period.input = as.numeric(caliPeriod),
                                                                  forecast.period.input = c(forecastPeriodRange), 
                                                                  date.input = dateValues$dates)
          
        })
        
        ###############################
        # Updating the reactive value #
        ###############################
        calibration.period.list$calibrations <- calibrationPeriod.return
        
        #########################################
        # Returning the calibration period list #
        #########################################
        return(calibration.period.list$calibrations)
        
        }else{
          
          return(NULL)
          
        }
        
      }) # End of 'observeEvent' statement 
      
    ##############################
    # Runs if no data is entered #
    ##############################
    }, error = function(e){
      
      # Returns NULL
      NULL
      
    }) # End of 'tryCatch' statement 
    
  }) # End of 'reactive' statement 
 
  
#------------------------------------------------------------------------------#
# Working with ARIMA Models ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section is centered on the available features with the  #
# included auto-regressive integrated moving average models. First, it creates #
# the ARIMA specific UI outputs, including, parameter specification,           #
# indication of seasonality, and other available features. It then runs the    #
# model and outputs quantile forecasts which are used throughout the rest of   #
# the shiny app, and also outputted to the main screen.                        #
#------------------------------------------------------------------------------#
  
  ##########################################
  # Creating the UI output for seasonality #
  ##########################################
  output$ARIMA.seasonality <- renderUI({
    
    # Creating the text input 
    textInput("seasonality", "Seasonal Pattern", value = 1)
    
    }) # End of 'renderUI' statement
  
  #############################################
  # Creating the UI parameter outputs - p Min #
  #############################################
  output$pMin <- renderUI({
    
    #####################################
    # Creating the numeric input - pMin #
    #####################################
    numericInput("pMin", label = "p min", value = 0)
    
    }) # End of 'renderUI' statement 
  
  #############################################
  # Creating the UI parameter outputs - P Min #
  #############################################
  output$PMin <- renderUI({
    
    #####################################
    # Creating the numeric input - pMin #
    #####################################
    numericInput("PMin", label = "P Min", value = 0)
    
  }) # End of 'renderUI' statment 
  
  #############################################
  # Creating the UI parameter outputs - p Max #
  #############################################
  output$pMax <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("pMax", label = "p Max", value = 10)
    
    }) # End of 'renderUI' statement
  
  #############################################
  # Creating the UI parameter outputs - P Max #
  #############################################
  output$PMax <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("PMax", label = "P Max", value = 3)
    
  }) # End of 'renderUI' statement
  
  #############################################
  # Creating the UI parameter outputs - q Min #
  #############################################
  output$qMin <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("qMin", label = "q Min", value = 0)
    
    }) # End of 'renderUI' statement
  
  #############################################
  # Creating the UI parameter outputs - Q Min #
  #############################################
  output$QMin <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("QMin", label = "Q Min", value = 1)
    
  }) # End of 'renderUI' statement
  
  
  #############################################
  # Creating the UI parameter outputs - q Max #
  #############################################
  output$qMax <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("qMax", label = "q Max", value = 5)
    
    }) # End of 'renderUI' statement
  
  #############################################
  # Creating the UI parameter outputs - Q Max #
  #############################################
  output$QMax <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("QMax", label = "Q Max", value = 3)
    
  }) # End of 'renderUI' statement
  
  ###################################################
  # Creating the UI parameter outputs - Differences #
  ###################################################
  output$differences <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("differences", label = "Non-seasonal Differences", value = 2)
    
  }) # End of 'renderUI' statement
  
  ############################################################
  # Creating the UI parameter outputs - Seasonal Differences #
  ############################################################
  output$Seasonaldifferences <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("Seasonaldifferences", label = "Seasonal Differences", value = 2)
    
  }) # End of 'renderUI' statement
 
  
#------------------------------------------------------------------------------#
# Producing the ARIMA Forecasts ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes in ARIMA inputs and runs the ARIMA model based on  #
# the user selected inputs. It then outputs a list with both the quantile      #
# based forecasts and the best-fit model information. The forecasts are saved  #
# in a reactive values and then shown on the main page.                        #
#------------------------------------------------------------------------------#
   
   ################################################
   # Initialize reactiveValues to store the dates #
   ################################################
   ARIMAInfo <- reactiveValues()
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     ########################
     # Runs if 'run' is hit #
     ########################
     observeEvent(input$run, {
       
       ###################################
       # Runs if not missing information #
       ###################################
       tryCatch({
         
         #########################################################
         # Isolating the behavior to only when the button is hit #
         #########################################################
         isolate({
           
           # List of selected models 
           model <- c(input$modelType)
           
           ######################################
           # Runs if an ARIMA model is selected #
           ######################################
           if(any(model %in% c("ARIMA"))){
             
             
             ##################################################################
             # Isolating the behavior of the function until run button is hit #
             ##################################################################
             isolate({
               
               ###############################################
               # ARIMA Function - Quantiles, Best-fit models #
               ###############################################
               ARIMAList <- ARIMA(calibration.input = calibration.period.list$calibrations, # List of forecast periods 
                                  horizon.input = input$forecastHorizon, # Forecasting horizon 
                                  smoother.input = input$smoothingInput, # Smoothing for data 
                                  parameter.input = c(input$pMin, input$pMax, input$qMin, input$qMax, 
                                                      input$differences, input$PMin, input$PMax, input$QMin, input$QMax, 
                                                      input$Seasonaldifferences), # ARIMA parameters 
                                  seasonality.input = input$seasonality) # ARIMA seasonality 
               
               #########################################################
               # Returning an error if calibration period is too small #
               #########################################################
    
               # Checking for NAs 
               isNAARIMA <- ARIMAList$Forecasts[is.na(ARIMAList$Forecasts)]
               
               # Pulling the names with issues
               namesErrorARIMA <- c(names(isNAARIMA))
                 
               ###################################################
               # Returning the list of lists if ARIMA model runs #
               ###################################################
               
               # Determining which ARIMA are not NA
               notNAARIMA <- ARIMAList$Forecasts[!is.na(ARIMAList$Forecasts)]
               
               # Saving list of lists to reactive value 
               ARIMAInfo$arima <- notNAARIMA
               
               #####################################################
               # Error if there are any forecasts that did not run #
               #####################################################
               if(length(isNAARIMA) > 0){
                 
                 # Error 
                 shinyalert("Unable to run some ARIMA forecasts (i.e., small case counts).", 
                            type = "error")
                 
               }
               
               }) # End of inner 'isolate' statement 
             
           ##############################################
           # End of code that runs for an 'ARIMA' model # 
           ##############################################
           }else{ 
             
             # Null
             NULL
             
           } # End of 'else' if ARIMA is not selected 
           
         }) # Outer 'isolate' statement 
       
       #####################################
       # Runs if no information is entered #
       #####################################
       }, error = function(e){
         
         # Returns a NULL
         NULL
         
         }) # End of 'tryCatch' statement 
       
     }) # End of 'observeEvent'
     
   }) # End of 'observe'
       
#------------------------------------------------------------------------------#
# Setting up the GLM forecasts -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the model specification options for the GLM      #
# models. The only user option is specifying the underlying error distribution.#
#------------------------------------------------------------------------------#
   
   ####################################
   # Selecting the error distribution #
   ####################################
   output$errorTermGLM <- renderUI({
     
     pickerInput("errorTermGLM",
                 "Error Distribution:",
                 choices = c("Normal", "Negative Binomial (NB)", "Poisson"))
     
   }) # End of 'textInput'
   
#------------------------------------------------------------------------------#
# Running the GLM forecasts ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the user inputs from above to produce a list of    #
# GLM forecasts which can be used throughout the remainder of the code.        #
#------------------------------------------------------------------------------#
   
   ####################################################################
   # Initialize 'reactiveValues' to store the a list of GLM forecasts #
   ####################################################################
   GLMList <- reactiveValues()
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     ##########################################
     # Runs only if the GLM model is selected #
     ##########################################
     observeEvent(input$run,{
       
       ########################################################################
       # 'tryCatch' to run the remainder of the code if no inputs are missing #
       ########################################################################
       tryCatch({
         
         #######################
         # Isolating behaviors #
         #######################
         isolate({
           
           # Model list 
           model <- c(input$modelType)
           
           ###################################
           # Running only if GLM is selected #
           ###################################
           if(any(model %in% c("GLM"))){
             
             #########################
             # Date Type of Interest #
             #########################
             dateSeq <- switch(as.character(dateValues$dates), # Calling the data type
                               "week" = 7, # Sequence forecast periods by seven if weekly data
                               "day" = 7, # Sequence forecast periods by seven if daily data
                               1) # Sequence forecast periods by one if daily, yearly, or time index
             
             ####################################
             # Inner 'isolate' for GLM function #
             ####################################
             isolate({
               
               ########################
               # Calling the function #
               ########################
               GLMListQuantile <- GLM(calibration.input = calibration.period.list$calibrations, # List of calibration periods 
                                      horizon.input = input$forecastHorizon, # Forecasting horizon 
                                      date.Type.input = dateSeq, # Date type 
                                      smoothing.input = input$smoothingInput, # Data smoothing 
                                      error.input = input$errorTermGLM) # Error distribution 
               
               #########################################################
               # Returning an error if calibration period is too small #
               #########################################################
               
               # Checking for NAs 
               isNAGLM <- GLMListQuantile$Forecasts[is.na(GLMListQuantile$Forecasts)]
               
               # Pulling the names with issues
               namesErrorGLM <- c(names(isNAGLM))
               
               #####################################################
               # Error if there are any forecasts that did not run #
               #####################################################
               if(length(namesErrorGLM) > 0){
                 
                 # Error 
                 shinyalert("Unable to run the following GLM forecasts (i.e., not enough data): ", 
                            paste(namesErrorGLM, collapse = "\n"), type = "error")
                 
               }
               

               #################################################
               # Returning the list of lists if GLM model runs #
               #################################################
               
               # Determining which GLM are not NA
               notNAGLM <- GLMListQuantile$Forecasts[!is.na(GLMListQuantile$Forecasts)]
               
               # Saving list of lists to reactive value 
               GLMList$GLM <- notNAGLM
               
             }) # End of inner 'isolate' function
             
           ###########################################
           # End of 'if' statement to run 'GLM' code #
           ###########################################
           }else{
             
             # Null
             NULL
             
           } # End of 'if-else' for GLM
           
         }) # End of outer 'isolate' statement 
         
       #############################################
       # Runs if no information is entered by user #
       #############################################
       }, error = function(e){
         
         NULL
         
       }) # End of 'tryCatch' statement
       
     }) # End of 'observeEvent'
     
   }) # End of 'observe'
           

#------------------------------------------------------------------------------#
# Setting up the GAM forecasts -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the model specification options for the GAM      #
# models. User options include specifying the number of basis functions (k),   #
# the underlying error distribution (error), and the smoothing term.           #
#------------------------------------------------------------------------------#
   
   ##########################################
   # Creating the number of basis functions #
   ##########################################
   output$numBasis <- renderUI({
     
     textInput("numberBasis", 
               "Number of Basis Functions:",
               value = floor(as.numeric(as.numeric(input$calibrationPeriod)/2) + 5)
               
     ) # End of 'textInput'
     
   }) # End of 'renderUI'
   
   ##################
   # Smoothing term #
   ##################
   output$smoothingTerm <- renderUI({
     
     textInput("smoothingTerm",
               "Smoothing Term:",
               value = "ps")
     
   }) # End of 'textInput'
   
   ####################################
   # Selecting the error distribution #
   ####################################
   output$errorTerm <- renderUI({
     
     pickerInput("errorTerm",
                 "Error Distribution:",
                 choices = c("Normal", "Negative Binomial (NB)", "Poisson"))
     
   }) # End of 'textInput'
   
  
   
#------------------------------------------------------------------------------#
# Running the GAM forecasts ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the user inputs from above to produce a list of    #
# GAM forecasts which can be used throughout the remainder of the code.        #
#------------------------------------------------------------------------------#
   
   ####################################################################
   # Initialize 'reactiveValues' to store the a list of GAM forecasts #
   ####################################################################
   GAMList <- reactiveValues()

   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({

     ##########################################
     # Runs only if the GAM model is selected #
     ##########################################
     observeEvent(input$run,{
       
       ##################################################
       # 'tryCatch' to run if no information is missing #
       ##################################################
       tryCatch({
         
         #######################
         # Isolating behaviors #
         #######################
         isolate({
           
           model <- c(input$modelType)
           
           ###################################
           # Running only if GAM is selected #
           ###################################
           if(any(model %in% c("GAM"))){
             
             #########################
             # Date Type of Interest #
             #########################
             dateSeq <- switch(as.character(dateValues$dates), # Calling the data type
                               "week" = 7, # Sequence forecast periods by seven if weekly data
                               "day" = 7, # Sequence forecast periods by seven if daily data
                               1) # Sequence forecast periods by one if daily, yearly, or time index
             
             ####################################
             # Inner 'isolate' for GAM function #
             ####################################
             isolate({
               
               ########################
               # Calling the function #
               ########################
               GAMListQuantile <- GAM(calibration.input = calibration.period.list$calibrations, # List of calibration periods 
                                      horizon.input = input$forecastHorizon, # Forecasting horizon 
                                      date.Type.input = dateSeq, # Date type 
                                      smoothing.input = input$smoothingInput, # Data smoothing 
                                      error.input = input$errorTerm, # Error distribution 
                                      k.input = as.numeric(input$numberBasis), # Number of basis functions 
                                      smoothingTerm.input = input$smoothingTerm) # Smoothing term for GAM 
               
               
               #########################################################
               # Returning an error if calibration period is too small #
               #########################################################
               
               # Checking for NAs 
               isNAGAM <- GAMListQuantile$Forecasts[is.na(GAMListQuantile$Forecasts)]
               
               # Pulling the names with issues
               namesErrorGAM <- c(names(isNAGAM))
               
               #####################################################
               # Error if there are any forecasts that did not run #
               #####################################################
               if(length(namesErrorGAM) > 0){
                 
                 # Error 
                 shinyalert("Unable to run the following GAM forecasts (i.e., not enough data): ", 
                            paste(namesErrorGAM, collapse = "\n"), type = "error")
                 
               }
               
               
               #################################################
               # Returning the list of lists if GAM model runs #
               #################################################
               
               # Determining which GAM are not NA
               notNAGAM <- GAMListQuantile$Forecasts[!is.na(GAMListQuantile$Forecasts)]
               
               # Saving list of lists to reactive value 
               GAMList$GAM <- notNAGAM
               
             }) # End of inner 'isolate' function
             
           ###################################
           # End of code running 'GAM' model #
           ###################################
           }else{
             
             # NULL if model is not selected 
             NULL
             
           } # End of 'if-else' for GAM code 
           
         }) # End of outer 'isolate'
         
       #############################################
       # Runs if no information is entered by user #
       #############################################
       }, error = function(e){
         
         NULL
         
       }) # End of 'tryCatch' statement 
       
     }) # End of 'observeEvent'
     
   }) # End of 'observe'
           
   
#------------------------------------------------------------------------------#
# Setting up for Prophet forecasting -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets up the 'Prophet-specific' functions that are        #
# available as part of the toolbox. These include, the type of growth trend,   #
# seasonal trends, and holidays.                                               #
#------------------------------------------------------------------------------#
   
   ################
   # Growth trend #
   ################
   output$growthTrend <- renderUI({
     
     # Creating the picker input for growth trends
     pickerInput("growthTrends",
                 "Growth Trend:",
                 choices = c("linear", "flat"),
                 selected = "linear")
     
   }) # End of render
   
   ###############
   # Seasonality #
   ###############
   output$prophetSeasonality <- renderUI({
     
     # Creating the picker input for seasonality 
     pickerInput("prophetSeasonality",
                 "Seasonality:",
                 choices = c("Auto", "Yearly", "Weekly", "Daily", "None"),
                 selected = "Auto")
     
   }) # End of render
   

#------------------------------------------------------------------------------#
# Running the Prophet model ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes in the inputs from above regarding the growth      #
# trends, seasonality, and potential holidays along with the standard inputs:  #
# (1) Calibration period, (2) Horizon, and (3) Calibration period size to run  #
# the Prophet model. The produced quantiles are then saved in a list to be     #
# used throughout the rest of the code.                                        #
#------------------------------------------------------------------------------#
   
   ####################################################################
   # Initialize 'reactiveValues' to store the a list of GAM forecasts #
   ####################################################################
   ProphetList <- reactiveValues()
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     ##########################################
     # Runs only if the GAM model is selected #
     ##########################################
     observeEvent(input$run,{
       
       ##################################################
       # 'tryCatch' to run if no information is missing #
       ##################################################
       tryCatch({
         
         #######################
         # Isolating behaviors #
         #######################
         isolate({
           
           # Model 
           model <- c(input$modelType)
          
           ###################################
           # Running only if GAM is selected #
           ###################################
           if(any(model %in% c("Prophet"))){
             
             #################
             # Inner Isolate #
             #################
             isolate({
               
               ########################
               # Running the function #
               ########################
               prophetList <- Prophet(calibration.input = calibration.period.list$calibrations, # Calibration period 
                                      horizon.input = input$forecastHorizon, # Horizon 
                                      date.type.input = dateValues$dates, # Date type 
                                      smoother.input = input$smoothingInput, # Data smoothing 
                                      seasonalityProphet.input = input$prophetSeasonality, # Seasonality for Prophet
                                      holidayDates.input = input$holidays, # Holidays 
                                      growthTrend.input = input$growthTrends) # Type of growth pattern 
               
               #########################################################
               # Returning an error if calibration period is too small #
               #########################################################
               
               # Checking for NAs 
               isNAProphet <- prophetList$Forecasts[is.na(prophetList$Forecasts)]
               
               # Pulling the names with issues
               namesErrorProphet <- c(names(isNAProphet))
               
               #####################################################
               # Error if there are any forecasts that did not run #
               #####################################################
               if(length(namesErrorProphet) > 0){
                 
                 # Error 
                 shinyalert("Unable to run the following Prophet forecasts (i.e., not enough data): ", 
                            paste(namesErrorProphet, collapse = "\n"), type = "error")
                 
               }
               
               
               #####################################################
               # Returning the list of lists if Prophet model runs #
               #####################################################
               
               # Determining which Prophet are not NA
               notNAProphet <- prophetList$Forecasts[!is.na(prophetList$Forecasts)]
               
               # Saving list of lists to reactive value 
               ProphetList$prophet <- notNAProphet
               
             }) # End of inner 'isolate'
             
           #################################
           # End of code running 'Prophet' #
           #################################
           }else{
             
             # Runs NULL if not model
             NULL
             
           } # End of Prophet 'if-else'
           
         }) # End of outer 'isolate'
         
         ##############################
         # Runs if missing user input #
         ##############################
       }, error = function(e){
         
         # Null
         NULL
         
       }) # End of 'tryCatch'
       
     }) # End of 'observeEvent'
       
   }) # End of 'observe'
   
#------------------------------------------------------------------------------#
# Creating the filtering options for the quantile forecast ---------------------
#------------------------------------------------------------------------------#
# About: This section creates the quantile forecast data filter for the main   #
# page of the dashboard.                                                       #
#------------------------------------------------------------------------------#
   
   ######################################
   # Creating the needed reactive value #
   ######################################
   
   # To store the filtered quantile forecasts 
   finalQuantileCombined <- reactiveValues()
   
   # Model choices 
   QuantileForecastModelChoice <- reactiveValues()
   
   # Selected models 
   QuantileForecastModelSelect <- reactiveValues()
   
   # Location choices 
   QuantileForecastLocationChoice <- reactiveValues()
   
   # Selected locations 
   QuantileForecastLocationSelect <- reactiveValues()
   
   # Indicator 
   QuantileForecastMAINIndicator <- reactiveVal(0)
   
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     #####################################
     # Running if information is entered #
     #####################################
     tryCatch({
       
       # Creating the combination of quantile forecasts 
       quantile.forecast <- c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet)
       
       # Running only if an error is not returned above
       if(all(!is.null(quantile.forecast))){
         
         #################
         # Model Options #
         #################
         
         # Choices
         QuantileForecastModelChoice$data <- unique(input$modelType)
         
         # Selected 
         QuantileForecastModelSelect$data <- QuantileForecastModelChoice$data
         
         ####################
         # Location Options #
         ####################
         
         # Choices
         QuantileForecastLocationChoice$data <- unique(input$locations)
         
         # Selected 
         QuantileForecastLocationSelect$data <- QuantileForecastLocationChoice$data
         
         #######################
         # Creating the pop-up #
         #######################
         observeEvent(input$filterQuantileForecasts, ignoreInit = T,{
           
           # Changing the indicator to one (i.e., button has been clicked)
           isolate({QuantileForecastMAINIndicator(1)})
           
           # Isolating button click behavior 
           isolate({
             
             # Button 
             showModal(modalDialog(
               
               title = "Filtering Options",
               pickerInput("modelQuantile", "Model:", c(QuantileForecastModelChoice$data), selected = c(QuantileForecastModelSelect$data), multiple = T), # Model filtering
               pickerInput("locationsQuantile", "Location:", c(QuantileForecastLocationChoice$data), selected = c(QuantileForecastLocationSelect$data), multiple = T), # Metric Type
               
             ))
             
           })
           
         })
         
         ##################################################
         # Function to filter the formatted forecast data #
         ##################################################
         
         filteredForecasts <- filteringQuantileForecasts(QuantileForecast.input = quantile.forecast, # Quantile forecast
                                                         modelFilterQ.input = input$modelQuantile, # Model filter
                                                         locationFilterQ.input = input$locationsQuantile, # Location filter
                                                         indicator.input = QuantileForecastMAINIndicator()) # Indicator

         # Saving the results to the reactive value
         finalQuantileCombined$data <- filteredForecasts
         
       } # End of 'else'
       
     ####################################
     # Running if data is NOT available #
     ####################################
     }, error = function(e){
       
       NULL
       
     })
     
   })
   
   
#------------------------------------------------------------------------------#
# Resetting the quantile forecast data -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the quantile forecast data when the data is       #
# changed.                                                                     #
#------------------------------------------------------------------------------#
   
   ###############################
   # Looking for the data change #
   ###############################
   observeEvent(file(), {
     
     # Clearing the output
     finalQuantileCombined$data <- NULL
     
     # Resetting the indicator
     QuantileForecastMAINIndicator(0)
     
     # Resetting the ARIMA
     ARIMAInfo$arima <- NULL
     
     # Resetting the GAM
     GAMList$GAM <- NULL 
     
     # Resetting the GLM
     GLMList$GLM <- NULL
     
     # Resetting the Prophet 
     ProphetList$prophet <- NULL
     
   })
   
   
#------------------------------------------------------------------------------#
# Arrows buttons for rendering quantile forecasts ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the forward and backward arrows for the quantile #
# box. Additionally, it renders the quantile data frame based on user selected #
# options.                                                                     #
#------------------------------------------------------------------------------#
   
   ####################################################################
   # Creating the reactive value to be used with the quantile buttons #
   ####################################################################
   current_index <- reactiveVal(1)
   
   #################################################
   # Going backwards if the previous button is hit #
   #################################################
   observeEvent(input$PreviousQuan, {
     
     # Isolating the action to only when the button is clicked 
     isolate({
       
       # Running if the current index is greater than one 
       if(current_index() > 1){
         
         # Changing the index of the reactive value 
         current_index(max(current_index() - 1))
         
       }
       
     }) # End of 'isolate' statement 
     
   }) # End of 'observeEvent' statement 
   

   ############################################
   # Going forwards if the next button is hit #
   ############################################
   observeEvent(input$NextQuan, {
     
     # Isolating the action to only when the button is clicked 
     isolate({
       
       # Run if the current index is less than the length of the list 
       if (current_index() < length(finalQuantileCombined$data)) {
         
         # Changing the index of the reactive value 
         current_index(min(current_index() + 1))
         
       }
       
     }) # End of 'isolate' statement 
     
   }) # End of 'observeEvent' statement 
   
   ######################################################
   # Fixes the index when the data is filtered - Models #
   ######################################################
   observeEvent(input$modelQuantiles, {
     
     # Isolate the behavior to when the button is clicked
     isolate({
       
       current_index(1)
       
     }) # End of isolate
     
   }) # End of 'observeEvent'
   
   #########################################################
   # Fixes the index when the data is filtered - Locations #
   #########################################################
   observeEvent(input$locationQuantiles, {
     
     # Isolate the behavior to when the button is clicked
     isolate({
       
       current_index(1)
       
     }) # End of isolate
     
   }) # End of 'observeEvent'
   

#------------------------------------------------------------------------------#
# Rending the current quantile data frame box title ----------------------------
#------------------------------------------------------------------------------#
# About: This section renders the current title for the shown quantile         #
# forecasts in the main body of the dashboard.                                 #
#------------------------------------------------------------------------------#
   
   #######################################################
   # Rendering the current quantile data frame box title #
   #######################################################
   output$quantileTitle <- renderText({
     
       
       # Rendering the data table box title 
       return(paste0(names(finalQuantileCombined$data[current_index()])))
       
     
     
   }) # End of render statement for quantile forecasts
   
   
#------------------------------------------------------------------------------#
# Rendering the current quantile data frame ------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the quantile data frame table to the main page   #
# of the dashboard.                                                            #
#------------------------------------------------------------------------------#
   
   #############################################
   # Rendering the current quantile data frame #
   #############################################
   output$quantileForecasts <- renderDataTable({
     

       # Rendering the data table 
       return(datatable(finalQuantileCombined$data[[current_index()]],
                        options = list(scrollX = T))) # Restricts the size of the box 

     
   }) # End of render statement for quantile forecasts
   
   
#------------------------------------------------------------------------------#
# Downloading the quantile forecasts as a zip ----------------------------------
#------------------------------------------------------------------------------#
# About: This section allows users to 'zip' the files that are loaded for      #
# quantile forecasts.                                                          #
#------------------------------------------------------------------------------#
   
   output$download_quantile_forecasts <- downloadHandler(
     
     ####################
     # Filename for ZIP #
     ####################
     filename = function(){
       
       paste("Quantile-Forecasts.zip", sep = "")
       
     },
     
     ############################################
     # Determining what should be in the folder #
     ############################################
     content = function(file){
       
       # Removing the message
       removeModal()
       
       # Creating a temp directory for files 
       temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
       
       # Physically creating the directory 
       dir.create(temp_directory)
       
       # Saving the ggplots 
       for (plot_name in names(finalQuantileCombined$data)) {
         
         # Plot 
         plot_obj <- data.frame(finalQuantileCombined$data[[plot_name]])
         
         # If plot is found 
         if (!is.null(plot_obj)) {
           
           # File name 
           file_name <- glue("{plot_name}.csv")
           
           # Saving the csv
           write_csv(plot_obj, file.path(temp_directory, file_name))
           
         }
         
       }
       
       #####################
       # Create a zip file #
       #####################
       zip::zip(
         zipfile = file,
         files = dir(temp_directory),
         root = temp_directory
       )
       
     },
     
     contentType = "application/zip"
     
   ) # End of download handler 
   
   
#------------------------------------------------------------------------------#
# Producing list of formatted forecasts ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section is centers around the function that re-formats the       #
# quantile forecasts based on the users choice of quantile. The reformatted    #
# forecasts include all available observed data, the predicted median, the     #
# dates, and the upper and lower bounds for the predictions.                   #
#------------------------------------------------------------------------------#
   
   ################################################################
   # Initialize `reactiveValues` to store the formatted forecasts #
   ################################################################
   foremattedForecasts <- reactiveValues()
   
   #####################################################
   # Observing changes in inputs/other reactive values #
   #####################################################
   observe({
     
     ###############################################
     # Observe event for hitting the action button #
     ###############################################
     observeEvent(input$run,{
       
       # Isolate statement 
       isolate({
         
         ####################################################
         # 'TryCatch' statement to run when info is entered #
         ####################################################
         tryCatch({
           
           ##################################################
           # Function to create list of formatted forecasts #
           ##################################################

             # 'Isolate' statement 
             isolate({
                 
               # Running the forecasts
               formattedForecastList <- formatted.forecast.function(quantile.input = c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet), # List of quantiles 
                                                                    data.input = file(), # Original data 
                                                                    calibration.input = as.numeric(input$calibrationPeriod), # Selected calibration period
                                                                    dateType.input = dateValues$dates, # Type of date data
                                                                    model.input = input$modelType, # Selected model 
                                                                    quantile.selected.input = input$quantileSelection, # Selected quantile
                                                                    horizon.input = input$forecastHorizon,
                                                                    smoothing.input = input$smoothingInput) # Data smoothing

               # Saving the exported list to a reactive value
               foremattedForecasts$forecasts <- formattedForecastList
               
             }) # End of inner 'isolate' statement 
           
         #######################################
         # Runs when no information is entered #
         #######################################
         }, error = function(e){
           
           # Returns a NULL
           NULL
           
         }) # End of 'tryCatch' statement 
         
       }) # End of outer 'isolate' statement 
       
     }) # End of 'observeEvent' statement 
     
   }) # End of 'observe' statement 
  

#------------------------------------------------------------------------------#
# Clearing the original data ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the above created formatted forecasts if the file #
# containing the original data is changed.                                     #
#------------------------------------------------------------------------------#

  observeEvent(file(), {
    
    foremattedForecasts$forecasts <- NULL
    
  })
   
#------------------------------------------------------------------------------#
# Creating the filtering options for the formatted forecast --------------------
#------------------------------------------------------------------------------#
# About: This section creates the formatted forecast data filter for the main  #
# page of the dashboard.                                                       #
#------------------------------------------------------------------------------#

  ######################################
  # Creating the needed reactive value #
  ######################################
  
  # To store the filtered formatted forecast
  finalForecastCombined <- reactiveValues()
  
  # Model choices 
  formattedForecastModelChoice <- reactiveValues()
  
  # Selected models 
  formattedForecastModelSelect <- reactiveValues()
  
  # Location choices 
  formattedForecastLocationChoice <- reactiveValues()
  
  # Selected locations 
  formattedForecastLocationSelect <- reactiveValues()
  
  # Indicator 
  formattedForecastMAINIndicator <- reactiveVal(0)


  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    #####################################
    # Running if information is entered #
    #####################################
    tryCatch({
    
    # Running only if an error is not returned above
    if(all(!is.null(foremattedForecasts$forecasts))){
      
      #################
      # Model Options #
      #################
      
      # Choices
      formattedForecastModelChoice$data <- unique(input$modelType)
      
      # Selected 
      formattedForecastModelSelect$data <- formattedForecastModelChoice$data
      
      ####################
      # Location Options #
      ####################
      
      # Choices
      formattedForecastLocationChoice$data <- unique(input$locations)
      
      # Selected 
      formattedForecastLocationSelect$data <- formattedForecastLocationChoice$data
      
      #######################
      # Creating the pop-up #
      #######################
      observeEvent(input$filterFormattedForecasts, ignoreInit = T,{
        
        # Changing the indicator to one (i.e., button has been clicked)
        isolate({formattedForecastMAINIndicator(1)})
        
        # Isolating button click behavior 
        isolate({
          
          # Button 
          showModal(modalDialog(
            
            title = "Filtering Options",
            pickerInput("modelsForecastFigs", "Model:", c(formattedForecastModelChoice$data), selected = c(formattedForecastModelSelect$data), multiple = T), # Model filtering
            pickerInput("locationsForecastFigs", "Location:", c(formattedForecastLocationChoice$data), selected = c(formattedForecastLocationSelect$data), multiple = T), # Metric Type
            
          ))
          
        })
        
      })
      
      ##################################################
      # Function to filter the formatted forecast data #
      ##################################################
    
      filteredForecasts  <- filteringFormattedForecasts(formattedForecast.input = foremattedForecasts$forecasts, # Formatted forecast
                                                        modelFilterFF.input = input$modelsForecastFigs, # Model filter
                                                        locationFilterFF.input = input$locationsForecastFigs, # Location filter
                                                        indicator.input = formattedForecastMAINIndicator()) # Indicator 

      # Saving the results to the reactive value
      finalForecastCombined$data <- filteredForecasts
      
    } # End of 'else'
    
  ####################################
  # Running if data is NOT available #
  ####################################
  }, error = function(e){
    
    NULL
    
  })
  
})

#------------------------------------------------------------------------------#
# Resetting the formatted forecast data ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the formatted forecast data when the data is      #
# changed.                                                                     #
#------------------------------------------------------------------------------#
  
  ###############################
  # Looking for the data change #
  ###############################
  observeEvent(file(), {
    
    # Clearing the output
    finalForecastCombined$data <- NULL
    
    # Resetting the indicator
    formattedForecastMAINIndicator(0)
    
  })

  
#------------------------------------------------------------------------------#
# Arrows and rendering formatted forecasts/figures -----------------------------
#------------------------------------------------------------------------------#
# About: This section creates the arrows that goes from forecast/figure to     #
# forecast/figure. Additionally, it renders the formatted forecasts which      #
# will show if selected by the user.                                           #
#------------------------------------------------------------------------------#
   
   #####################################################################
   # Creating the reactive value to be used with the formatted buttons #
   #####################################################################
   current_index_formatted <- reactiveVal(1)
   
   #################################################
   # Going backwards if the previous button is hit #
   #################################################
   observeEvent(input$PreviousFigure, {
     
     # Isolating the action to only when the button is clicked
     isolate({
       
       # Running if the current index is greater than one
       if(current_index_formatted() > 1){
         
         # Changing the index of the reactive value
         current_index_formatted(max(current_index_formatted() - 1))
         
       }
       
     }) # End of 'isolate' statement
     
   }) # End of 'observeEvent' statement
   
   
   ############################################
   # Going forwards if the next button is hit #
   ############################################
   observeEvent(input$NextFigure, {
     
     # Isolating the action to only when the button is clicked
     isolate({
       
       # Run if the current index is less than the length of the list
       if (current_index_formatted() < length(finalForecastCombined$data) & current_index_formatted() < length(finalForecastCombined$data)) {
         
         # Changing the index of the reactive value
         current_index_formatted(min(current_index_formatted() + 1))
         
       }
       
     }) # End of 'isolate' statement
     
   }) # End of 'observeEvent' statement
   
   ######################################################
   # Fixes the index when the data is filtered - Models #
   ######################################################
   observeEvent(input$modelsForecastFigs, {
     
     # Isolate the behavior to when the button is clicked
     isolate({
       
       current_index_formatted(1)
       
     }) # End of isolate
     
   }) # End of 'observeEvent'
   
   #########################################################
   # Fixes the index when the data is filtered - Locations #
   #########################################################
   observeEvent(input$locationFigures, {
     
     # Isolate the behavior to when the button is clicked
     isolate({
       
       current_index_formatted(1)
       
     }) # End of isolate
     
   }) # End of 'observeEvent'
   
   ######################################################
   # Rendering the title for the formatted forecast box #
   ######################################################
   output$Formatted.ForecastTitle <- renderText({
     
     # Producing nothing if no locations or models are chosen
     if(length(input$locationsForecastFigs) == 0 || length(input$modelsForecastFigs) == 0 || is.null(finalForecastCombined$data)){
       
       # Returning NULL
       return(NULL)
       
     # Runs if at least one location or model is selected
     }else{
       
       # Rendering the title of the formatted forecast box 
       return(paste0(names(finalForecastCombined$data[current_index_formatted()]), " (", input$quantileSelection, "% ", "PI)"))
       
     } # End of 'if-else' creating the title 
     
   }) # End of render statement for quantile forecasts
   
   
   #######################################################
   # Rendering the current formatted forecast data frame #
   #######################################################
   output$Formatted.Forecast <- renderDataTable({
    
       # Rendering the data table
       return(datatable(finalForecastCombined$data[[current_index_formatted()]],
                        options = list(scrollX = T)))

     
   }) # End of render statement for formatted forecasts
   
   
#------------------------------------------------------------------------------#
# Downloading the formatted forecasts as a 'zip' file --------------------------
#------------------------------------------------------------------------------#
# About: This section uses the list of names from above and the list of shown  #
# formatted forecasts to create a 'zip' folder. The user can then select where #
# to save the formatted forecasts within their personal computer.              #
#------------------------------------------------------------------------------#
   
   output$download_FormatttedForecasts <- downloadHandler(
     
     ####################
     # Filename for ZIP #
     ####################
     filename = function(){
       
       paste("Formatted-Forecasts.zip", sep = "")
       
     },
     
     ############################################
     # Determining what should be in the folder #
     ############################################
     content = function(file){
       
       # Removing the message
       removeModal()
       
       # Creating a temp directory for files 
       temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
       
       # Physically creating the directory 
       dir.create(temp_directory)
       
       # Saving the ggplots 
       for (plot_name in names(finalForecastCombined$data)) {
         
         # Plot 
         plot_obj <- finalForecastCombined$data[[plot_name]]
         
         # If plot is found 
         if (!is.null(plot_obj)) {
           
           # File name 
           file_name <- glue("{plot_name}.csv")
           
           # Saving the csv
           write_csv(plot_obj, file.path(temp_directory, file_name))
           
         }
         
       }
       
       #####################
       # Create a zip file #
       #####################
       zip::zip(
         zipfile = file,
         files = dir(temp_directory),
         root = temp_directory
       )
       
     },
     
     contentType = "application/zip"
     
   ) # End of 'downloadHandler'
   
#------------------------------------------------------------------------------#
# Button to edit forecast figures ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the legend labels for the    #
# panel figure containing multiple types of forecasts.                         #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYFFPanel <- reactiveValues(logScale = "Original") 
  
  #######################################
  # Reactive value for the y-axis label #
  #######################################
  yAxisLabFFPanel <- reactiveValues(lab = "Count")
  
  ################################################
  # Reactive value for the number of date breaks #
  ################################################
  dateBreaksReactiveFFPanel <- reactiveValues(breaksDate = "1")
  
  #############################################
  # Reactive value for starting at the y-axis #
  #############################################
  startYReactiveFFPanel <- reactiveValues(check = "0")
  
  ###############################
  # Reactive value for dot size #
  ###############################
  dotSizeReactiveFFPanel <- reactiveValues(sizeVal = 2)
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editForecastFigures, {
    
    showModal(modalDialog(
      title = "Figure Options",
      h3("Y-Axis Options", style = "font-size: 15px;"),
      pickerInput("logScaleFFPanel", "Scale for Y-Axis:", c("Original", "Log(Base 10)"), selected = scaleYFFPanel$logScale), # Scale of y-axis
      pickerInput("zeroStartFFPanel", "Y-Axis Origin:", c("0", "Minimum value in data"), selected = startYReactiveFFPanel$check), # Starting of the y-axis
      textInput("yaxisTimeSeriesFFPanel", "Label for Y-Axis:", value = yAxisLabFFPanel$lab), # Label for y-axis
      h3("X-Axis Options", style = "font-size: 15px;"),
      textInput("dateBreaksTSFFPanel", "Number of Date Breaks (Label):", value = dateBreaksReactiveFFPanel$breaksDate), # Number of date breaks
      h3("Other Options", style = "font-size: 15px;"),
      numericInput("dotSizeFFPanel", "Size of the obs. data points:", value = dotSizeReactiveFFPanel$sizeVal, step = 0.01) # Data dot size option
    ))
    
  })
  
  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleFFPanel, {
    
    # Updating the scale
    scaleYFFPanel$logScale <- input$logScaleFFPanel
    
  })
  
  ###############################################################
  # Update the reactive value - Y-axis for the timeseries label #
  ###############################################################
  observeEvent(input$yaxisTimeSeriesFFPanel, {
    
    # Updating the y-axis label
    yAxisLabFFPanel$lab <- input$yaxisTimeSeriesFFPanel
    
  })
  
  #####################################################
  # Update the reactive value - Number of date breaks #
  #####################################################
  observeEvent(input$dateBreaksTSFFPanel,{
    
    # Updating the number of date breaks
    dateBreaksReactiveFFPanel$breaksDate <- input$dateBreaksTSFFPanel
    
  })
  
  ###############################################
  # Update the reactive value - Start at Y axis #
  ###############################################
  observeEvent(input$zeroStartFFPanel,{
    
    # Updating the start point for the y-axis
    startYReactiveFFPanel$check <- input$zeroStartFFPanel
    
  })
  
  #############################################
  # Update the reactive value - data dot size #
  #############################################
  observeEvent(input$dotSizeFFPanel,{
    
    # Updating the data point size
    dotSizeReactiveFFPanel$sizeVal <- input$dotSizeFFPanel
    
  })
  
#------------------------------------------------------------------------------#
# Producing list of forecast figures -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the list of formatted forecasts created above to   #
# produce a list of forecast figures based on the quantile selected by the     #
# user, along with other model and forecasting specifications. The output of   #
# this section is a list of ggplot figures used throughout the rest of the     #
# dashboard.                                                                   #
#------------------------------------------------------------------------------#
   
  #############################################################
  # Initialize `reactiveValues` to store the forecast figures #
  #############################################################
  figuresForecast <- reactiveValues()
  
  #######################################################
  # List of events to trigger the re-running of figures #
  #######################################################
  eventsTriggerIFig <- reactive({
    
    # List of events
    events <- list(scaleYFFPanel$logScale, foremattedForecasts$forecasts,
                   yAxisLabFFPanel$lab, dateBreaksReactiveFFPanel$breaksDate,
                   startYReactiveFFPanel$check, dotSizeReactiveFFPanel$sizeVal)
    
    # Returning the list
    return(events)
    
  })
  
  #####################################################
  # Observing changes in inputs/other reactive values #
  #####################################################
  observeEvent(eventsTriggerIFig(), ignoreInit = T, {
    
    if(!is.null(foremattedForecasts$forecasts)){
      
      ####################################################
      # 'TryCatch' statement to run when info is entered #
      ####################################################
      tryCatch({
        
        ###############################################
        # Function to create list of forecast figures #
        ###############################################
        
        ######################
        # Individual figures #
        ######################
        isolate({
          
        individual <- forecast.figures(formatted.forecast.input = foremattedForecasts$forecasts, # Formatted figures list
                                       data.type.input = dateValues$dates, # Date input
                                       smoothing.input = input$smoothingInput, # Smoothing input
                                       scaleYAxis.input = scaleYFFPanel$logScale, # Scale y-axis
                                       yAxisLabel.input = yAxisLabFFPanel$lab, # Y-axis label
                                       dateBreaks.input = dateBreaksReactiveFFPanel$breaksDate, # Date breaks
                                       startYPoint.input = startYReactiveFFPanel$check, # Y-axis start point
                                       dotSize.input = dotSizeReactiveFFPanel$sizeVal) # Dot size 
        
        })
        
        ###########################################################
        # Returning an error if there are infinities in the plots #
        ###########################################################
        
        # Checking for NAs 
        isNAIndFig <- individual[is.na(individual)]
        
        # Pulling the names with issues
        namesErrorIndFig <- c(names(isNAIndFig))
        
        #####################################################
        # Error if there are any forecasts that did not run #
        #####################################################
        if(length(namesErrorIndFig) > 0){
          
          # Error 
          shinyalert("Unable to produce the following figures due to infinity UB: ", 
                     paste(namesErrorIndFig, collapse = "\n"), type = "error")
          
        }
        
        #########################################################
        # Returning the list of lists if individual figures run #
        #########################################################
        
        # Determining which figures are not NA
        notNAIndFig <- individual[!is.na(individual)]
        
        # Saving the exported list to a reactive value
        figuresForecast$figure <- notNAIndFig
        
        
      #######################################
      # Runs when no information is entered #
      #######################################
      }, error = function(e){
        
        # Returns a NULL
        NULL
        
      }) # End of 'tryCatch' statement 
      
    }
    
  }) # End of 'observe' statement 
   
  
#------------------------------------------------------------------------------#
# Creating the filtering options for the formatted forecast figure -------------
#------------------------------------------------------------------------------#
# About: This section creates the formatted forecast data filter for the main  #
# page of the dashboard.                                                       #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################
  
  # To store the filtered formatted forecast figure
  finalForecastFigureCombined <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    #####################################
    # Running if information is entered #
    #####################################
    tryCatch({
      
      # Running only if an error is not returned above
      if(all(!is.null(foremattedForecasts$forecasts))){
        
        ##################################################
        # Function to filter the formatted forecast data #
        ##################################################
        
        filteredForecastsFigure <- filteringFormattedIndivFigs(formattedForecast.input = figuresForecast$figure, # Formatted forecast
                                                               modelFilterFF.input = input$modelsForecastFigs, # Model filter
                                                               locationFilterFF.input = input$locationsForecastFigs, # Location filter
                                                               indicator.input = formattedForecastMAINIndicator()) # Indicator

        
        

        
        # Saving the results to the reactive value
        finalForecastFigureCombined$data <- filteredForecastsFigure
        
      } # End of 'else'
      
      ####################################
      # Running if data is NOT available #
      ####################################
    }, error = function(e){
      
      NULL
      
    })
    
  })
  
   
#------------------------------------------------------------------------------#
# Resetting the index for formatted forecast for panel figures -----------------
#------------------------------------------------------------------------------#
# About: This section fixes an error that occurs when the current index is     #
# larger than the number of panel figures. Therefore, when the panel figure    #
# button is clicked, the current index resets.                                 #
#------------------------------------------------------------------------------#
   
   observeEvent(input$panelModelsForecasts, {
     
     current_index_formatted(1)
     
   })
   
   
#------------------------------------------------------------------------------#
# Producing list of forecast figure panels -------------------------------------
#------------------------------------------------------------------------------#
# About: This section, like the one above, takes in the list of formatted      #
# forecast figures to create panel figures. Panel figures refers to groups of  #
# figures by location and forecast period. Each panel has graphs for the       #
# models selected in the side panel.                                           #
#------------------------------------------------------------------------------#
   
  ###################################################################
  # Initialize `reactiveValues` to store the forecast figure panels #
  ###################################################################
  figuresForecastPanel <- reactiveValues()
   
  #####################################################
  # Observing changes in inputs/other reactive values #
  #####################################################
  observeEvent(eventsTriggerIFig(), ignoreInit = T, {
    
    if(!is.null(foremattedForecasts$forecasts)){
      
      ####################################################
      # 'TryCatch' statement to run when info is entered #
      ####################################################
      tryCatch({
        
        ###############################################
        # Function to create list of forecast figures #
        ###############################################
        
        #########################
        # Panel figures Isolate #
        #########################
        isolate({
          
             
             #################
             # Panel figures #
             #################
             panelOutput <- panel.forecast.figures(formatted.forecast.input = foremattedForecasts$forecasts, # Formatted figures
                                                   data.type.input = dateValues$dates, # Date type 
                                                   smoothing.input = input$smoothingInput, # Smoothing input 
                                                   scaleYAxis.input = scaleYFFPanel$logScale, # Scale y-axis
                                                   yAxisLabel.input = yAxisLabFFPanel$lab, # Y-axis label
                                                   dateBreaks.input = dateBreaksReactiveFFPanel$breaksDate, # Date breaks
                                                   startYPoint.input = startYReactiveFFPanel$check, # Y-axis start point
                                                   dotSize.input = dotSizeReactiveFFPanel$sizeVal) # Dot size 
             
             # Reversing the order of the list
             panelOutput <- rev(panelOutput)
             
             # Saving the exported list to a reactive value
             figuresForecastPanel$figure <- panelOutput
             
             
        }) # End of 'isolate' statement 
        
      #######################################
      # Runs when no information is entered #
      #######################################
      }, error = function(e){
        
        # Returns a NULL
        NULL
        
      }) # End of 'tryCatch' statement 
      
    } # End of "if statement"
    
  }) # End of 'observeEvent'

#------------------------------------------------------------------------------#
# Determining which plot to render ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines which type of plot to render, individual or   #
# panel based on if the check is hit.                                          #
#------------------------------------------------------------------------------#
  
  ########################################
  # Reactive value to save final figures #
  ########################################
  finalFiguresDashboard <- reactiveValues()
  
  #####################################
  # Determining which plots to render #
  #####################################
  observe({
    
    #############################
    # Showing the panel figures #
    #############################
    if(input$panelModelsForecasts){
     
      # List of individual figures 
      finalFiguresDashboard$data <- figuresForecastPanel$figure
      
  
    # Showing the individual figure  
    }else{
      
      finalFiguresDashboard$data <- finalForecastFigureCombined$data
      
    }
    
  })
  
#------------------------------------------------------------------------------#
# Wiping the plots if the data set is changed ----------------------------------
#------------------------------------------------------------------------------#
# About: This section wipes the existing plots if the primary data set is      #
# changed.                                                                     #
#------------------------------------------------------------------------------#
  
  observeEvent(file(), {
    
    # Resetting the list containing individual figures
    finalForecastFigureCombined$data <- NULL
    
    # Resetting the list containing panel figures
    figuresForecastPanel$figure <- NULL
    
    # Resetting the reactive value containing the figures to output 
    finalFiguresDashboard$data <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Rendering a title for the panel plots ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders a title for the panel forecast plots.            #
#------------------------------------------------------------------------------#
   
   #######################
   # Rendering the title #
   #######################
   output$panelForecastTitle <- renderText({
     
     #Runs if data is entered 
     tryCatch({
       
         if(is.null(finalForecastCombined$data)){
           
           return(NULL)
           
         }else{
           
           # Rendering the title
           return(paste0(names(finalFiguresDashboard$data[current_index_formatted()]), " (", input$quantileSelection, "% ", "PI)"))
           
         }
         
       
       # Runs if no data is entered  
     }, error = function(e){
       
       # Returning a NULL
       NULL
       
     }) # End of 'tryCatch'
     
     })
    
#------------------------------------------------------------------------------#  
# Rendering the plots ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the correct plots, and allows the arrows to work.#
#------------------------------------------------------------------------------#
   
   ########################################################
   # Rendering the 'plotly' figures to the main dashboard #
   ########################################################
   output$Forecast.Figure <- renderPlotly({
     
     # Runs if data is entered 
     tryCatch({
       
         
         # Rendering the figure
         return(
           
           ggplotly(finalFiguresDashboard$data[[current_index_formatted()]], tooltip = "text") 
           
           )
         
     
     # Runs if no data is entered  
     }, error = function(e){
       
       # Returning a NULL
       NULL
       
     }) # End of 'tryCatch'
     
   }) # End of 'renderPlotly'
   

#------------------------------------------------------------------------------#
# Downloading the figures pop-up -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up for the download button associated    #
# with the figures box. It allows users to select the dpi, height, width,      #
# unit of measurement for size, and type of photo. Additionally, it allows     #
# users to download the figure with the user specifications.                   #
#------------------------------------------------------------------------------#
   
   #################################
   # Setting Figure Specifications #
   #################################
   observeEvent(input$forecastFigure, {
     
     ################################
     # Figure specification options #
     ################################
     isolate({
       
       showModal(modalDialog(
         
         title = "Figure Specifications",
         numericInput("dpi", "Figure DPI:", value = 900),
         numericInput("width", "Figure Width:", value = 9),
         numericInput('height', 'Figure Height:', value = 5),
         pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
         pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
         downloadButton("downloadForecastFigure", "Download Forecast Figure(s)"),
         easyClose = TRUE
         
         ))
       
       }) # End of 'isolate' statement 
     
     }) # End of 'observeEvent' statement
   
   
#------------------------------------------------------------------------------#
# Downloading the figure(s) ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the figure specifications from the menu pop-up to   #
# save a '.zip' file of forecast figures or panels.                            #
#------------------------------------------------------------------------------#
   
   ##########################
   # Downloading the images #
   ##########################
   output$downloadForecastFigure <- downloadHandler(
     
     ####################
     # Filename for ZIP #
     ####################
     filename = function(){
       
       paste("Forecast-Figures.zip", sep = "")
       
     },
     
     ############################################
     # Determining what should be in the folder #
     ############################################
     content = function(file){
       
       # Removing the message
       removeModal()
       
       # Creating a temp directory for files 
       temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
       
       # Physically creating the directory 
       dir.create(temp_directory)
       
       # Saving the ggplots 
       for (plot_name in names(finalFiguresDashboard$data)) {
         
         # Plot 
         plot_obj <- finalFiguresDashboard$data[[plot_name]]
         
         # If plot is found 
         if (!is.null(plot_obj)) {
           
           # File name 
           file_name <- glue("{plot_name}.{input$extFig}")
           
           # TIFF file 
           if(input$extFig == "tiff"){
             ggsave(
               file.path(temp_directory, file_name),
               plot = plot_obj,
               dpi = input$dpi,
               width = input$width,
               height = input$height,
               units = input$units,
               device = input$extFig,
               compression = "lzw")
             
           }else{
             
             # All other image types
             ggsave(
               file.path(temp_directory, file_name),
               plot = plot_obj,
               dpi = input$dpi,
               width = input$width,
               height = input$height,
               units = input$units,
               device = input$extFig)
             
           }
           
         }
         
       }
       
       #####################
       # Create a zip file #
       #####################
       zip::zip(
         zipfile = file,
         files = dir(temp_directory),
         root = temp_directory
       )
       
     },
     contentType = "application/zip"
   )

   

# MODEL METRICS PAGE -----------------------------------------------------------
 
#------------------------------------------------------------------------------#
# Model Fit  Metrics -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes in the quantile forecast list from above, the      #
# original data, the date type, and the calibration period size to apply the   #
# function which produces the model fit metrics - MSE, MAE, WIS, and PI. The   #
# section focuses only on the crude metrics.                                   #
#------------------------------------------------------------------------------#
   
   ############################################################
   # Initialize reactiveValues to store the model fit metrics #
   ############################################################
   modelFitMetricsList <- reactiveValues()
   
   ########################################
   # Observing changes in input variables #
   ########################################
   observe({
     
     tryCatch({

       #################################################
       # Running the function to get model fit metrics #
       #################################################
       modelFitOutput <- modelFitMetrics(crude.data.input = file(), # Crude data 
                                         calibration.input = input$calibrationPeriod, # Calibration period 
                                         date.Type.input = dateValues$dates, # Date type 
                                         quantile.list.input = c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet)) # List of quantile forecasts
       
       #########################################################
       # Returning a notification if ARIMA models were present #
       #########################################################
       if(modelFitOutput[[2]] == 1 & input$metricsToShow == "Model Fit" & input$my_picker == "Model Metrics"){
         
         shinyalert("Model fit metrics are not avaliable for ARIMA models", type = "warning")
         
       }
       
       #####################################################
       # Adding the forecast metrics to the reactive value #
       #####################################################
       modelFitMetricsList$fitMetrics <- modelFitOutput[[1]]
       
       ###############################################
       # Returns a NULL if no information is entered #
       ###############################################
       }, error = function(e){
         
         NULL
         
         }) # End of 'tryCatch'
     
     }) # End of 'observe'
   

#------------------------------------------------------------------------------#
# Calculating the forecasting metrics ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes in the crude data, forecasting horizon, type of    #
# date data, and list of quantile forecast to calculate the MSE, MAE, WIS, and #
# 95% PI for the applicable forecasts. For those it can't run, it returns a    #
# message to the user.                                                         #
#------------------------------------------------------------------------------#
   
   ###########################################################
   # Initialize reactiveValues to store the forecast metrics #
   ###########################################################
   forecastMetricsListCrude <- reactiveValues()
   
   ##############################
   # Observering changes inputs #
   ##############################
   observe({
       
         ##################################
         # Running if inputs are selected #
         ##################################
         tryCatch({
           
           ########################################
           # Function to produce forecast metrics #
           ########################################
           forecastMetricsList <- forecastingMetrics(crude.data.input = file(), # Crude data 
                                                     horizon.input = input$forecastHorizon, # Horizon 
                                                     date.Type.input = dateValues$dates, # Date type 
                                                     quantile.list.input = c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet)) # Quantile list
           
           ###################
           # Reactive values #
           ###################
           forecastMetricsFinal <- forecastMetricsList
               
           # Adding it to the reactive value
           forecastMetricsListCrude$forecastMetrics <- forecastMetricsFinal

         ##################################
         # Runs if no inputs were entered #
         ##################################
         }, error = function(e){
           
           # NULL
           NULL
           
         }) # End of 'tryCatch'

   }) # End of 'observe'
         
         
#------------------------------------------------------------------------------#
# Creating the list of crude metrics to show -----------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if it should be working with the crude model  #
# fit metrics or forecast metrics. Next, it filters the list based on the user #
# selected options in the box.                                                 #
#------------------------------------------------------------------------------#
   
   ########################################################
   # Initialize reactiveValues to store the shown metrics #
   ########################################################
   modelMetricsCrude <- reactiveValues()
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     # Empty list 
     metrics <- NA
     
     ################################
     # Runs if options are selected #
     ################################
     tryCatch({
       
       #####################################
       # Runs if the model fit is selected #
       #####################################
       if(input$metricsToShow == "Model Fit"){
         
         # Using model fit metrics 
         metrics <- modelFitMetricsList$fitMetrics
       
       ########################################
       # Runs if forecast metrics is selected #
       ########################################
       }else{
         
         # Handling when forecasting methods can not be evaluated
         if(nrow(forecastMetricsListCrude$forecastMetrics) == 0){
           
           # Alert 
           shinyalert("Not enough data to evaluate forecasting performance." , type = "error")
           
           # Indicator to clear output
           crudeMetricsFiltered$metricsFULL <- NULL
           
           # Clearing average output
           avgMetricsFiltered$metricsFULL <- NULL
           
         # Handling if forecast methods can be evaluated 
         }else{
         
           # Using forecast metrics 
           metrics <- forecastMetricsListCrude$forecastMetrics
           
         }
        
       }
       
       ###############################################
       # Adding the final list to the reactive value #
       ###############################################
       modelMetricsCrude$metricsList <- metrics
       
     ###################################
     # Runs if no information is added #
     ###################################
     }, error = function(e){
       
       # Returns a NULL
       NULL
       
     }) # End of 'tryCatch'
     
   }) # End of 'observe'
  
#------------------------------------------------------------------------------#
# Nulling out the reactive value -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section NULLs out the reactive value if the original data is     #
# changed.                                                                     #
#------------------------------------------------------------------------------#
  
  ####################################
  # Observing event of file changing #
  ####################################
  observeEvent(file(),{
    
    # Clearing the reactive value
    modelMetricsCrude$metricsList <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Creating the filtering options for the crude metrics data --------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options for the average metrics    #
# data table. The filtering options are triggered when the 'Filtering Options' #
# button is clicked.                                                           #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################
  
  # To store the filtered crude data
  crudeMetricsFiltered <- reactiveValues()
  
  # Model choices
  modelReactiveChoiceCrudeDASH <- reactiveValues()
  
  # Selected models
  modelReactiveSelectCrudeDASH <- reactiveValues()
  
  # Performance metric choice
  performanceReactiveChoiceCrudeDASH <- reactiveValues()
  
  # Selected performance metric choices
  performanceReactiveSelectCrudeDASH <- reactiveValues()
  
  # Location choices
  locationReactiveChoiceCrudeDASH <- reactiveValues()
  
  # Selected locations
  locationReactiveSelectCrudeDASH <- reactiveValues()
  
  # Indicator for which data to show
  indicatorForFilterMetricsCrudeDASH <- reactiveVal(0)
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    #####################################
    # Running if information is entered #
    #####################################
    tryCatch({
      
      # Running only if an error is not returned above
      if(!is.null(modelMetricsCrude$metricsList)){
        
        # Crude metrics data
        crudeMetrics <- modelMetricsCrude$metricsList
        
        #################
        # Model Options #
        #################
        
        # Choices
        modelReactiveChoiceCrudeDASH$data <- unique(crudeMetrics$Model)
        
        # Selected
        modelReactiveSelectCrudeDASH$data <- modelReactiveChoiceCrudeDASH$data
        
        ############################
        # Performance type metrics #
        ############################
        
        # Choices
        performanceReactiveChoiceCrudeDASH$data <- c("MSE", "MAE", "WIS", "95%PI")
        
        # Selected
        performanceReactiveSelectCrudeDASH$data <- performanceReactiveChoiceCrudeDASH$data
        
        ####################
        # Location Options #
        ####################
        
        # Choices
        locationReactiveChoiceCrudeDASH$data <- unique(crudeMetrics$Location)
        
        # Selected
        locationReactiveSelectCrudeDASH$data <- locationReactiveChoiceCrudeDASH$data
        
        #######################
        # Creating the pop-up #
        #######################
        observeEvent(input$filterCrudeMetrics, ignoreInit = T,{
          
          # Changing the indicator to one (i.e., button has been clicked)
          indicatorForFilterMetricsCrudeDASH(1)
        
          # Button
          showModal(modalDialog(
            title = "Filtering Options",
            pickerInput("modelMetrics", "Model:", c(modelReactiveChoiceCrudeDASH$data), selected = c(modelReactiveSelectCrudeDASH$data), multiple = T), # Model filtering
            pickerInput("perfTypeCrude", "Performance Metric Type:", c(performanceReactiveChoiceCrudeDASH$data), selected = c(performanceReactiveSelectCrudeDASH$data), multiple = T), # Metric Type
            pickerInput("locationMetrics", "Location:", c(locationReactiveChoiceCrudeDASH$data), selected = c(locationReactiveSelectCrudeDASH$data), multiple = T), # Location
            
          ))
        
        })
        
        #########################################
        # Function to filter the crude metrics  #
        #########################################
        filteredMetricsCrude <- filterMetricsCRUDE(crudeMetrics.input = modelMetricsCrude$metricsList, # Crude metrics
                                                   averageMetrics.input = NULL, # Average metrics
                                                   crudeModel.input = input$modelMetrics, # Crude model choices
                                                   crudePerformance.input = input$perfTypeCrude, # Crude performance metric type
                                                   crudeLocation.input = input$locationMetrics, # Crude location choice
                                                   AverageModel.input = NULL, # Average model choice
                                                   AveragePerformance.input = NULL, # Average performance metric type
                                                   AverageLocation.input = NULL, # Average location choice
                                                   inputindicator = indicatorForFilterMetricsCrudeDASH()) # Indicator for data to show
          
        
        # Saving the results to the reactive value
        crudeMetricsFiltered$metricsFULL <- filteredMetricsCrude
        
      } # End of 'else'
      
      ####################################
      # Running if data is NOT available #
      ####################################
    }, error = function(e){
      
      NULL
    })
    
  })
  
#------------------------------------------------------------------------------#
# Nulling out the reactive value -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section NULLs out the reactive value if the original data is      #
# changed.                                                                     #
#------------------------------------------------------------------------------#
  
  ####################################
  # Observing event of file changing #
  ####################################
  observeEvent(file(),{
    
    # Clearing the reactive value
    crudeMetricsFiltered$metricsFULL <- NULL
    
    # Resetting the indicator
    indicatorForFilterMetricsCrudeDASH(0)
    
  })
  
#------------------------------------------------------------------------------#
# Rendering the data frame for crude metrics -----------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the data table containing either the crude fit   #
# or forecasting metrics for the selected locations, models, and inputted      #
# data.                                                                        #
#------------------------------------------------------------------------------#
   
   ##########################
   # Rending the data table #
   ##########################
   output$CrudeMetricsData <- renderDataTable({
     
       # Rendering the data table
       return(datatable(crudeMetricsFiltered$metricsFULL, 
                        options = list(scrollX = T)))
       
   }) # End of rendering the crude metrics
   
   
#------------------------------------------------------------------------------#
# Downloading the crude metrics as a 'zip' file --------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the list of names from above and the list of shown  #
# crude metrics to create a 'zip' folder. The user can then select where to    #
# save the crude metrics within their personal computer.                       #
#------------------------------------------------------------------------------#
   
   output$download_metrics <- downloadHandler(
     
     ##########################################################
     # Function to create the file-name to save crude metrics #
     ##########################################################
     filename = function() {
       
       paste("crude-metrics-data-", input$dataset, sep = "")
       
     },
     
     #######################################
     # Function to save the file - as .csv #
     #######################################
     content = function(file) {
       
       # Saving the file
       write.csv(data.frame(crudeMetricsFiltered$metricsFULL), file, row.names = FALSE)
       
     }
     
   ) # End of download button 
   
#------------------------------------------------------------------------------#
# Button to edit crude metrics figures -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the legend labels for the    #
# panel figures of crude metrics.                                              #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYCrudeMetric <- reactiveValues(logScale = NULL) 
  
  ########################################
  # Reactive value for the low-end color #
  ########################################
  lowColorCrudeMetric <- reactiveValues(lowColor = "#6495ED")
  
  #########################################
  # Reactive value for the high-end color #
  #########################################
  highColorCrudeMetric <- reactiveValues(highColor = "#D0312D")
  
  ########################################
  # Reactive value for the outline color #
  ########################################
  outlineColorCrudeMetric <- reactiveValues(outlineColor = "#FFFFFF")
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$figOptCRUDEMetrics, {
    
    # Creating the pop-up
    showModal(modalDialog(
      title = "Figure Options",
      pickerInput("logScaleCrudeMetric", "Metrics to Show in Log Base 10:", c("MSE", "MAE", "WIS", "95%PI"), selected = scaleYCrudeMetric$logScale, multiple = T),
      textInput("lowColorCrude", "Color for the lowest values (#XXXXXX):", value = lowColorCrudeMetric$lowColor),
      textInput("highColorCrude", "Color for the highest values (#XXXXXX):", value = highColorCrudeMetric$highColor),
      textInput("outlineColorCrude", "Color for the outline of tiles (#XXXXXX):", value = outlineColorCrudeMetric$outlineColor),
    ))
    
  })
  
  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleCrudeMetric, {
    
    scaleYCrudeMetric$logScale <- input$logScaleCrudeMetric
    
  })
  
  ##########################################
  # Update the reactive value - Low colors #
  ##########################################
  observeEvent(input$lowColorCrude,{
    
    # Updating the number of date breaks
    lowColorCrudeMetric$lowColor <- input$lowColorCrude
    
  })
  
  ###########################################
  # Update the reactive value - High colors #
  ###########################################
  observeEvent(input$highColorCrude,{
    
    # Updating the number of date breaks
    highColorCrudeMetric$highColor <- input$highColorCrude
    
  })
  
  ##############################################
  # Update the reactive value - Outline colors #
  ##############################################
  observeEvent(input$outlineColorCrude,{
    
    # Updating the number of date breaks
    outlineColorCrudeMetric$outlineColor <- input$outlineColorCrude
    
  })
  
  
#------------------------------------------------------------------------------#
# Creating the tile heat map for the crude forecast metrics --------------------
#------------------------------------------------------------------------------#
# About: This section creates tile heat maps showing the forecast or fit model #
# performance across all forecasting dates for each country/group of interest. #
#------------------------------------------------------------------------------#
   
   ####################################################################
   # Initialize reactiveValues to store the model fit metrics figures #
   ####################################################################
   modelCrudePlot <- reactiveValues()
   
   ############################################
   # Observing changes in the reactive values #
   ############################################
   observe({
     
     ###################################
     # Running if options are selected #
     ###################################
     tryCatch({
       
       ##################################
       # Function to produce panel plot #
       ##################################
       if(!is.null(crudeMetricsFiltered$metricsFULL)){
         
         metricPanelCrude <- CrudeMetricsFigure(crudeMetrics = crudeMetricsFiltered$metricsFULL, 
                                                dateType = dateValues$dates,
                                                scaleY.input = scaleYCrudeMetric$logScale, 
                                                lowColor.input = lowColorCrudeMetric$lowColor,
                                                highColor.input = highColorCrudeMetric$highColor, 
                                                outlineColor.input = outlineColorCrudeMetric$outlineColor)
         
       }
       
       # Adding the figures to the reactive value
       modelCrudePlot$figures <- metricPanelCrude

       
     ##################################
     # Runs if no inputs are selected #
     ##################################
     }, error = function(e){
       
       # Returning a NULL
       NULL
       
     }) # End of 'tryCatch'
     
   }) # End of 'observe'
  
#------------------------------------------------------------------------------#
# Clearing the crude model figures ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the crude model figures list when the orignal     #
# data is changed.                                                             #
#------------------------------------------------------------------------------#
  
  ######################
  # If file is changed #
  ######################
  observeEvent(file(), {
    
    # Clearing the list of crude figures
    modelCrudePlot$figures <- NULL
    
  })
  
  #########################################
  # If forecast metrics are not available #
  #########################################
  observe({
    
    if(is.null(crudeMetricsFiltered$metricsFULL)){
      
      # Clearing the list of crude figures
      modelCrudePlot$figures <- NULL
      
    }
    
  })
  
   
#------------------------------------------------------------------------------#
# Setting up the reactivity of the arrows going through crude figures ----------
#------------------------------------------------------------------------------#
# About: This section creates the reactivity of the foward and backwards       #
# arrows for the crude forecast and fit metrics for each location.             #
#------------------------------------------------------------------------------#
   
   ###################################################################
   # Creating the reactive value to be used with the metrics buttons #
   ###################################################################
   current_index_Metrics <- reactiveVal(1)
   
   
   #################################################
   # Going backwards if the previous button is hit #
   #################################################
   observeEvent(input$PreviousMetricFigure, {
     
     # Isolating the action to only when the button is clicked
     isolate({
       
       # Running if the current index is greater than one
       if(current_index_Metrics() > 1){
         
         # Changing the index of the reactive value
         current_index_Metrics(max(current_index_Metrics() - 1))
         
       }
       
     }) # End of 'isolate' statement
     
   }) # End of 'observeEvent' statement
   
   ############################################
   # Going forwards if the next button is hit #
   ############################################
   observeEvent(input$NextMetricFigure, {
     
     # Isolating the action to only when the button is clicked
     isolate({
       
       # Run if the current index is less than the length of the list
       if (current_index_Metrics() < length(modelCrudePlot$figures)) {
         
         # Changing the index of the reactive value
         current_index_Metrics(min(current_index_Metrics() + 1))
         
       }
       
     }) # End of 'isolate' statement
     
   }) # End of 'observeEvent' statement
   
   
   ######################################################
   # Fixes the index when the data is filtered - Models #
   ######################################################
   observeEvent(input$modelMetrics, {
     
     # Isolate the behavior to when the button is clicked
     isolate({
       
       current_index_Metrics(1)
       
     }) # End of isolate
     
   }) # End of 'observeEvent'
   
   #########################################################
   # Fixes the index when the data is filtered - Locations #
   #########################################################
   observeEvent(input$locationMetrics, {
     
     # Isolate the behavior to when the button is clicked
     isolate({
       
       current_index_Metrics(1)
       
     }) # End of isolate
     
   }) # End of 'observeEvent'
   
#------------------------------------------------------------------------------#
# Rendering the title for each of the figures ----------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the figures for the crude metrics.               #
#------------------------------------------------------------------------------#

   output$CrudeMetricsTitle <- renderText({
     
       
       # Rendering the data table
       return(names(modelCrudePlot$figures)[current_index_Metrics()])
      
   }) # End of 'renderPlot'


#------------------------------------------------------------------------------#
# Rendering each of the figures ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates each of the figures shown in the main UI         #
# interface.                                                                   #
#------------------------------------------------------------------------------#
   
   output$CrudeMetricsFigure <- renderPlot({
    
       
       # Rendering the data table
       return(modelCrudePlot$figures[[current_index_Metrics()]])
       

   }) # End of 'renderPlot'
   

#------------------------------------------------------------------------------#
# Downloading the figures pop-up -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up for the download button associated    #
# with the figures box. It allows users to select the dpi, height, width,      #
# unit of measurement for size, and type of photo. Additionally, it allows     #
# users to download the figure with the user specifications.                   #
#------------------------------------------------------------------------------#
   
   #################################
   # Setting Figure Specifications #
   #################################
   observeEvent(input$download_metricsFig, {
     
     ################################
     # Figure specification options #
     ################################
     isolate({
       
       showModal(modalDialog(
         
         title = "Figure Specifications",
         numericInput("dpi", "Figure DPI:", value = 900),
         numericInput("width", "Figure Width:", value = 9),
         numericInput('height', 'Figure Height:', value = 5),
         pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
         pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
         downloadButton("downloadCrudeMetricsFigure", "Download Crude Metrics Figure(s)"),
         easyClose = TRUE
         
       ))
       
     }) # End of 'isolate' statement 
     
   }) # End of 'observeEvent' statement
  
   
#------------------------------------------------------------------------------#
# Downloading the figure(s) ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the figure specifications from the menu pop-up to   #
# save a '.zip' file of crude metrics figures or panels.                       #
#------------------------------------------------------------------------------#
   
   ##############################################
   # Creating the option to download the figure #
   ##############################################
   output$downloadCrudeMetricsFigure <- downloadHandler(
     
     ####################
     # Filename for ZIP #
     ####################
     filename = function(){
       
       paste("Crude-Metrics-Figures.zip", sep = "")
       
     },
     
     ############################################
     # Determining what should be in the folder #
     ############################################
     content = function(file){
       
       # Removing the message
       removeModal()
       
       # Creating a temp directory for files 
       temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
       
       # Physically creating the directory 
       dir.create(temp_directory)
       
       # Saving the ggplots 
       for (plot_name in names(modelCrudePlot$figures)) {
         
         # Plot 
         plot_obj <- modelCrudePlot$figures[[plot_name]]
         
         # If plot is found 
         if (!is.null(plot_obj)) {
           
           # File name 
           file_name <- glue("{plot_name}.{input$extFig}")
           
           # TIFF file 
           if(input$extFig == "tiff"){
             ggsave(
               file.path(temp_directory, file_name),
               plot = plot_obj,
               dpi = input$dpi,
               width = input$width,
               height = input$height,
               units = input$units,
               device = input$extFig,
               compression = "lzw")
             
           }else{
             
             # All other image types
             ggsave(
               file.path(temp_directory, file_name),
               plot = plot_obj,
               dpi = input$dpi,
               width = input$width,
               height = input$height,
               units = input$units,
               device = input$extFig)
             
           }
           
         }
         
       }
       
       #####################
       # Create a zip file #
       #####################
       zip::zip(
         zipfile = file,
         files = dir(temp_directory),
         root = temp_directory
       )
       
     },
     
     contentType = "application/zip"
      
     )
   
   
#------------------------------------------------------------------------------#
# Calculating average metrics --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the average metrics over forecast periods.    #
# Therefore, each location and model has a set of metrics assigned to it.      #
#------------------------------------------------------------------------------#
   
   ######################################
   # Reactive value for average metrics #
   ######################################
   avgMetric <- reactiveValues()
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     ################################
     # Runs if options are selected #
     ################################
     tryCatch({
       
       metrics <- modelMetricsCrude$metricsList
       
       #####################################################
       # Running the function to calculate average metrics #
       #####################################################
       averageMetrics <- metrics %>%
         dplyr::group_by(Model, Location) %>% # Grouping by location and model
         dplyr::mutate(MSE = round(mean(MSE), 2), # Avg. MSE
                       MAE = round(mean(MAE), 2), # Avg. MAE
                       `95%PI` = round(mean(`95%PI`), 2), # Avg. PI
                       WIS = round(mean(WIS), 2)) %>% # Avg. WIS
         dplyr::select(Model, Location, MSE, MAE, `95%PI`, WIS) %>% # Selecting needed variables
         distinct(Model, Location, .keep_all = T) # Remove duplicate rows


       # Adding the metrics to the reactive value
       avgMetric$metricsList <- averageMetrics
       
     ###################################
     # Runs if inputs are not selected #
     ###################################
     }, error = function(e){
       
       # Null
       NULL
       
     }) # End of 'tryCatch'
     
   }) # End of 'observe'
  
#------------------------------------------------------------------------------#
# Nulling out the reactive value -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section NULLs out the reactive value if the orignal data is      #
# changed.                                                                     #
#------------------------------------------------------------------------------#
  
  ####################################
  # Observing event of file changing #
  ####################################
  observeEvent(file(),{
    
    # Clearing the reactive value
    avgMetric$metricsList <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Creating the filtering options for the average metrics data ------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options for the average metrics    #
# data table. The filtering options are triggered when the 'Filtering Options' #
# button is clicked.                                                           #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################
  
  # To store the filtered average data
  avgMetricsFiltered <- reactiveValues()
  
  # Model choices
  modelReactiveChoiceAvgDASH <- reactiveValues()
  
  # Selected models
  modelReactiveSelectAvgDASH <- reactiveValues()
  
  # Performance metric choice
  performanceReactiveChoiceAvgDASH <- reactiveValues()
  
  # Selected performance metric choices
  performanceReactiveSelectAvgDASH <- reactiveValues()
  
  # Location choices
  locationReactiveChoiceAvgDASH <- reactiveValues()
  
  # Selected locations
  locationReactiveSelectAvgDASH <- reactiveValues()
  
  # Indicator for which data to show
  indicatorForFilterMetricsAvgDASH <- reactiveVal(0)
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    #####################################
    # Running if information is entered #
    #####################################
    tryCatch({
      
      # Running only if an error is not returned above
      if(!is.null(avgMetric$metricsList)){
        
        # Average metrics data
        avgMetrics <- avgMetric$metricsList
        
        #################
        # Model Options #
        #################
        
        # Choices
        modelReactiveChoiceAvgDASH$data <- unique(avgMetrics$Model)
        
        # Selected
        modelReactiveSelectAvgDASH$data <- modelReactiveChoiceAvgDASH$data
        
        ############################
        # Performance type metrics #
        ############################
        
        # Choices
        performanceReactiveChoiceAvgDASH$data <- c("MSE", "MAE", "WIS", "95%PI")
        
        # Selected
        performanceReactiveSelectAvgDASH$data <- performanceReactiveChoiceAvgDASH$data
        
        ####################
        # Location Options #
        ####################
        
        # Choices
        locationReactiveChoiceAvgDASH$data <- unique(avgMetrics$Location)
        
        # Selected
        locationReactiveSelectAvgDASH$data <- locationReactiveChoiceAvgDASH$data
        
        #######################
        # Creating the pop-up #
        #######################
        observeEvent(input$filterAvgMetrics, ignoreInit = T,{
          
          # Changing the indicator to one (i.e., button has been clicked)
          indicatorForFilterMetricsAvgDASH(1)
          
          # Isolating button click behavior
        
            
            # Button
            showModal(modalDialog(
              title = "Filtering Options",
              pickerInput("modelAvgMetrics", "Model:", c(modelReactiveChoiceAvgDASH$data), selected = c(modelReactiveSelectAvgDASH$data), multiple = T), # Model filtering
              pickerInput("perfTypeAvg", "Performance Metric Type:", c(performanceReactiveChoiceAvgDASH$data), selected = c(performanceReactiveSelectAvgDASH$data), multiple = T), # Metric Type
              pickerInput("locationsAvgMetrics", "Location:", c(locationReactiveChoiceAvgDASH$data), selected = c(locationReactiveSelectAvgDASH$data), multiple = T), # Location
              
            ))
            
         
          
        })
        
        ###########################################
        # Function to filter the average metrics  #
        ###########################################
        filteredMetricsAvg  <- filterMetrics(crudeMetrics.input = NULL, # Crude metrics
                                             averageMetrics.input = avgMetric$metricsList, # Average metrics
                                             crudeModel.input = NULL, # Crude model choices
                                             crudePerformance.input = NULL, # Crude performance metric type
                                             crudeLocation.input = NULL, # Crude location choice
                                             AverageModel.input = input$modelAvgMetrics, # Average model choice
                                             AveragePerformance.input = input$perfTypeAvg, # Average performance metric type
                                             AverageLocation.input = input$locationsAvgMetrics, # Average location choice
                                             inputindicator = indicatorForFilterMetricsAvgDASH()) # Indicator for data to show
        

        # Saving the results to the reactive value
        avgMetricsFiltered$metricsFULL <- filteredMetricsAvg

      } # End of 'else'
      
    ####################################
    # Running if data is NOT available #
    ####################################
    }, error = function(e){
      
      NULL
    })
    
  })
  
#------------------------------------------------------------------------------#
# Nulling out the reactive value -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section NULLs out the reactive value if the orignal data is      #
# changed.                                                                     #
#------------------------------------------------------------------------------#
  
  ####################################
  # Observing event of file changing #
  ####################################
  observeEvent(file(),{
    
    # Clearing the reactive value
    avgMetricsFiltered$metricsFULL <- NULL
    
    # Resetting the indicator
    indicatorForFilterMetricsAvgDASH(0)
    
  })

#------------------------------------------------------------------------------#
# Rendering the data frame containing the average metrics ----------------------
#------------------------------------------------------------------------------#
# About: This section creates the data frame shown in the user UI.             #
#------------------------------------------------------------------------------#
   
   ##########################
   # Rending the data table #
   ##########################
   output$AvgMetricsData <- renderDataTable({
       
       # Rendering the data table
       return(datatable(as.data.frame(avgMetricsFiltered$metricsFULL),
                        options = list(scrollX = T)))
       
   }) # End of render statement for the data frame


#------------------------------------------------------------------------------#
# Downloading the average metrics as a 'zip' file ------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the list of names from above and the list of shown  #
# average metrics to create a 'zip' folder. The user can then select where to  #
# save the average metrics within their personal computer.                     #
#------------------------------------------------------------------------------#

   output$download_AvgMetrics <- downloadHandler(
     
     ##########################################################
     # Function to create the file-name to save crude metrics #
     ##########################################################
       filename = function() {
         
         paste("Average-Metrics-Metrics-", input$dataset, sep = "")
         
       },
       
       #######################################
       # Function to save the file - as .csv #
       #######################################
       content = function(file) {
         
         # Saving the file
         write.csv(data.frame(avgMetricsFiltered$metricsFULL), file, row.names = FALSE)
         
       }
       
     ) # End of download button 
  
#------------------------------------------------------------------------------#
# Button to edit average metrics figures ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the legend labels for the    #
# panel figures of average metrics.                                            #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYAvgMetric <- reactiveValues(logScale = c("MSE", "MAE", "WIS")) 
  
  ########################################
  # Reactive value for the low-end color #
  ########################################
  lowColorAvgMetric <- reactiveValues(lowColor = "#FFFFFF")
  
  #########################################
  # Reactive value for the high-end color #
  #########################################
  highColorAvgMetric <- reactiveValues(highColor = "#59788E")
  
  ########################################
  # Reactive value for the outline color #
  ########################################
  outlineColorAvgMetric <- reactiveValues(outlineColor = "#FFFFFF")
  
  ###################################
  # Reactive value for showing text #
  ###################################
  showTextAvgMetric <- reactiveValues(showText = TRUE)
  
  #################################
  # Reactive value for text color #
  #################################
  textColorAvgMetric <- reactiveValues(textColor = "#000000")
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editAvgMetrics, {
    
    # Creating the pop-up
    showModal(modalDialog(
      title = "Figure Options",
      pickerInput("logScaleAvgMetric", "Metrics to Show in Log Base 10:", c("MSE", "MAE", "WIS", "95%PI"), selected = scaleYAvgMetric$logScale, multiple = T),
      textInput("lowColorAvg", "Color for the lowest values (#XXXXXX):", value = lowColorAvgMetric$lowColor),
      textInput("highColorAvg", "Color for the highest values (#XXXXXX):", value = highColorAvgMetric$highColor),
      textInput("outlineColorAvg", "Color for the outline of tiles (#XXXXXX):", value = outlineColorAvgMetric$outlineColor),
      checkboxInput("showTextAvg", "Show text within each tile", value = showTextAvgMetric$showText),
      conditionalPanel(
        condition = "input.showTextAvg == true", # Condition to show the text input
        textInput("textColorAvg", "Color for the text (#XXXXXX):", value = textColorAvgMetric$textColor)
      )
    ))
    
  })
  
  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleAvgMetric, {
    
    scaleYAvgMetric$logScale <- input$logScaleAvgMetric
    
  })
  
  ##########################################
  # Update the reactive value - Low colors #
  ##########################################
  observeEvent(input$lowColorAvg,{
    
    # Updating the low color
    lowColorAvgMetric$lowColor <- input$lowColorAvg
    
  })
  
  ###########################################
  # Update the reactive value - High colors #
  ###########################################
  observeEvent(input$highColorAvg,{
    
    # Updating the high color
    highColorAvgMetric$highColor <- input$highColorAvg
    
  })
  
  ##############################################
  # Update the reactive value - Outline colors #
  ##############################################
  observeEvent(input$outlineColorAvg,{
    
    # Updating the outline color 
    outlineColorAvgMetric$outlineColor <- input$outlineColorAvg
    
  })
  
  #########################################
  # Update the reactive value - Show Text #
  #########################################
  observeEvent(input$showTextAvg,{
    
    # Updating the text color
    showTextAvgMetric$showText <- input$showTextAvg
    
  })
  
  
  ###########################################
  # Update the reactive value - Text colors #
  ###########################################
  observeEvent(input$textColorAvg,{
    
    # Updating the text color
    textColorAvgMetric$textColor <- input$textColorAvg
    
  })

#------------------------------------------------------------------------------#
# Creating the tile plot for average metrics -----------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the average metrics over time as a tile plot.      #
# However, the plot only shows if the user selects the check mark.             #
#------------------------------------------------------------------------------#
   
   ############################################################################
   # Initialize reactiveValues to store the average model fit metrics figures #
   ############################################################################
   modelAvgPlot <- reactiveValues()

   ############################################
   # Observing changes in the reactive values #
   ############################################
   observe({

     ###################################
     # Running if options are selected #
     ###################################
     tryCatch({

       ##################################
       # Function to produce panel plot #
       ##################################
       metricPanelAverage <- AverageMetricsPanel(avgMetrics.input = avgMetricsFiltered$metricsFULL,
                                                 dateType.input = dateValues$dates,
                                                 scaleY.input = scaleYAvgMetric$logScale, 
                                                 lowColor.input = lowColorAvgMetric$lowColor,  
                                                 highColor.input = highColorAvgMetric$highColor,  
                                                 outlineColor.input = outlineColorAvgMetric$outlineColor, 
                                                 textColor.input = textColorAvgMetric$textColor,
                                                 showText.input = showTextAvgMetric$showText)

       # Adding the figures to the reactive value
       modelAvgPlot$figures <- metricPanelAverage[[1]]

     ##################################
     # Runs if no inputs are selected #
     ##################################
     }, error = function(e){

       # Returning a NULL
       NULL

     }) # End of 'tryCatch'

   }) # End of 'observe'



#------------------------------------------------------------------------------#
# Rendering the plotly figure --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the plot showing the average metrics for each    #
# location and model of interest.                                              #
#------------------------------------------------------------------------------#
   
   output$AvgMetricsFigure <- renderPlot({


       # Rendering the data table
       return((modelAvgPlot$figures))

   }) # End of 'renderPlot'


#------------------------------------------------------------------------------#
# Downloading the figures pop-up -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up for the download button associated    #
# with the figures box. It allows users to select the dpi, height, width,      #
# unit of measurement for size, and type of photo. Additionally, it allows     #
# users to download the figure with the user specifications.                   #
#------------------------------------------------------------------------------#
   
   #################################
   # Setting Figure Specifications #
   #################################
   observeEvent(input$download_AvgmetricsFig, {
     
     ################################
     # Figure specification options #
     ################################
     isolate({
       
       showModal(modalDialog(
         
         title = "Figure Specifications",
         numericInput("dpi", "Figure DPI:", value = 900),
         numericInput("width", "Figure Width:", value = 9),
         numericInput('height', 'Figure Height:', value = 5),
         pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
         pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
         downloadButton("downloadAvgMetrics", "Download Average Metrics Figure"),
         easyClose = TRUE
         
       ))
       
     }) # End of 'isolate' statement
     
   }) # End of 'observeEvent' statement
   
     
   
#------------------------------------------------------------------------------#
# Downloading the figure -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section downloads the average metrics plot panel to the user's   #
# desired folder and with the users desired file options.                      #
#------------------------------------------------------------------------------#
   
   ##############################################
   # Creating the option to download the figure #
   ##############################################
   output$downloadAvgMetrics<- downloadHandler(
     
     ####################################
     # Function to create the file-name #
     ####################################
     filename = function() {
       
       # Closing the figure specification 
       removeModal()
       
       # Removing '.csv' 
       fileName <- gsub('.csv', '', input$dataset)
       
       # File name 
       paste(fileName, "-average-metrics-panel.", input$extFig, sep = "")
       
     },
     
     #############################
     # Function to save the file #
     #############################
     content = function(file) {
       
       # Running with compression if using a '.tiff'
       if(input$extFig == 'tiff'){
         
         # Saving the file
         ggsave(file, plot = modelAvgPlot$figures, 
                dpi = input$dpi,
                width = input$width, 
                height = input$height, 
                units = input$units,
                compression = "lzw")
         
         # Running without compression if not using a '.tiff'
       }else{
         
         # Saving the file
         ggsave(file, plot = modelAvgPlot$figures, 
                dpi = input$dpi,
                width = input$width, 
                height = input$height, 
                units = input$units)
       }
       
     }) # End of saving the figure(s) 
   
   
#------------------------------------------------------------------------------#
# Winkler Scores ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates Winkler scores for the produced ARIMA, GLM,   #
# GAM, Prophet models. The Winkler scores provide a quantitative measure of    #
# comparison for prediction interval coverage.                                 #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################
  
  # To store the final Winkler Scores data 
  winklerFinalAGGP <- reactiveValues()
  
  # Location choices 
  locationWinklerMainChoices <- reactiveValues()
  
  # Selected Locations 
  locationWinklerMainSelected <- reactiveValues()
  
  # Model choices
  modelWinklerMainChoices <- reactiveValues()
  
  # Selected Models 
  modelWinklerMainSelected <- reactiveValues()
  
  # Indicator for filtering
  filterWinklerMainIndicator <- reactiveVal(0)
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    #####################################
    # Running if information is entered #
    #####################################
    tryCatch({
      
      # Running if forecast data is available 
      if(all(!is.null(foremattedForecasts$forecasts))){
        
        ####################
        # Location Options #
        ####################
        
        # Choices 
        locationWinklerMainChoices$data <- c(input$locations)
        
        # Selected 
        locationWinklerMainSelected$data <- locationWinklerMainChoices$data
        
        #################
        # Model Options #
        #################
        
        # Choices
        modelWinklerMainChoices$data <- c(input$modelType)
        
        # Selected 
        modelWinklerMainSelected$data <- modelWinklerMainChoices$data
        
        #######################
        # Creating the pop-up #
        #######################
        observeEvent(input$filterWinklerDataMain, ignoreInit = T,{
          
          # Setting the indicator
          isolate({filterWinklerMainIndicator(1)})
          
          # Button
          showModal(modalDialog(
            title = "Filtering Options",
            pickerInput("locationsWinklerMain", "Location(s):", c(locationWinklerMainChoices$data), selected = c(locationWinklerMainSelected$data), multiple = T), # Location input 
            pickerInput("modelsWinklerMain", "Model(s):", c(modelWinklerMainChoices$data), selected = c(modelWinklerMainSelected$data), multiple = T) # Model Input
          
            ))

        })
        
        ##################################
        # Calculating the Winkler Scores #
        ##################################
        winkler.scores.output <- winkler.scores.AGGP(formattedForecasts = foremattedForecasts$forecasts, # Forecast files 
                                                     locations.input = input$locationsWinklerMain, # Filtered locations 
                                                     models.input = input$modelsWinklerMain, # Filtered models 
                                                     filterIndicator.input = filterWinklerMainIndicator(), # Filtering indicator 
                                                     averageIndicator.input = input$seeAverageWinklerMain, # Average indicator 
                                                     metricPage.input = input$metricsToShow) # Metric filtering 
        
        # Saving the results to the reactive value
        winklerFinalAGGP$scores <- winkler.scores.output
        
      } # End of 'else'
      
      ####################################
      # Running if data is NOT available #
      ####################################
    }, error = function(e){
      
      NULL
    })
    
  })
  
#------------------------------------------------------------------------------#
# Clearing the Winkler Scores Data ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the Winkler Scores data and resets the filtering  #
# indicator when a new data set is read into the dashboard.                    #
#------------------------------------------------------------------------------#
  
  ####################################
  # Observing the change in data set #
  ####################################
  observeEvent(file(), {
    
    # Resetting the Winkler Scores
    winklerFinalAGGP$scores <- NULL
    
    # Resetting the indicator 
    filterWinklerMainIndicator(0)
    
  })
   
#------------------------------------------------------------------------------#
# Rendering the Winkler Scores data frame --------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes in the winkler scores calculated above and returns #
# them to the main UI as a data table.                                         #
#------------------------------------------------------------------------------#
   
   ###########################################
   # Rendering the Winkler Scores Data Frame #
   ###########################################
   output$winklerDataTableAGGP <- renderDataTable({winklerFinalAGGP$scores}) 
     
#------------------------------------------------------------------------------#
# Downloading the Winkler Scores -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows users to download the Winkler Scores data as a    #
# '.csv' file to the directory of their choosing.                              #
#------------------------------------------------------------------------------#
   
   ##############################################
   # Creating the option to download the figure #
   ##############################################
   output$downloadWinkerData<- downloadHandler(
     
     ####################################
     # Function to create the file-name #
     ####################################
     filename = function() {
       
       # File name 
       paste("winkler-Scores-", input$dataset, sep = "")
       
     },
     
     #############################
     # Function to save the file #
     #############################
     content = function(file) {
       
       # Saving the file
       write.csv(winklerFinalAGGP$scores, file, row.names = FALSE)
       
     }
     
   ) # End of download button  
   
#------------------------------------------------------------------------------#
# Creating the Winkler Scores Figures ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the Winkler figures using the data from above.   #
# For each location/group included in the data, a line plot is outputted where #
# the y-axis is the Winkler Score and each line is a model. The resulting plot #
# is then rendered to the main dashboard.                                      #
#------------------------------------------------------------------------------#
   
   ################################################################
   # Reactive value to save list of figures for later user-saving #
   ################################################################
   WinklerFiguresSave <- reactiveValues()
   
   #######################################
   # Rendering the Winkler Socres Figure #
   #######################################
   output$winklerFigureAGGP <- renderPlot({
     
     ##############################
     # Running if no error occurs #
     ##############################
     tryCatch({
       
       # Function to produce figures 
       winklerFigures <- winkler.figure.AGGP(scoresFigure = winklerFinalAGGP$scores) # FIltering indicator
       
       # Saving output of function to a reactive value
       WinklerFiguresSave$figures <- winklerFigures
       
    
     ##############################
     # Running if an error occurs #
     ##############################
     }, error = function(e){
       
       NULL
       
     }) # End of 'tryCatch'
     
     ##############################################
     # Returning the figure to the main dashboard #
     ##############################################
     return(winklerFigures)
     
   }) # End of render
   

#------------------------------------------------------------------------------#
# Creating the figures pop-up --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up for the download button associated    #
# with the figures box. It allows users to select the dpi, height, width,      #
# unit of measurement for size, and type of photo. Additionally, it allows     #
# users to download the figure with the user specifications.                   #
#------------------------------------------------------------------------------#
   
   #################################
   # Setting Figure Specifications #
   #################################
   observeEvent(input$downloadWinklerMainFig, {
     
     ################################
     # Figure specification options #
     ################################
     isolate({
       
       showModal(modalDialog(
         
         title = "Figure Specifications",
         numericInput("dpi", "Figure DPI:", value = 900),
         numericInput("width", "Figure Width:", value = 9),
         numericInput('height', 'Figure Height:', value = 5),
         pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
         pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
         downloadButton("downloadWinklerFigAGGP", "Download Winkler Scores Figure"),
         easyClose = TRUE
         
       ))
       
     }) # End of 'isolate' statement
     
   }) # End of 'observeEvent' statement
   

#------------------------------------------------------------------------------#
# Downloading the Winkler Scores Figure ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the download handler for the Winkler figure. It  #
# allows the user to click the download button, and save the figure to the     #
# folder of their choosing.                                                    #
#------------------------------------------------------------------------------#
   
   ##############################################
   # Creating the option to download the figure #
   ##############################################
   output$downloadWinklerFigAGGP <- downloadHandler(
     
     ####################################
     # Function to create the file-name #
     ####################################
     filename = function() {
       
       # Closing the figure specification 
       removeModal()
       
       # Removing '.csv' 
       fileName <- gsub('.csv', '', input$dataset)
       
       # File name 
       paste(fileName, "-Winkler-Scores.", input$extFig, sep = "")
       
     },
     
     #############################
     # Function to save the file #
     #############################
     content = function(file) {
       
       # Running with compression if using a '.tiff'
       if(input$extFig == 'tiff'){
         
         # Saving the file
         ggsave(file, plot = WinklerFiguresSave$figures, 
                dpi = input$dpi,
                width = input$width, 
                height = input$height, 
                units = input$units,
                compression = "lzw")
         
       # Running without compression if not using a '.tiff'
       }else{
         
         # Saving the file
         ggsave(file, plot = WinklerFiguresSave$figures, 
                dpi = input$dpi,
                width = input$width, 
                height = input$height, 
                units = input$units)
       }
       
     }) # End of saving the figure(s) 
  
  
#------------------------------------------------------------------------------#
# Creating the filtering options for the Skill Scores data ---------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options for the Skill Scores       #
# data table. The filtering options are triggered when the 'Filtering Options' #
# button is clicked.                                                           #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################
  
  # To store the filtered Skill Scores data
  finalSSCombinedMAIN <- reactiveValues()
  
  # Model choices - Baseline
  modelReactiveChoiceBaselineMAIN <- reactiveValues()
  
  # Selected models - Baseline
  modelReactiveSelectBaselineMAIN <- reactiveValues()
  
  # Model choices - Comparison
  modelReactiveChoiceComparisonMAIN <- reactiveValues()
  
  # Selected models - Comparison
  modelReactiveSelectComparisonMAIN <- reactiveValues()
  
  # Location choices
  locationReactiveChoiceSSMAIN <- reactiveValues()
  
  # Selected locations
  locationReactiveSelectSSMAIN <- reactiveValues()
  
  # Indicator for which metrics to show
  filterSSIndicatorMAIN <- reactiveVal(0)
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Crude Metrics Data
    crudeMetData <- modelMetricsCrude$metricsList
    
    #####################################
    # Running if information is entered #
    #####################################
    tryCatch({
      
      # Reading in the crude data
      if(all(!is.null(crudeMetData))){
        
        ############################
        # Model Options - Baseline #
        ############################
        
        # Choices
        modelReactiveChoiceBaselineMAIN$data <- c(unique(crudeMetData$Model))
        
        # Selected
        modelReactiveSelectBaselineMAIN$data <- modelReactiveChoiceBaselineMAIN$data
        
        ##############################
        # Model Options - Comparison #
        ##############################
        
        # Choices
        modelReactiveChoiceComparisonMAIN$data <- c(unique(crudeMetData$Model))
        
        # Selected
        modelReactiveSelectComparisonMAIN$data <- modelReactiveChoiceComparisonMAIN$data
        
        ####################
        # Location Options #
        ####################
        
        # Choices
        locationReactiveChoiceSSMAIN$data <- c(unique(crudeMetData$Location))
        
        # Selected
        locationReactiveSelectSSMAIN$data <- locationReactiveChoiceSSMAIN$data
        
        #######################
        # Creating the pop-up #
        #######################
        observeEvent(input$filterSSDataMain, ignoreInit = T,{
          
          # Setting the indicator
          isolate({filterSSIndicatorMAIN(1)})
          
          # Button
          showModal(modalDialog(
            title = "Filtering Options",
            pickerInput("baselineModelsMAIN", "Baseline Model(s):", c(modelReactiveChoiceBaselineMAIN$data), selected = c(modelReactiveSelectBaselineMAIN$data), multiple = T), # Model filtering - Baseline
            pickerInput("compareModels2MAIN", "Comparison Model(s):", c(modelReactiveChoiceComparisonMAIN$data), selected = c(modelReactiveSelectComparisonMAIN$data), multiple = T), # Model filtering - Comparison
            pickerInput("locationInputSelectMAIN", "Location:", c(locationReactiveChoiceSSMAIN$data), selected = c(locationReactiveSelectSSMAIN$data), multiple = T), # Location
 
          ))
          
          
        })
        
        ######################################
        # Obtaining the crude winkler scores #
        ######################################
        winklerScoresDATESPECIFIC <- winkler.scores.AGGP(formattedForecasts = foremattedForecasts$forecasts, # Forecast files
                                                         locations.input = input$locationsWinklerMain, # Filtered locations
                                                         models.input = input$modelsWinklerMain, # Filtered models
                                                         filterIndicator.input = 0, # Filtering indicator
                                                         averageIndicator.input = F, # Average indicator
                                                         metricPage.input = input$metricsToShow) # Metric filtering


        ################################
        # Calculating the Skill Scores #
        ################################
        skillScores <- skillScoresMain(averageIndicator= input$seeAvgSS,
                                       locationsFilter = input$locationInputSelectMAIN,
                                       CrudeMetrics = modelMetricsCrude$metricsList,
                                       benchModel = input$baselineModelsMAIN,
                                       compModels = input$compareModels2MAIN,
                                       winkler.input = winklerScoresDATESPECIFIC,
                                       filterIndicator.input = filterSSIndicatorMAIN())
        

       # Saving the output to a reactive value
       finalSSCombinedMAIN$scores <- skillScores
        
        
      } # End of 'else'
      
      ####################################
      # Running if data is NOT available #
      ####################################
    }, error = function(e){
      
      NULL
    })
    
  })
   
#------------------------------------------------------------------------------#
# Resetting the skill scores data ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section resets the skill scores input and related indicator when #
# the original data set is changed.                                            #
#------------------------------------------------------------------------------#
  
  ####################################
  # Observing the change in the data #
  ####################################
  observeEvent(file(), {
    
    # Resetting the skill scores
    finalSSCombinedMAIN$scores <- NULL
    
    # Resetting the indicator 
    filterSSIndicatorMAIN(0)
    
  })
         
#------------------------------------------------------------------------------#
# Rendering the skill scores data ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the skill scores the to the main dashboard.      #
#------------------------------------------------------------------------------#
   
   ############################
   # Rendering the data frame #
   ############################
   output$skillScoresAGGPData <- renderDataTable({finalSSCombinedMAIN$scores})
     

#------------------------------------------------------------------------------#
# Downloading the skill scores as a '.csv' -------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the skill scores data as a '.csv' file to the       #
# directory of their choosing.                                                 #
#------------------------------------------------------------------------------#
   
  output$downloadSSMetrics <- downloadHandler(
    
    ####################################
    # Function to create the file-name #
    ####################################
    filename = function() {
      
      # File name 
      paste("skill-scores-", input$dataset, sep = "")
      
    },
    
    #############################
    # Function to save the file #
    #############################
    content = function(file) {
      
      # Saving the file
      write.csv(finalSSCombinedMAIN$scores, file, row.names = FALSE)
      
    }
    
  ) # End of download button
  
#------------------------------------------------------------------------------#
# Creating the figures for skill scores ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section calls the skill scores figure function, and produces the #
# reactive value with the figures to return to the main dashboard.             #
#------------------------------------------------------------------------------#
   
   ##################################
   # Reactive value to save figures #
   ##################################
   skillScoresFigs <- reactiveValues()
   
   ############################################
   # Observing changes in the reactive values #
   ############################################
   observe({
     
     # Runs if no error occurs
     tryCatch({
       
       #####################################
       # Creating the skill scores figures #
       #####################################
       ssFigOutput <- skill.scores.figures.AGGP(skillScores = finalSSCombinedMAIN$scores)
       
       # Saving the output to the reactive value
       skillScoresFigs$figList <- ssFigOutput
       
     # Runs if error occurs
     }, error = function(e){
       
       NULL
       
     })

   }) # End of 'observe'
   
#------------------------------------------------------------------------------#
# Creating the forward and backwards arrows for the SS figures   ---------------
#------------------------------------------------------------------------------#
# About: This section provides the functionality for the forward and back      #
# arrows for going through the skill scores figures                            #
#------------------------------------------------------------------------------#
   
   ###################################################################
   # Creating the reactive value to be used with the metrics buttons #
   ###################################################################
   current_index_skillScores <- reactiveVal(1)
   
   #################################################
   # Going backwards if the previous button is hit #
   #################################################
   observeEvent(input$PreviousSS, {
     
     # Isolating the action to only when the button is clicked
     isolate({
       
       # Running if the current index is greater than one
       if(current_index_skillScores() > 1){
         
         # Changing the index of the reactive value
         current_index_skillScores(max(current_index_skillScores() - 1))
         
       }
       
     }) # End of 'isolate' statement
     
   }) # End of 'observeEvent' statement
   
   ############################################
   # Going forwards if the next button is hit #
   ############################################
   observeEvent(input$NextSS, {
     
     # Isolating the action to only when the button is clicked
     isolate({
       
       # Run if the current index is less than the length of the list
       if (current_index_skillScores() < length(skillScoresFigs$figList)) {
         
         # Changing the index of the reactive value
         current_index_skillScores(min(current_index_skillScores() + 1))
         
       }
       
     }) # End of 'isolate' statement
     
   }) # End of 'observeEvent' statement   
   
#------------------------------------------------------------------------------#
# Resetting the index value  ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section resets the current index to one if the average metrics   #
# button is hit.                                                               #
#------------------------------------------------------------------------------#
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   output$SSFigureAGGP <- renderPlot({
     
     
     # Runs if no errors occur 
     tryCatch({
       
         # Running if working with average figure
         if(length(skillScoresFigs$figList) == 1 | is.na(skillScoresFigs$figList) | is.null(skillScoresFigs$figList) | length(skillScoresFigs$figList) == 0){
           
           return(skillScoresFigs$figList)
           
         }else{
           
           return(skillScoresFigs$figList[[current_index_skillScores()]])
         }
       
       }, error = function(e){
       
       NULL
         
     })
     
     }) # End of observe
  
  
  
  
  
   
# Reading in additional models -------------------------------------------------
   
#------------------------------------------------------------------------------#
# Data Formatting Pop-up Message -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a pop-up message that appears when a user clicks #
# on the "Model Comparison" page. It first makes sure that the users know to   #
# go to the instructions page to set up their data prior to running the page.  #
#------------------------------------------------------------------------------#
   
   #####################################################################
   # Pop-up to show when the "Model Comparison" page button in clicked #
   #####################################################################
   observeEvent(input$my_picker, {

     #################################################
     # Pop-up only if on the "Model Comparison" page #
     #################################################
     if(input$my_picker == "Model Comparison"){

       # Instructions for model comparisons page
       showModal(modalDialog(

         title = "Instructions",
         HTML("<p>Welcome to the Model Comparison page. This page allows users to
           create forecast figures and panels and compare performance metrics
           for models not included in the dashboard to the forecasts conducted
           within the dashboard.</p>

           <p>To use this feature, please ensure your data follows the correct
           setup. Instructions for the proper data format can be found on the
           `Instructions` tab of the dashboard.</p>

           <p>All options MUST be selected and forecasts run on the primary dashboard
           page before comparing models and reading in outside forecasts.</p>"
         ),

        easyClose = TRUE

      ))

    }

     })

#------------------------------------------------------------------------------#
# Reading in multiple files ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in multiple files unrelated to the benchmarking    #
# models. This is the background behind reading in the multiple files.         #
#------------------------------------------------------------------------------#
  
  ###################################################
  # Reactive value to store multiple forecast files #
  ###################################################
  listOtherForecasts <- reactiveValues()
  
  #######################################################
  # Running if the formatted forecast button is clicked #
  #######################################################
  observeEvent(input$dataset2,{
    
    ################################
    # Running if data is available #
    ################################
    tryCatch({
      
      # Empty list to store files 
      filesOtherLst <- list()
      
      ###################################
      # Isolating the change in 'files' #
      ###################################
      isolate({
      
        # List of files read in 
        files <- input$dataset2
        
      })
      
      ##############################
      # Looping through file names #
      ##############################
      for(i in 1:nrow(files)){
        
        # Indexed fie 
        indexedFile <- files$datapath[i]
        
        # Indexed file name
        indexedFileName <- files$name[i]
        
        # Extract the extension of the file
        ext <- tools::file_ext(indexedFileName)
        
        ######################################################
        # Produces an error if a '.csv' file is not selected #
        ######################################################
        if (ext != "csv") {
          
          # Produced error
          showModal(modalDialog(
            title = "Error",
            paste("File", indexedFileName, "is not a '.csv' file. Please upload only '.csv' files."),
            easyClose = TRUE
          ))
          
          return(NULL)
          
        }
        
        # Reading in the data
        data <- read.csv(indexedFile, header = T)
        
        # Saving the files to a list 
        filesOtherLst[[i]] <- data
        
        # Adding the name of the file
        names(filesOtherLst)[i] <- indexedFileName
        
      } # End of file loop 
      
      ##########################################################
      # Saving the list of files under the reactive value name #
      ##########################################################
      listOtherForecasts$forecastData <- filesOtherLst
      
    ####################################
    # Running if data is not available #
    ####################################
    }, error = function(e){
      
      print("The other forematted forecast data did not load properly.")
      
    })
    
    })
   

#------------------------------------------------------------------------------#
# Resetting the reactive value with files --------------------------------------
#------------------------------------------------------------------------------#
# About: This section resets the reactive value containing the loaded files if #
# the main data set is changed.                                                #
#------------------------------------------------------------------------------#
  
  #######################################################################
  # Observing the change in the reactive value holding the orignal data #
  #######################################################################
  observeEvent(file(), {
    
     # Reset the reactive value to NULL when file() changes
     listOtherForecasts$forecastData <- NULL
     
   })
   
   
#------------------------------------------------------------------------------#
# Button to edit individual forecast figures -----------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the legend labels for the    #
# individual figures containing multiple types of forecasts.                   #
#------------------------------------------------------------------------------#
   
   ############################################
   # Reactive value for the y-axis scale type #
   ############################################
   scaleYOther <- reactiveValues(logScale = "Original") 
   
   #######################################
   # Reactive value for the y-axis label #
   #######################################
   yAxisLabOther <- reactiveValues(lab = "Count")
   
   ################################################
   # Reactive value for the number of date breaks #
   ################################################
   dateBreaksReactiveOther <- reactiveValues(breaksDate = "1")
   
   #############################################
   # Reactive value for starting at the y-axis #
   #############################################
   startYReactiveOther <- reactiveValues(check = "0")
   
   ###############################
   # Reactive value for dot size #
   ###############################
   dotSizeReactiveOther <- reactiveValues(sizeVal = 2)
   
   ##############################
   # Creating the button pop-up #
   ##############################
   observeEvent(input$editLegendLabelsOther, {
     
     showModal(modalDialog(
       title = "Figure Options",
       h3("Y-Axis Options", style = "font-size: 15px;"), 
       pickerInput("logScaleOther", "Scale for Y-Axis:", c("Original", "Log(Base 10)"), selected = scaleYOther$logScale), # Scale of y-axis
       pickerInput("zeroStart", "Y-Axis Origin:", c("0", "Minimum value in data"), selected = startYReactiveOther$check), # Starting of the y-axis 
       textInput("yaxisTimeSeriesOther", "Label for Y-Axis:", value = yAxisLabOther$lab), # Label for y-axis
       h3("X-Axis Options", style = "font-size: 15px;"),
       textInput("dateBreaksTSOther", "Number of Date Breaks (Label):", value = dateBreaksReactiveOther$breaksDate), # Number of date breaks  
       h3("Other Options", style = "font-size: 15px;"), 
       numericInput("dotSizeOther", "Size of the obs. data points:", value = dotSizeReactiveOther$sizeVal, step = 0.01) # Data dot size option
     ))
     
   })
   
   ###############################################
   # Update the reactive value - scale of y-axis #
   ###############################################
   observeEvent(input$logScaleOther, {
     
     # Updating the scale
     scaleYOther$logScale <- input$logScaleOther
     
   })
   
   ###############################################################
   # Update the reactive value - Y-axis for the timeseries label #
   ###############################################################
   observeEvent(input$yaxisTimeSeriesOther, {
     
     # Updating the y-axis label
     yAxisLabOther$lab <- input$yaxisTimeSeriesOther
     
   })
   
   #####################################################
   # Update the reactive value - Number of date breaks #
   #####################################################
   observeEvent(input$dateBreaksTSOther,{
     
     # Updating the number of date breaks
     dateBreaksReactiveOther$breaksDate <- input$dateBreaksTSOther
     
   })
   
   ###############################################
   # Update the reactive value - Start at Y axis #
   ###############################################
   observeEvent(input$zeroStart,{
     
     # Updating the start point for the y-axis
     startYReactiveOther$check <- input$zeroStart
     
   })
   
   #############################################
   # Update the reactive value - data dot size #
   #############################################
   observeEvent(input$dotSizeOther,{
     
     # Updating the data point size
     dotSizeReactiveOther$sizeVal <- input$dotSizeOther
     
   })
   
   
#------------------------------------------------------------------------------#
# Creating the individual forecast figures -------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the individual forecast figures for other models #
# read into the dashboard.                                                     #
#------------------------------------------------------------------------------#
   
   ####################################################
   # Creating the reactive value to save the plots in #
   ####################################################
   individualOtherPlots <- reactiveValues()
   
   ###########################################
   # Observing changes in reactive behaviors #
   ###########################################
   observe({
    
     ############################# 
     # Runs if data is available #
     #############################
     tryCatch({
       
       ##########################################
       # Locations included in the orignal data #
       ##########################################
       dataLocations <- colnames(file())[-1]
       
       ######################################
       # List of triggering reactive values #
       ######################################
       indPlotsTrigger <- reactive({
         
         # Triggering events 
         allEvents <- list(input$dataset2,
                           scaleYOther$logScale,
                           yAxisLabOther$lab,
                           dateBreaksReactiveOther$breaksDate,
                           startYReactiveOther$check,
                           dotSizeReactiveOther$sizeVal)
         
         # Returning the list of reative values 
         return(allEvents)
         
       })
       
       #######################################################
       # Function to produce the individual forecast figures #
       #######################################################
       observeEvent(indPlotsTrigger(), ignoreNULL = T, {
         
         ##################################
         # Running only if data is loaded #
         ##################################
         if(!is.null(listOtherForecasts$forecastData)){
           
           # Isolating to when one of the buttons is clicked 
           isolate({
             
             ##########################################
             # Function to produce individual figures #
             ##########################################
             otherIndividual <- Otherforecast.figures(OTHERformattedForecastInput = listOtherForecasts$forecastData, # Data files 
                                                      date.type.input = dateValues$dates, # Date type 
                                                      scaleYAxis.input = scaleYOther$logScale, # Scale y-axis
                                                      yAxisLabel.input = yAxisLabOther$lab, # Y-axis label
                                                      dateBreaks.input = dateBreaksReactiveOther$breaksDate, # Date breaks
                                                      startYPoint.input = startYReactiveOther$check, # Y-axis start point
                                                      dotSize.input = dotSizeReactiveOther$sizeVal, # Size of dots
                                                      locations.input = dataLocations, # Location in original data
                                                      orignalData.input = foremattedForecasts$forecasts) # Formatted forecast files
             
             }) # End of isolate 
           
           ###################################################################
           # Updating the reactive value when the inputted files are changed #
           ###################################################################
           individualOtherPlots$figures <- otherIndividual
           
           }
         
         })
       
     #################################
     # Runs if data is NOT available #
     #################################
     }, error = function(e){
       
       NULL
       
     })
     
   })
         

#------------------------------------------------------------------------------#
# Returning the respective errors ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns specific errors based upon the results of the    #
# above function.                                                              #
#------------------------------------------------------------------------------#
  
  #################################################################
  # Observing changes in the reactive value containing the panels #
  #################################################################
  observe({
    
    #############################
    # Runs if data is available #
    #############################
    tryCatch({
      
      ##################################################################
      # Error to return if the files are loaded prior to the dashboard #
      ##################################################################
      if(individualOtherPlots$figures == "ERROR1"){
        
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "Please run the main dashboard prior to loading files.")
        
        # Clearing reactive values holding individual figures 
        individualOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
        
      #######################################################
      # Error to return if the column names are not correct #
      #######################################################
      }else if(individualOtherPlots$figures == "ERROR2"){
        
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "Please check the names of the columns within loaded data. The should be in the following order: date, data, median, LB, UB")
        
        # Clearing reactive values holding individual figures 
        individualOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
       
      ########################################################
      # Error to return if the name of the file is incorrect #
      ########################################################
      }else if(individualOtherPlots$figures == "ERROR3"){
        
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "Please check the naming scheme of your data.")
        
        # Clearing reactive values holding individual figures 
        individualOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
        
      ###################################################
      # Error to return if there is an issue with dates #
      ###################################################
      }else if(individualOtherPlots$figures == "ERROR4"){
       
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "The date type you indicated in the file name does not match the data type included in the dashboard nor the forecast file. Please check your date specification (Year = YYYY, Day/Week = MM-DD-YYYY)")
        
        # Clearing reactive values holding individual figures 
        individualOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
        
      ##########################################################
      # Error to return if there is an issue with the location #
      ##########################################################
      }else if(individualOtherPlots$figures == "ERROR5"){
        
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "Please check your file names. The location listed does not match any location loaded with the orignal data.")
        
        # Clearing reactive values holding individual figures 
        individualOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
        
      }
    
    #################################
    # Runs if data is not available #
    #################################
    }, error = function(e){
      
      NULL
      
    })
    
  })
       
#------------------------------------------------------------------------------#
# Creating the forward and backwards arrows for the other figures   ------------
#------------------------------------------------------------------------------#
# About: This section provides the functionality for the forward and back      #
# arrows for going through the other model forecasts.                          #
#------------------------------------------------------------------------------#
  
  ###################################################################
  # Creating the reactive value to be used with the metrics buttons #
  ###################################################################
  current_index_otherModels <- reactiveVal(1)
  
  #################################################
  # Going backwards if the previous button is hit #
  #################################################
  observeEvent(input$otherFigsPrevious, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Running if the current index is greater than one
      if(current_index_otherModels() > 1){
        
        # Changing the index of the reactive value
        current_index_otherModels(max(current_index_otherModels() - 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement
  
  ############################################
  # Going forwards if the next button is hit #
  ############################################
  observeEvent(input$otherFigstNext, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Run if the current index is less than the length of the list
      if (current_index_otherModels() < length(individualOtherPlots$figures)) {
        
        # Changing the index of the reactive value
        current_index_otherModels(min(current_index_otherModels() + 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement   

#------------------------------------------------------------------------------#
# Rendering the individual forecast --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the information from above to create individual     #
# forecast figures for each other model read into the dashboard.               #
#------------------------------------------------------------------------------#
  
 ###############################
 # Rendering the plotly figure #
 ###############################
  
  #############################
  # Runs if data is available #
  #############################
  tryCatch({
  
    # Producing the figures 
    output$otherModelFigure <- renderPlotly({ggplotly(individualOtherPlots$figures[[current_index_otherModels()]], tooltip = "text")})
    
    ##########################################
    # Clearing the figure if an error occurs #
    ##########################################
    }, error = function(e){
      
      # Clearing the output
      output$otherModelFigure <- NULL
      
  })
  
#------------------------------------------------------------------------------#
# Creating the title for other individual forecasts ----------------------------
#------------------------------------------------------------------------------#
# About: Creating the title for the other individual forecast figures.         #
#------------------------------------------------------------------------------#
  
  #######################
  # Rendering the title #
  #######################
  output$OtherForecastTitle <- renderText({
    
    ###########################
    # Runs if data is entered #
    ###########################
    tryCatch({
      
      # Rendering the title
     return(names(individualOtherPlots$figures[current_index_otherModels()]))
      
    ##############################
    # Runs if no data is entered #
    ##############################
    }, error = function(e){
      
      # Returning a NULL
      NULL
      
    }) # End of 'tryCatch'
    
  })
  
#------------------------------------------------------------------------------#
# Resetting the reactive value with the individual figures ---------------------
#------------------------------------------------------------------------------#
# About: This section resets the reactive value containing the formatted       #
# forecast figures (individual) when the orignal data is changed.              #
#------------------------------------------------------------------------------#
  
  #######################################################################
  # Observing the change in the reactive value holding the orignal data #
  #######################################################################
  observeEvent(file(), {
    
    # Reset the reactive value to NULL when file() changes
    individualOtherPlots$figures <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Creating the download handler for the individual figures ---------------------
#------------------------------------------------------------------------------#
# About: This section enables the downloading of the forecast figures to the   #
# folder of the user's choosing.                                               #
#------------------------------------------------------------------------------#

  #################################################
  # Message that pops up with downloading options #
  #################################################
  
  #################################
  # Setting Figure Specifications #
  #################################
  observeEvent(input$downloadOtherForecastsFigs, {
    
    ################################
    # Figure specification options #
    ################################
    isolate({
      
      showModal(modalDialog(
        
        title = "Figure Specifications",
        numericInput("dpi", "Figure DPI:", value = 900),
        numericInput("width", "Figure Width:", value = 9),
        numericInput('height', 'Figure Height:', value = 5),
        pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
        pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
        downloadButton("downloadOtherFigsIndividual", "Download Forecast Figures"),
        easyClose = TRUE
        
      )) 
    })
    
  })
  
  ##########################
  # Downloading the images #
  ##########################
  output$downloadOtherFigsIndividual <- downloadHandler(
    
    ####################
    # Filename for ZIP #
    ####################
    filename = function(){
      
      paste("other-individual-forecast-figures.zip", sep = "")
      
    },
    
    ############################################
    # Determining what should be in the folder #
    ############################################
    content = function(file){
      
      # Removing the message
      removeModal()
      
      # Creating a temp directory for files 
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      
      # Physically creating the directory 
      dir.create(temp_directory)
      
      # Saving the ggplots 
      for (plot_name in names(individualOtherPlots$figures)) {
        
        # Plot 
        plot_obj <- individualOtherPlots$figures[[plot_name]]
        
        # If plot is found 
        if (!is.null(plot_obj)) {
          
          # File name 
          file_name <- glue("{plot_name}.{input$extFig}")
          
          # TIFF file 
          if(input$extFig == "tiff"){
            
            ggsave(
              file.path(temp_directory, file_name),
              plot = plot_obj,
              dpi = input$dpi,
              width = input$width,
              height = input$height,
              units = input$units,
              device = input$extFig,
              compression = "lzw")
            
          }else{
            
            # All other image types
            ggsave(
              file.path(temp_directory, file_name),
              plot = plot_obj,
              dpi = input$dpi,
              width = input$width,
              height = input$height,
              units = input$units,
              device = input$extFig)
            
          }
          
        }
        
      }
      
      #####################
      # Create a zip file #
      #####################
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
    },
    contentType = "application/zip"
  )
  
  
#------------------------------------------------------------------------------#
# Button to edit forecast figures ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the legend labels for the    #
# panel figure containing multiple types of forecasts.                         #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYOtherPanel <- reactiveValues(logScale = "Original") 
  
  #######################################
  # Reactive value for the y-axis label #
  #######################################
  yAxisLabOtherPanel <- reactiveValues(lab = "Count")
  
  ################################################
  # Reactive value for the number of date breaks #
  ################################################
  dateBreaksReactiveOtherPanel <- reactiveValues(breaksDate = "1")
  
  #############################################
  # Reactive value for starting at the y-axis #
  #############################################
  startYReactiveOtherPanel <- reactiveValues(check = "0")
  
  ###############################
  # Reactive value for dot size #
  ###############################
  dotSizeReactiveOtherPanel <- reactiveValues(sizeVal = 2)
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editLegendLabelsOtherPanel, {
    
    showModal(modalDialog(
      title = "Figure Options",
      h3("Y-Axis Options", style = "font-size: 15px;"), 
      pickerInput("logScaleOtherPanel", "Scale for Y-Axis:", c("Original", "Log(Base 10)"), selected = scaleYOtherPanel$logScale), # Scale of y-axis
      pickerInput("zeroStartPanel", "Y-Axis Origin:", c("0", "Minimum value in data"), selected = startYReactiveOtherPanel$check), # Starting of the y-axis 
      textInput("yaxisTimeSeriesOtherPanel", "Label for Y-Axis:", value = yAxisLabOtherPanel$lab), # Label for y-axis
      h3("X-Axis Options", style = "font-size: 15px;"),
      textInput("dateBreaksTSOtherPanel", "Number of Date Breaks (Label):", value = dateBreaksReactiveOtherPanel$breaksDate), # Number of date breaks  
      h3("Other Options", style = "font-size: 15px;"), 
      numericInput("dotSizeOtherPanel", "Size of the obs. data points:", value = dotSizeReactiveOtherPanel$sizeVal, step = 0.01) # Data dot size option
      ))
    
  })
  
  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleOtherPanel, {
    
    # Updating the scale
    scaleYOtherPanel$logScale <- input$logScaleOtherPanel
    
  })
  
  ###############################################################
  # Update the reactive value - Y-axis for the timeseries label #
  ###############################################################
  observeEvent(input$yaxisTimeSeriesOtherPanel, {
    
    # Updating the y-axis label
    yAxisLabOtherPanel$lab <- input$yaxisTimeSeriesOtherPanel
    
  })
  
  #####################################################
  # Update the reactive value - Number of date breaks #
  #####################################################
  observeEvent(input$dateBreaksTSOtherPanel,{
    
    # Updating the number of date breaks
    dateBreaksReactiveOtherPanel$breaksDate <- input$dateBreaksTSOtherPanel
    
  })
  
  ###############################################
  # Update the reactive value - Start at Y axis #
  ###############################################
  observeEvent(input$zeroStartPanel,{
    
    # Updating the start point for the y-axis
    startYReactiveOtherPanel$check <- input$zeroStartPanel
    
  })
  
  #############################################
  # Update the reactive value - data dot size #
  #############################################
  observeEvent(input$dotSizeOtherPanel,{
    
    # Updating the data point size
    dotSizeReactiveOtherPanel$sizeVal <- input$dotSizeOtherPanel
    
  })
  

#------------------------------------------------------------------------------#
# Creating the panel forecast figures -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the panel forecast figures for other models      #
# read into the dashboard.                                                     #
#------------------------------------------------------------------------------#
  
  ####################################################
  # Creating the reactive value to save the plots in #
  ####################################################
  PanelOtherPlots <- reactiveValues()
  
  ###########################################
  # Observing changes in reactive behaviors #
  ###########################################
  observe({
    
    ############################# 
    # Runs if data is available #
    #############################
    tryCatch({
      
      ###########################################
      # Locations included in the original data #
      ###########################################
      dataLocations <- colnames(file())[-1]
      
      ######################################
      # List of triggering reactive values #
      ######################################
      PANELPlotsTrigger <- reactive({
        
        # Triggering events 
        allEvents <- list(input$dataset2,
                          scaleYOtherPanel$logScale,
                          yAxisLabOtherPanel$lab,
                          dateBreaksReactiveOtherPanel$breaksDate,
                          startYReactiveOtherPanel$check,
                          dotSizeReactiveOtherPanel$sizeVal)
        
        # Returning the list of reactive values 
        return(allEvents)
        
      })
      
      #######################################################
      # Function to produce the individual forecast figures #
      #######################################################
      observeEvent(PANELPlotsTrigger(), ignoreNULL = T, {
        
        ##################################
        # Running only if data is loaded #
        ##################################
        if(all(!is.null(listOtherForecasts$forecastData))){
          
          # Isolating to when one of the buttons is clicked 
          isolate({
            
            #######################################################
            # Function to produce the individual forecast figures #
            #######################################################
            OtherpanelOutput <- other.panel.forecast.figures(formatted.forecast.input = foremattedForecasts$forecasts, # Formatted forecast data
                                                             date.type.input = dateValues$dates, # Date type
                                                             formatted.forecast.Other.input = listOtherForecasts$forecastData, # Read in forecasts
                                                             yAxisScale.input = scaleYOtherPanel$logScale, # Scale for Y-Axis
                                                             yAxisLabel.input = yAxisLabOtherPanel$lab, # Y axis label 
                                                             dateBreaks.input = dateBreaksReactiveOtherPanel$breaksDate, # Date breaks 
                                                             startY.input = startYReactiveOtherPanel$check, # Start of Y axis 
                                                             dataDot.input = dotSizeReactiveOtherPanel$sizeVal, # Size of data dot 
                                                             errorGLM.input = input$errorTermGLM, # Error input GLM
                                                             location.input = dataLocations) # Locations 
            
            # Saving the output to the reactive value list
            PanelOtherPlots$figures <- OtherpanelOutput
            
          }) # End of isolate
          
        }
        
      }) # End of 'observeEvent'
      
    }, error = function(e){
      
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'observe'
  
  
#------------------------------------------------------------------------------#
# Returning the respective errors ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns specific errors based upon the results of the    #
# above function.                                                              #
#------------------------------------------------------------------------------#
  
  #################################################################
  # Observing changes in the reactive value containing the panels #
  #################################################################
  observe({
    
    #############################
    # Runs if data is available #
    #############################
    tryCatch({
      
      ##################################################################
      # Error to return if the files are loaded prior to the dashboard #
      ##################################################################
      if(PanelOtherPlots$figures == "ERROR1"){
        
        # Clearing reactive values holding panel figures 
        PanelOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
        
      #######################################################
      # Error to return if the column names are not correct #
      #######################################################
      }else if(PanelOtherPlots$figures == "ERROR2"){
        
        # Clearing reactive values holding panel figures 
        PanelOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
        
      ########################################################
      # Error to return if the name of the file is incorrect #
      ########################################################
      }else if(individualOtherPlots$figures == "ERROR3"){
        
        # Clearing reactive values holding panel figures 
        PanelOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
        
      ###################################################
      # Error to return if there is an issue with dates #
      ###################################################
      }else if(individualOtherPlots$figures == "ERROR4"){
      
        # Clearing reactive values holding panel figures 
        PanelOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
        
      ##########################################################
      # Error to return if there is an issue with the location #
      ##########################################################
      }else if(individualOtherPlots$figures == "ERROR5"){
        
        # Clearing reactive values holding panel figures 
        PanelOtherPlots$figures <- NULL
        
        # Clearing reactive value containing other formatted forecast data 
        listOtherForecasts$forecastData <- NULL
        
      }
      
    #################################
    # Runs if data is not available #
    #################################
    }, error = function(e){
      
      NULL
      
    })
    
  })  


#------------------------------------------------------------------------------#
# Creating the previous and next arrows for the other panel figures ------------
#------------------------------------------------------------------------------#
# About: This section creates the reactive value, and previous and next arrow  #
# buttons for the panel other model figures. It then gives functionality to    #
# the buttons as its related to going through the list of figures.             #
#------------------------------------------------------------------------------#
  
  #######################################################################
  # Creating the reactive value to be used with the other pabel buttons #
  #######################################################################
  current_index_Panels <- reactiveVal(1)
  
  #################################################
  # Going backwards if the previous button is hit #
  #################################################
  observeEvent(input$otherFigsPanelsPrevious, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Running if the current index is greater than one
      if(current_index_Panels() > 1){
        
        # Changing the index of the reactive value
        current_index_Panels(max(current_index_Panels() - 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement
  
  ############################################
  # Going forwards if the next button is hit #
  ############################################
  observeEvent(input$otherFigsPanelsNext, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Run if the current index is less than the length of the list
      if (current_index_Panels() < length(PanelOtherPlots$figures)) {
        
        # Changing the index of the reactive value
        current_index_Panels(min(current_index_Panels() + 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement   
  
#------------------------------------------------------------------------------#
# Rendering the all models plot figures ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the other figure that correspond to the panels   #
# of figures of both read-in and ARIMA/GLM/GAM/Prophet/Other Models figures.   #
#------------------------------------------------------------------------------#
  
  ########################
  # Rendering the figure #
  ########################
  
  #############################
  # Try to produce the figure #
  #############################
  tryCatch({

    # Producing the panel plot
    output$otherModelPanelFigure <- renderPlot({
        
      PanelOtherPlots$figures[[current_index_Panels()]]
      
      })
    
    ########################
    # Runs if error occurs #
    ########################
    }, error = function(e){
    
      output$otherModelPanelFigure <- NULL
      
    })
  
#------------------------------------------------------------------------------#
# Creating the title for panel -------------------------------------------------
#------------------------------------------------------------------------------#
# About: Creating the title for the panel figures.                             #
#------------------------------------------------------------------------------#
  
  #######################
  # Rendering the title #
  #######################
  output$OtherPanelForecastTitle <- renderText({
    
    ###########################
    # Runs if data is entered #
    ###########################
    tryCatch({
      
      # Rendering the title
      return(names(PanelOtherPlots$figures[current_index_Panels()]))
        
    ##############################
    # Runs if no data is entered #
    ##############################
    }, error = function(e){
      
      # Returning a NULL
      NULL
      
    }) # End of 'tryCatch'
    
  })
  
#------------------------------------------------------------------------------#
# Resetting the reactive value with the panel figures --------------------------
#------------------------------------------------------------------------------#
# About: This section resets the reactive value containing the formatted       #
# forecast figures (panel) when the original data is changed.                  #
#------------------------------------------------------------------------------#
  
  #######################################################################
  # Observing the change in the reactive value holding the orignal data #
  #######################################################################
  observeEvent(file(), {
    
    # Reset the reactive value to NULL when file() changes
    PanelOtherPlots$figures <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Creating the download handler for the panel figures --------------------------
#------------------------------------------------------------------------------#
# About: This section enables the downloading of the forecast figures to the   #
# folder of the user's choosing.                                               #
#------------------------------------------------------------------------------#
  
  #################################################
  # Message that pops up with downloading options #
  #################################################
  
  #################################
  # Setting Figure Specifications #
  #################################
  observeEvent(input$downloadOtherForecastsPanels, {
    
    ################################
    # Figure specification options #
    ################################
    isolate({
      
      showModal(modalDialog(
        
        title = "Figure Specifications",
        numericInput("dpi", "Figure DPI:", value = 900),
        numericInput("width", "Figure Width:", value = 9),
        numericInput('height', 'Figure Height:', value = 5),
        pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
        pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
        downloadButton("downloadOtherFigsPanels", "Download Forecast Figures"),
        easyClose = TRUE
        
      )) 
      
    })
    
  })  
  
  ##########################
  # Downloading the images #
  ##########################
  output$downloadOtherFigsPanels <- downloadHandler(
    
    ####################
    # Filename for ZIP #
    ####################
    filename = function(){
      
      paste("other-panel-forecast-figures.zip", sep = "")
      
    },
    
    ############################################
    # Determining what should be in the folder #
    ############################################
    content = function(file){
      
      # Removing the message
      removeModal()
      
      # Creating a temp directory for files 
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      
      # Physically creating the directory 
      dir.create(temp_directory)
      
      # Saving the ggplots 
      for (plot_name in names(PanelOtherPlots$figures)) {
        
        # Plot 
        plot_obj <- PanelOtherPlots$figures[[plot_name]]
        
        # If plot is found 
        if (!is.null(plot_obj)) {
          
          # File name 
          file_name <- glue("{plot_name}.{input$extFig}")
          
          # TIFF file 
          if(input$extFig == "tiff"){
            ggsave(
              file.path(temp_directory, file_name),
              plot = plot_obj,
              dpi = input$dpi,
              width = input$width,
              height = input$height,
              units = input$units,
              device = input$extFig,
              compression = "lzw")
            
          }else{
            
            # All other image types
            ggsave(
              file.path(temp_directory, file_name),
              plot = plot_obj,
              dpi = input$dpi,
              width = input$width,
              height = input$height,
              units = input$units,
              device = input$extFig)
            
          }
          
        }
        
      }
      
      #####################
      # Create a zip file #
      #####################
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
    },
    
    contentType = "application/zip"
    
  )
  
  
#------------------------------------------------------------------------------#
# Handling crude metrics (Data) ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows users to read in the crude metrics of their       #
# interest, combines them with the metrics calculated for the ARIMA, GLM, GAM, #
# and Prophet models, and exports a data set with the crude metrics. Figures   #
# will be created at a later step.                                             #
#------------------------------------------------------------------------------#
  
#------------------------------------------------------------------------------#
# Reading in the crude metrics for the other files -----------------------------
#------------------------------------------------------------------------------#
# About: This section provides the re-activity to the download button located  #
# in the side-bar panel. It allows the user to read in multiple files, pending #
# the files are names correctly.                                               #
#------------------------------------------------------------------------------#
  
  #################################################
  # Reactive value to store multiple metric files #
  #################################################
  metricsOtherReactive <- reactiveValues()
  
  ##################################################
  # Running if the other metrics button is clicked #
  ##################################################
  observeEvent(input$metricsOther,{
    
    ################################
    # Running if data is available #
    ################################
    tryCatch({
      
      # Empty list to store files 
      filesOtherLst <- list()
      
      ###################################
      # Isolating the change in 'files' #
      ###################################
      isolate({
        
        # List of files read in 
        files <- input$metricsOther
        
      })
      
      ##############################
      # Looping through file names #
      ##############################
      for(i in 1:nrow(files)){
        
        # Indexed fie 
        indexedFile <- files$datapath[i]
        
        # Indexed file name
        indexedFileName <- files$name[i]
        
        # Extract the extension of the file
        ext <- tools::file_ext(indexedFileName)
        
        ######################################################
        # Produces an error if a '.csv' file is not selected #
        ######################################################
        if (ext != "csv") {
          
          # Produced error
          showModal(modalDialog(
            title = "Error",
            paste("File", indexedFileName, "is not a '.csv' file. Please upload only '.csv' files."),
            easyClose = TRUE
          ))
          
          return(NULL)
          
        }
        
        # Reading in the data
        data <- read.csv(indexedFile, header = T)
        
        # Saving the files to a list 
        filesOtherLst[[i]] <- data
        
        # Adding the name of the file
        names(filesOtherLst)[i] <- indexedFileName
        
      } # End of file loop 
      
      ##########################################################
      # Saving the list of files under the reactive value name #
      ##########################################################
      metricsOtherReactive$metricData <- filesOtherLst
      
    ####################################
    # Running if data is not available #
    ####################################
    }, error = function(e){
      
      print("The other metric data did not load properly.")
      
    })
    
  })
  
#------------------------------------------------------------------------------#
# Resetting the reactive value with files --------------------------------------
#------------------------------------------------------------------------------#
# About: This section resets the reactive value containing the loaded files if #
# the main data set is changed.                                                #
#------------------------------------------------------------------------------#
  
  #######################################################################
  # Observing the change in the reactive value holding the orignal data #
  #######################################################################
  observeEvent(file(), {
    
    # Reset the reactive value to NULL when file() changes
    metricsOtherReactive$metricData <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Combining crude metrics ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows users to combine the crude metrics they have read #
# into the dashboard with the produced ARIMA, GLM, GAM, Prophet results. The   #
# files are matched on if they are fit or forecast statistics, horizon,        #
# calibration, forecast period, and location/group. Additionally, the data     #
# will be structured where each column is a metrics, and the first column is   #
# the forecast date. If there are additionally statistics outside those        #
# produced in the toolbox, they will have their own column.                    #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reactive value to save the crude metrics #
  ############################################
  crudeMetricsDataOther <- reactiveValues()
  
  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({
    
    ################################
    # Running if data is available #
    ################################
    tryCatch({
      
      ##########################################
      # Locations included in the original data #
      ##########################################
      dataLocations <- colnames(file())[-1]
      
      #######################################################
      # Function to produce the individual forecast figures #
      #######################################################
      observeEvent(input$metricsOther, ignoreNULL = T, {
        
        ##################################
        # Running only if data is loaded #
        ##################################
        if(!is.null(metricsOtherReactive$metricData)){
          
          # Isolating to when one of the buttons is clicked 
          isolate({
            
            ##################################
            # Running only if data is loaded #
            ##################################
            metricsCombined <- combiningAllMetrics(otherMetrics.input = metricsOtherReactive$metricData, # Other metrics
                                                   modelFit.input = modelFitMetricsList$fitMetrics, # Dashboard fit metrics
                                                   modelForecast.input = forecastMetricsListCrude$forecastMetrics, # Dashboard forecast metrics
                                                   horizon.input = input$forecastHorizon, # Forecasting horizon
                                                   calibration.input = input$calibrationPeriod, # Calibration period metrics 
                                                   locations.input = dataLocations, # Original locations 
                                                   formattedForecastOrignal.input = foremattedForecasts$forecasts, # Formatted forecast data
                                                   date.type.input = dateValues$dates) # Date type
            
          }) # End of isolate
          
          ##########################################
          # Saving the metrics in a reactive value #
          ##########################################
          crudeMetricsDataOther$data <- metricsCombined
          
        }
        
      }) # End of 'observeEvent'
      
    #################################
    # Runs if data is not available #
    #################################
    }, error = function(e){
      
      # Returning NULL
      NULL
      
    }) # End of 'tryCaych'
    
  }) # End of 'observe'
  
          
#------------------------------------------------------------------------------#
# Returning the respective errors ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns specific errors based upon the results of the    #
# above function.                                                              #
#------------------------------------------------------------------------------#

  ############################################
  # Indicator to clear average metrics input #
  ############################################
  clearAverageMetricsOther <- reactiveVal(0)
  
  #################################################################
  # Observing changes in the reactive value containing the panels #
  #################################################################
  observe({
    
    #############################
    # Runs if data is available #
    #############################
    tryCatch({
      
      ##################################################################
      # Error to return if the files are loaded prior to the dashboard #
      ##################################################################
      if(crudeMetricsDataOther$data == "ERROR1"){
        
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "Please run the main dashboard prior to loading files.")
        
        # Clearing reactive values holding crude metrics 
        crudeMetricsDataOther$data <- NULL
        
        # Clearing reactive value containing the metrics data 
        metricsOtherReactive$metricData <- NULL
        
        # Clearing the reactive value containing the filtered data
        finalCrudeCombined$metricsFULL <- NULL
        
        # Setting the indicator to 1
        clearAverageMetricsOther(1)
        
      #######################################################
      # Error to return if the column names are not correct #
      #######################################################
      }else if(crudeMetricsDataOther$data == "ERROR2"){
        
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "Please check the names of the columns within loaded metric data. The first column should be Model, followed by the included metrics.")
        
        # Clearing reactive values holding crude metrics 
        crudeMetricsDataOther$data <- NULL
        
        # Clearing reactive value containing the metrics data 
        metricsOtherReactive$metricData <- NULL
        
        # Clearing the reactive value containing the filtered data
        finalCrudeCombined$metricsFULL <- NULL
        
        # Setting the indicator to 1
        clearAverageMetricsOther(1)
        
      ########################################################
      # Error to return if the name of the file is incorrect #
      ########################################################
      }else if(crudeMetricsDataOther$data == "ERROR3"){
        
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "Please check the naming scheme of your metrics files.")
        
        # Clearing reactive values holding crude metrics 
        crudeMetricsDataOther$data <- NULL
        
        # Clearing reactive value containing the metrics data 
        metricsOtherReactive$metricData <- NULL
        
        # Clearing the reactive value containing the filtered data
        finalCrudeCombined$metricsFULL <- NULL
        
        # Setting the indicator to 1
        clearAverageMetricsOther(1)
        
      ###################################################
      # Error to return if there is an issue with dates #
      ###################################################
      }else if(crudeMetricsDataOther$data == "ERROR4"){
        
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "The date type you indicated in the metric file name(s) does not match the data type included in the dashboard nor the metric file. Please check your date specification (Year = YYYY, Day/Week = MM-DD-YYYY)")
        
        # Clearing reactive values holding crude metrics 
        crudeMetricsDataOther$data <- NULL
        
        # Clearing reactive value containing the metrics data 
        metricsOtherReactive$metricData <- NULL
        
        # Clearing the reactive value containing the filtered data
        finalCrudeCombined$metricsFULL <- NULL
        
        # Setting the indicator to 1
        clearAverageMetricsOther(1)
        
      ##########################################################
      # Error to return if there is an issue with the location #
      ##########################################################
      }else if(crudeMetricsDataOther$data == "ERROR5"){
        
        # Alert to show
        shinyalert(title = "Error", type = "error", text = "Please check your metric file names. The location listed does not match any location loaded with the orignal data.")
        
        # Clearing reactive values holding crude metrics 
        crudeMetricsDataOther$data <- NULL
        
        # Clearing reactive value containing the metrics data 
        metricsOtherReactive$metricData <- NULL
        
        # Clearing the reactive value containing the filtered data
        finalCrudeCombined$metricsFULL <- NULL
        
        # Setting the indicator to 1
        clearAverageMetricsOther(1)
        
        
      }
      
    #################################
    # Runs if data is not available #
    #################################
    }, error = function(e){
      
      NULL
      
    })
    
  })
  
#------------------------------------------------------------------------------#
# Clearing output --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the crude metrics output if the orignal data is   #
# changed.                                                                     #
#------------------------------------------------------------------------------#
  
  ###############################
  # Looking for the data change #
  ###############################
  observeEvent(file(), {
    
    # Clearing the output
    crudeMetricsDataOther$data <- NULL
    
    # Setting the indicator to 0
    clearAverageMetricsOther(0)
    
  })
 
#------------------------------------------------------------------------------#
# Creating the filtering options for the crude metrics data --------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options for the crude metrics data #
# table. The filtering options are triggered when the 'Filtering Options'      #
# button is clicked.                                                           #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################
  
  # To store the filtered crude data 
  finalCrudeCombined <- reactiveValues()
  
  # Model choices 
  modelReactiveChoice <- reactiveValues()
  
  # Selected models 
  modelReactiveSelect <- reactiveValues()
  
  # Performance metric choice
  performanceReactiveChoice <- reactiveValues()
  
  # Selected performance metric choices 
  performanceReactiveSelect <- reactiveValues()
  
  # Location choices 
  locationReactiveChoice <- reactiveValues()
  
  # Selected locations 
  locationReactiveSelect <- reactiveValues()
  
  # Calibration length choices 
  calibrationReactiveChoice <- reactiveValues()
  
  # Selected calibration lengths
  calibrationReactiveSelect <- reactiveValues()
  
  # Horizon choices 
  horizonReactiveChoice <- reactiveValues()
  
  # Selected horizon 
  horizonReactiveSelect <- reactiveValues()
  
  # Indicator for which data to show 
  indicatorForFilterMetrics <- reactiveVal(0)
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    #####################################
    # Running if information is entered #
    #####################################
    tryCatch({
      
      # Running only if an error is not returned above
      if(all(crudeMetricsDataOther$data %!in% c("ERROR1", "ERROR2", "ERROR3",
                                            "ERROR4", "ERROR5", !is.null(crudeMetricsDataOther$data)))){
        
        # Crude metrics data
        crudeMetrics <- crudeMetricsDataOther$data
        
        #################
        # Model Options #
        #################
        
        # Choices
        modelReactiveChoice$data <- unique(crudeMetrics$Model)
        
        # Selected 
        modelReactiveSelect$data <- modelReactiveChoice$data
        
        ############################
        # Performance type metrics #
        ############################

        # Choices
        performanceReactiveChoice$data <- unique(crudeMetrics$`Performance Metric Type`)
        
        # Selected 
        performanceReactiveSelect$data <- performanceReactiveChoice$data
        
        ####################
        # Location Options #
        ####################
        
        # Choices
        locationReactiveChoice$data <- unique(crudeMetrics$Location)
        
        # Selected 
        locationReactiveSelect$data <- locationReactiveChoice$data
        
        #######################
        # Calibration Options #
        #######################
      
        # Choices
        calibrationReactiveChoice$data <- unique(crudeMetrics$Calibration)
        
        # Selected 
        calibrationReactiveSelect$data <- calibrationReactiveChoice$data
        
        ###################
        # Horizon Options #
        ###################
      
        # Choices
        horizonReactiveChoice$data <- unique(crudeMetrics$Horizon)
        
        # Selected 
        horizonReactiveSelect$data <- horizonReactiveChoice$data
        
        #######################
        # Creating the pop-up #
        #######################
        observeEvent(input$filterCrudeCombinedMetrics, ignoreInit = T,{
          
          # Changing the indicator to one (i.e., button has been clicked)
          isolate({indicatorForFilterMetrics(1)})
          
          # Isolating button click behavior 
          isolate({
            
            # Button 
            showModal(modalDialog(
              title = "Filtering Options",
              pickerInput("ModelCrudeData1", "Model:", c(modelReactiveChoice$data), selected = c(modelReactiveSelect$data), multiple = T), # Model filtering
              pickerInput("perfType1", "Performance Metric Type:", c(performanceReactiveChoice$data), selected = c(performanceReactiveSelect$data), multiple = T), # Metric Type
              pickerInput("locationChoicesCrude1", "Location:", c(locationReactiveChoice$data), selected = c(locationReactiveSelect$data), multiple = T), # Location
              pickerInput("calibrationChoices1", "Calibration:", c(calibrationReactiveChoice$data), selected = c(calibrationReactiveSelect$data), multiple = T), # Calibration 
              pickerInput("horizonChoices1", "Horizon:", c(horizonReactiveChoice$data), selected = c(horizonReactiveSelect$data), multiple = T) # Horizon
              
            ))
            
          })
          
        })
        
        #######################################################
        # Function to produce the individual forecast figures #
        #######################################################
        filteredMetrics  <- filterOtherMetrics(crudeMetrics.input = crudeMetricsDataOther$data, # Crude metrics
                                               averageMetrics.input = NULL, # Average metrics 
                                               crudeModel.input = input$ModelCrudeData1, # Crude model choices 
                                               crudePerformance.input = input$perfType1, # Crude performance metric type 
                                               crudeLocation.input = input$locationChoicesCrude1, # Crude location choice 
                                               crudeCalibration.input = input$calibrationChoices1, # Crude calibration choice 
                                               crudeHorizon.input = input$horizonChoices1, # Crude horizon choice
                                               AverageModel.input = NULL, # Average model choice 
                                               AveragePerformance.input = NULL, # Average performance metric type
                                               AverageLocation.input = NULL, # Average location choice
                                               AverageCalibration.input = NULL, # Average calibration choice
                                               AverageHorizon.input = NULL, # Average horizon choice
                                               inputindicator = indicatorForFilterMetrics()) # Indicator for data to show
        
        # Saving the results to the reactive value 
        finalCrudeCombined$metricsFULL <- filteredMetrics
        
      } # End of 'else'
      
    ####################################
    # Running if data is NOT available #
    ####################################
    }, error = function(e){
      
      NULL
    })
    
  })
          
#------------------------------------------------------------------------------#
# Rendering the data table with the crude data ---------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the data table with the crude data to the main   #
# dashboard.                                                                   #
#------------------------------------------------------------------------------#
  
  ############################
  # Rendering the data frame #
  ############################
  output$crudeMetricsOther <- renderDataTable({
    
    ###########################
    # Runs if no errors occur #
    ###########################
    tryCatch({
    
      # Returning the data frame
      return(datatable(finalCrudeCombined$metricsFULL))
        
    ###########################
    # Runs if an error occurs #
    ###########################
    }, error = function(e){
      
      # Returning a null 
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'renderDataTable'
  

#------------------------------------------------------------------------------#
# Clearing the filtered data set and indicator ---------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the final, filtered data and resets the indicator #
# for the data to show (i.e., full or filtered). It is triggered when the      #
# original data is changed.                                                    #
#------------------------------------------------------------------------------#

  #######################################################################
  # Observing the change in the reactive value holding the orignal data #
  #######################################################################
  observeEvent(file(), {
    
    if(is.null(foremattedForecasts$forecasts)){
      
      # Filtered data
      finalCrudeCombined$metricsFULL <- NULL
      
      # Resetting the indicator
      indicatorForFilterMetrics(0)
    
    }
    
  })
  
#------------------------------------------------------------------------------#
# Downloading the combined metrics data as a '.csv' ----------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the combined crude metrics as a '.csv' file to the  #
# directory of their choosing.                                                 #
#------------------------------------------------------------------------------#
  
  output$downloadCrudeMetrics <- downloadHandler(
    
    ####################################
    # Function to create the file-name #
    ####################################
    filename = function() {
      
      # File name 
      paste("combined-crude-metrics-", input$dataset, sep = "")
      
    },
    
    #############################
    # Function to save the file #
    #############################
    content = function(file) {
      
      # Saving the file
      write.csv(finalCrudeCombined$metricsFULL, file, row.names = FALSE)
      
    }
    
  ) # End of download button 
  
  
#------------------------------------------------------------------------------#
# Button to edit crude metrics figures -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the legend labels for the    #
# panel figures of crude metrics.                                              #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYOtherCrudeMetric <- reactiveValues(logScale = NULL) 
  
  ################################################
  # Reactive value for the number of date breaks #
  ################################################
  dateBreaksReactiveOtherCrudeMetric <- reactiveValues(breaksDate = "1")
  
  #############################################
  # Reactive value for starting at the y-axis #
  #############################################
  startYReactiveOtherCrudeMetric <- reactiveValues(check = "0")
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$figOptCombCrudeMetrics, {
    
    # Determining the metric choices 
    allMetricNames <- colnames(finalCrudeCombined$metricsFULL)
    
    # Creating the metric choice variable
    metricChoices <- allMetricNames[-c(1:6)]
    
    # Creating the pop-up
    showModal(modalDialog(
      title = "Figure Options",
      h3("Y-Axis Options", style = "font-size: 15px;"), 
      pickerInput("logScaleOtherCrudeMetric", "Metrics to Show in Log Base 10:", c("None", metricChoices), selected = scaleYOtherCrudeMetric$logScale, multiple = T),                                                                                                                                                                                                                             
      pickerInput("zeroStartOtherCrudeMetric", "Y-Axis Origin:", c("0", "Minimum value in data"), selected = startYReactiveOtherCrudeMetric$check), # Starting of the y-axis 
      h3("X-Axis Options", style = "font-size: 15px;"),
      textInput("dateBreaksTSOtherCrudeMetric", "Number of Date Breaks (Label):", value = dateBreaksReactiveOtherCrudeMetric$breaksDate) # Number of date breaks  
    ))
    
  })
  
  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleOtherCrudeMetric, {
    
      scaleYOtherCrudeMetric$logScale <- input$logScaleOtherCrudeMetric
    
  })
  
  #####################################################
  # Update the reactive value - Number of date breaks #
  #####################################################
  observeEvent(input$dateBreaksTSOtherCrudeMetric,{
    
    # Updating the number of date breaks
    dateBreaksReactiveOtherCrudeMetric$breaksDate <- input$dateBreaksTSOtherCrudeMetric
    
  })
  
  ###############################################
  # Update the reactive value - Start at Y axis #
  ###############################################
  observeEvent(input$zeroStartOtherCrudeMetric,{
    
    # Updating the start point for the y-axis
    startYReactiveOtherCrudeMetric$check <- input$zeroStartOtherCrudeMetric
    
  })
  
  
  
#------------------------------------------------------------------------------#
# Plotting the crude metrics ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the crude metrics based upon the entered files,    #
# the dashboard, and how the users filter the data.                            #
#------------------------------------------------------------------------------#
  
  ####################################################
  # Creating the reactive value to save the plots in #
  ####################################################
  CrudeMetricsOtherPlots <- reactiveValues()
  
  ###########################################
  # Observing changes in reactive behaviors #
  ###########################################
  observe({
    
    ############################# 
    # Runs if data is available #
    #############################
    tryCatch({
      
      ######################################
      # List of triggering reactive values #
      ######################################
      CrudeMetricsOtherPlotsTrigger <- reactive({
        
        # Triggering events 
        allEvents <- list(finalCrudeCombined$metricsFULL,
                          scaleYOtherCrudeMetric$logScale,
                          dateBreaksReactiveOtherCrudeMetric$breaksDate,
                          startYReactiveOtherCrudeMetric$check)
        
        # Returning the list of reactive values 
        return(allEvents)
        
      })
      
      #######################################################
      # Function to produce the panel crude metrics figures #
      #######################################################
      observeEvent(CrudeMetricsOtherPlotsTrigger(), ignoreNULL = T, {
        
        ##################################
        # Running only if data is loaded #
        ##################################
        if(all(!is.null(finalCrudeCombined$metricsFULL))){
          
          # Isolating to when one of the buttons is clicked 
          isolate({
            
            #######################################################
            # Function to produce the individual forecast figures #
            #######################################################
            crudeMetricOtherpanelOutput <- crudeMetricsFigureOther(crudedataforfigure.input = finalCrudeCombined$metricsFULL, 
                                                                    date.Type.input = dateValues$dates,
                                                                    date.break.input = dateBreaksReactiveOtherCrudeMetric$breaksDate, 
                                                                    scale.y.input = scaleYOtherCrudeMetric$logScale,  
                                                                    start.y.input = startYReactiveOtherCrudeMetric$check)
                                         
            
            # Saving the output to the reactive value list
            CrudeMetricsOtherPlots$figures <- crudeMetricOtherpanelOutput
            
          }) # End of isolate
          
        }
        
      }) # End of 'observeEvent'
      
    }, error = function(e){
      
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'observe'
  
#------------------------------------------------------------------------------#
# Creating the previous and next arrows for the other panel figures ------------
#------------------------------------------------------------------------------#
# About: This section creates the reactive value, and previous and next arrow  #
# buttons for the panel other crude metric figures. It then gives              #
# functionality to the buttons as its related to going through the list of     #
# figures.                                                                     #
#------------------------------------------------------------------------------#
  
  #######################################################################
  # Creating the reactive value to be used with the other panel buttons #
  #######################################################################
  current_index_crudeMetricPanel <- reactiveVal(1)
  
  #################################################
  # Going backwards if the previous button is hit #
  #################################################
  observeEvent(input$otherCrudeMetricPanelsPrevious, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Running if the current index is greater than one
      if(current_index_crudeMetricPanel() > 1){
        
        # Changing the index of the reactive value
        current_index_crudeMetricPanel(max(current_index_crudeMetricPanel() - 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement
  
  ############################################
  # Going forwards if the next button is hit #
  ############################################
  observeEvent(input$otherCrudeMetricPanelsNext, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Run if the current index is less than the length of the list
      if (current_index_crudeMetricPanel() < length(CrudeMetricsOtherPlots$figures)) {
        
        # Changing the index of the reactive value
        current_index_crudeMetricPanel(min(current_index_crudeMetricPanel() + 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement 
  
  ########################################################
  # Resetting the index if the original data is filtered #
  ########################################################
  observeEvent(finalCrudeCombined$metricsFULL, {
    
    # Resetting the index
    current_index_crudeMetricPanel(1)
    
  })
  
#------------------------------------------------------------------------------#
# Rendering the crude metrics figures ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the panels of crude metrics figures created in   #
# the above function.                                                          #
#------------------------------------------------------------------------------#
  
  ########################
  # Rendering the figure #
  ########################
  
  #############################
  # Try to produce the figure #
  #############################
  tryCatch({
    
    # Producing the panel plot
    output$crudeMetricOtherPanel <- renderPlot({
      
      CrudeMetricsOtherPlots$figures[[current_index_crudeMetricPanel()]]
      
    })
    
  ########################
  # Runs if error occurs #
  ########################
  }, error = function(e){
    
    output$crudeMetricOtherPanel <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Creating the title for panel -------------------------------------------------
#------------------------------------------------------------------------------#
# About: Creating the title for the panel figures.                             #
#------------------------------------------------------------------------------#
  
  #######################
  # Rendering the title #
  #######################
  output$crudeMetricOtherPanelTitle <- renderText({

    ###########################
    # Runs if data is entered #
    ###########################
    tryCatch({

      # Rendering the title
      return(names(CrudeMetricsOtherPlots$figures[current_index_crudeMetricPanel()]))

    ##############################
    # Runs if no data is entered #
    ##############################
    }, error = function(e){

      # Returning a NULL
      NULL

    }) # End of 'tryCatch'

  })
  
#------------------------------------------------------------------------------#
# Resetting the reactive value with the panel figures --------------------------
#------------------------------------------------------------------------------#
# About: This section resets the reactive value containing the crude metric    #
# figures (panel) when the original data is changed.                           # 
#------------------------------------------------------------------------------#
  
  ########################################################################
  # Observing the change in the reactive value holding the original data #
  ########################################################################
  observeEvent(file(), {
    
    # Reset the reactive value to NULL when file() changes
    CrudeMetricsOtherPlots$figures <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Resetting the reactive value containing crude metrics panels if error --------
#------------------------------------------------------------------------------#
# About: This section resets the reactive value containing the crude metric    #
# figures (panel) when the original data is changed.                           #
#------------------------------------------------------------------------------#
  
  #######################################
  # Observing changes in reactive value #
  #######################################
  observe({
    
    tryCatch({
      
      if(all(is.null(finalCrudeCombined$metricsFULL))){
        
        # Resetting the reactive value to NULL
        CrudeMetricsOtherPlots$figures <- NULL
    
      }
      
      }, error = function(e){
        
        NULL
      })
    
    })
  
#------------------------------------------------------------------------------#
# Creating the download handler for the panel figures --------------------------
#------------------------------------------------------------------------------#
# About: This section enables the downloading of the crude metric figures to   #
# the folder of the user's choosing.                                           #
#------------------------------------------------------------------------------#
  
  #################################################
  # Message that pops up with downloading options #
  #################################################
  
  #################################
  # Setting Figure Specifications #
  #################################
  observeEvent(input$downloadOtherCombinedMetricsFig, {
    
    ################################
    # Figure specification options #
    ################################
    isolate({
      
      showModal(modalDialog(
        
        title = "Figure Specifications",
        numericInput("dpi", "Figure DPI:", value = 900),
        numericInput("width", "Figure Width:", value = 9),
        numericInput('height', 'Figure Height:', value = 5),
        pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
        pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
        downloadButton("downloadOtherCrudeMetrics", "Download Forecast Figures"),
        easyClose = TRUE
        
      )) 
      
    })
    
  })  
  
  ##########################
  # Downloading the images #
  ##########################
  output$downloadOtherCrudeMetrics <- downloadHandler(
    
    ####################
    # Filename for ZIP #
    ####################
    filename = function(){
      
      paste("model-comparison-panel-crude-metrics.zip", sep = "")
      
    },
    
    ############################################
    # Determining what should be in the folder #
    ############################################
    content = function(file){
      
      # Removing the message
      removeModal()
      
      # Creating a temp directory for files 
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      
      # Physically creating the directory 
      dir.create(temp_directory)
      
      # Saving the ggplots 
      for (plot_name in names(CrudeMetricsOtherPlots$figures)) {
        
        # Plot 
        plot_obj <- CrudeMetricsOtherPlots$figures[[plot_name]]
        
        # If plot is found 
        if (!is.null(plot_obj)) {
          
          # File name 
          file_name <- glue("{plot_name}.{input$extFig}")
          
          # TIFF file 
          if(input$extFig == "tiff"){
            ggsave(
              file.path(temp_directory, file_name),
              plot = plot_obj,
              dpi = input$dpi,
              width = input$width,
              height = input$height,
              units = input$units,
              device = input$extFig,
              compression = "lzw")
            
          }else{
            
            # All other image types
            ggsave(
              file.path(temp_directory, file_name),
              plot = plot_obj,
              dpi = input$dpi,
              width = input$width,
              height = input$height,
              units = input$units,
              device = input$extFig)
            
          }
          
        }
        
      }
      
      #####################
      # Create a zip file #
      #####################
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
    },
    
    contentType = "application/zip"
    
  )
  
  
#------------------------------------------------------------------------------#
# Calculating the average metrics ----------------------------------------------
#------------------------------------------------------------------------------#
# This section calculates the average metrics based upon the crude metrics     #
# from above. The average metrics are then saved within a reactive value for   #
# filtering at later steps.                                                    #
#------------------------------------------------------------------------------#
 
  ############################################
  # Reactive value to save the crude metrics #
  ############################################
  averageMetricsDataOther <- reactiveValues()
  
  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({
    
    ################################
    # Running if data is available #
    ################################
    tryCatch({
      
      #######################################################
      # Function to produce the individual forecast figures #
      #######################################################
      observeEvent(crudeMetricsDataOther$data, ignoreNULL = T, {
        
        ##################################
        # Running only if data is loaded #
        ##################################
        if(!is.null(crudeMetricsDataOther$data)){
          
          # Isolating to when one of the buttons is clicked 
          isolate({
            
            ##################################
            # Running only if data is loaded #
            ##################################
            avgMetricsOutput <- avgAllMetrics(metricsCrude.input = crudeMetricsDataOther$data)
            
          }) # End of isolate
          
          ##########################################
          # Saving the metrics in a reactive value #
          ##########################################
          averageMetricsDataOther$data <- avgMetricsOutput
          
        }
        
      }) # End of 'observeEvent'
      
      #################################
      # Runs if data is not available #
      #################################
    }, error = function(e){
      
      # Returning NULL
      NULL
      
    }) # End of 'tryCaych'
    
  }) # End of 'observe'
  
  
#------------------------------------------------------------------------------#
# Returning the respective errors ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the average metrics reactive value if an error    #
# occurs with loaded data into the dashboard.                                  #
#------------------------------------------------------------------------------#
  
  #################################################################
  # Observing changes in the reactive value containing the panels #
  #################################################################
  observe({
    
    #############################
    # Runs if data is available #
    #############################
    tryCatch({
      
      ##################################################################
      # Error to return if the files are loaded prior to the dashboard #
      ##################################################################
      if(crudeMetricsDataOther$data == "ERROR1"){
        
        # Clearing the reactive value containing the average data
        averageMetricsDataOther$data <- NULL
        
      #######################################################
      # Error to return if the column names are not correct #
      #######################################################
      }else if(crudeMetricsDataOther$data == "ERROR2"){
        
        # Clearing the reactive value containing the average data
        averageMetricsDataOther$data <- NULL
        
      ########################################################
      # Error to return if the name of the file is incorrect #
      ########################################################
      }else if(crudeMetricsDataOther$data == "ERROR3"){
        
        # Clearing the reactive value containing the average data
        averageMetricsDataOther$data <- NULL
        
      ###################################################
      # Error to return if there is an issue with dates #
      ###################################################
      }else if(crudeMetricsDataOther$data == "ERROR4"){
        
        # Clearing the reactive value containing the average data
        averageMetricsDataOther$data <- NULL
        
      ##########################################################
      # Error to return if there is an issue with the location #
      ##########################################################
      }else if(crudeMetricsDataOther$data == "ERROR5"){
        
        # Clearing the reactive value containing the average data
        averageMetricsDataOther$data <- NULL
        
      }
      
    #################################
    # Runs if data is not available #
    #################################
    }, error = function(e){
      
      NULL
      
    })
    
  })
  
#------------------------------------------------------------------------------#
# Clearing output --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the average metrics output if the original data   #
# is changed.                                                                  #
#------------------------------------------------------------------------------#
  
  ###############################
  # Looking for the data change #
  ###############################
  observeEvent(file(), {
    
    # Clearing the output
    averageMetricsDataOther$data <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Creating the filtering options for the average metrics data ------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options for the average metrics    #
# data table. The filtering options are triggered when the 'Filtering Options' #
# button is clicked.                                                           #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################

  # To store the filtered average data
  finalAvgCombined <- reactiveValues()

  # Model choices
  modelReactiveChoiceAvg <- reactiveValues()

  # Selected models
  modelReactiveSelectAvg <- reactiveValues()

  # Performance metric choice
  performanceReactiveChoiceAvg <- reactiveValues()

  # Selected performance metric choices
  performanceReactiveSelectAvg <- reactiveValues()

  # Location choices
  locationReactiveChoiceAvg <- reactiveValues()

  # Selected locations
  locationReactiveSelectAvg <- reactiveValues()

  # Calibration length choices
  calibrationReactiveChoiceAvg <- reactiveValues()

  # Selected calibration lengths
  calibrationReactiveSelectAvg <- reactiveValues()

  # Horizon choices
  horizonReactiveChoiceAvg <- reactiveValues()

  # Selected horizon
  horizonReactiveSelectAvg <- reactiveValues()

  # Indicator for which data to show
  indicatorForFilterMetricsAvg <- reactiveVal(0)

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
  
    #####################################
    # Running if information is entered #
    #####################################
    tryCatch({

      # Running only if an error is not returned above
      if(all(crudeMetricsDataOther$data %!in% c("ERROR1", "ERROR2", "ERROR3",
                                                "ERROR4", "ERROR5", !is.null(averageMetricsDataOther$data)))){

        # Average metrics data
        avgMetrics <- averageMetricsDataOther$data
      
        #################
        # Model Options #
        #################

        # Choices
        modelReactiveChoiceAvg$data <- unique(avgMetrics$Model)

        # Selected
        modelReactiveSelectAvg$data <- modelReactiveChoiceAvg$data

        ############################
        # Performance type metrics #
        ############################

        # Choices
        performanceReactiveChoiceAvg$data <- unique(avgMetrics$`Performance Metric Type`)

        # Selected
        performanceReactiveSelectAvg$data <- performanceReactiveChoiceAvg$data

        ####################
        # Location Options #
        ####################

        # Choices
        locationReactiveChoiceAvg$data <- unique(avgMetrics$Location)

        # Selected
        locationReactiveSelectAvg$data <- locationReactiveChoiceAvg$data

        #######################
        # Calibration Options #
        #######################

        # Choices
        calibrationReactiveChoiceAvg$data <- unique(avgMetrics$Calibration)

        # Selected
        calibrationReactiveSelectAvg$data <- calibrationReactiveChoiceAvg$data

        ###################
        # Horizon Options #
        ###################

        # Choices
        horizonReactiveChoiceAvg$data <- unique(avgMetrics$Horizon)

        # Selected
        horizonReactiveSelectAvg$data <- horizonReactiveChoiceAvg$data

        #######################
        # Creating the pop-up #
        #######################
        observeEvent(input$filterAvgCombinedMetrics, ignoreInit = T,{

          # Changing the indicator to one (i.e., button has been clicked)
          isolate({indicatorForFilterMetricsAvg(1)})

          # Isolating button click behavior
          isolate({

            # Button
            showModal(modalDialog(
              title = "Filtering Options",
              pickerInput("ModelCrudeData2", "Model:", c(modelReactiveChoiceAvg$data), selected = c(modelReactiveSelectAvg$data), multiple = T), # Model filtering
              pickerInput("perfType2", "Performance Metric Type:", c(performanceReactiveChoiceAvg$data), selected = c(performanceReactiveSelectAvg$data), multiple = T), # Metric Type
              pickerInput("locationChoicesCrude2", "Location:", c(locationReactiveChoiceAvg$data), selected = c(locationReactiveSelectAvg$data), multiple = T), # Location
              pickerInput("calibrationChoices2", "Calibration:", c(calibrationReactiveChoiceAvg$data), selected = c(calibrationReactiveSelectAvg$data), multiple = T), # Calibration
              pickerInput("horizonChoices2", "Horizon:", c(horizonReactiveChoiceAvg$data), selected = c(horizonReactiveSelectAvg$data), multiple = T) # Horizon

            ))

          })

        })

        #######################################################
        # Function to produce the individual forecast figures #
        #######################################################
        filteredMetricsAvg  <- filterOtherMetrics(crudeMetrics.input = NULL, # Crude metrics
                                                  averageMetrics.input = averageMetricsDataOther$data, # Average metrics
                                                  crudeModel.input = NULL, # Crude model choices
                                                  crudePerformance.input = NULL, # Crude performance metric type
                                                  crudeLocation.input = NULL, # Crude location choice
                                                  crudeCalibration.input = NULL, # Crude calibration choice
                                                  crudeHorizon.input = NULL, # Crude horizon choice
                                                  AverageModel.input = input$ModelCrudeData2, # Average model choice
                                                  AveragePerformance.input = input$perfType2, # Average performance metric type
                                                  AverageLocation.input = input$locationChoicesCrude2, # Average location choice
                                                  AverageCalibration.input = input$calibrationChoices2, # Average calibration choice
                                                  AverageHorizon.input = input$horizonChoices2, # Average horizon choice
                                                  inputindicator = indicatorForFilterMetricsAvg()) # Indicator for data to show
                                                   

        # Saving the results to the reactive value
        finalAvgCombined$metricsFULL <- filteredMetricsAvg

      } # End of 'else'

    ####################################
    # Running if data is NOT available #
    ####################################
    }, error = function(e){

      NULL
    })

  })
  

#------------------------------------------------------------------------------#
# Clearing the rendered average metrics ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the data rendered below if an error occurs in the #
# data loaded process.                                                         #
#------------------------------------------------------------------------------#
  
  #######################################
  # Observing changes in reactive value #
  #######################################
  observe({
    
    ################################
    # Running if data is available #
    ################################
    tryCatch({
      
      ####################################
      # Clearing data if an error occurs #
      ####################################
      if(clearAverageMetricsOther() == 1){
        
        # Clearing the average metrics data 
        finalAvgCombined$metricsFULL <- NULL
        
      #######################################
      # Keeping the data if no error occurs #
      #######################################
      }else{
        
        # Keeping the normal metrics
        finalAvgCombined$metricsFULL <- finalAvgCombined$metricsFULL
        
      }
      
    ####################################
    # Running if data is not available #
    ####################################
    }, error = function(e){NULL})

  })

#------------------------------------------------------------------------------#
# Rendering the data table with the average data -------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the data table with the average data to the main #
# dashboard.                                                                   #
#------------------------------------------------------------------------------#

  ############################
  # Rendering the data frame #
  ############################
  output$AverageMetricsOther <- renderDataTable({

    ###########################
    # Runs if no errors occur #
    ###########################
    tryCatch({

      # Returning the data frame
      return(datatable(finalAvgCombined$metricsFULL))

    ###########################
    # Runs if an error occurs #
    ###########################
    }, error = function(e){

      # Returning a null
      NULL

    }) # End of 'tryCatch'

  }) # End of 'renderDataTable'


#------------------------------------------------------------------------------#
# Clearing the filtered data set and indicator ---------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the final, filtered data and resets the indicator #
# for the data to show (i.e., full or filtered). It is triggered when the      #
# original data is changed.                                                    #
#------------------------------------------------------------------------------#

  ########################################################################
  # Observing the change in the reactive value holding the original data #
  ########################################################################
  observeEvent(file(), {

    if(is.null(foremattedForecasts$forecasts)){

      # Filtered data
      finalAvgCombined$metricsFULL <- NULL

      # Resetting the indicator
      indicatorForFilterMetricsAvg(0)

    }

  })

#------------------------------------------------------------------------------#
# Downloading the combined metrics data as a '.csv' ----------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the combined average metrics as a '.csv' file to    #
# the directory of their choosing.                                             #
#------------------------------------------------------------------------------#

  output$downloadAverageMetrics <- downloadHandler(

    ####################################
    # Function to create the file-name #
    ####################################
    filename = function() {

      # File name
      paste("combined-average-metrics-", input$dataset, sep = "")

    },

    #############################
    # Function to save the file #
    #############################
    content = function(file) {

      # Saving the file
      write.csv(finalAvgCombined$metricsFULL, file, row.names = FALSE)

    }

  ) # End of download button


#------------------------------------------------------------------------------#
# Button to edit average metrics figures ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the legend labels for the    #
# panel figures of average metrics.                                            #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYOtherAvgMetric <- reactiveValues(logScale = NULL) 
  
  ########################################
  # Reactive value for the low-end color #
  ########################################
  lowColorAverageMetricOther <- reactiveValues(lowColor = "#6495ED")
  
  #########################################
  # Reactive value for the high-end color #
  #########################################
  highColorAverageMetricOther <- reactiveValues(highColor = "#D0312D")
  
  ########################################
  # Reactive value for the outline color #
  ########################################
  outlineColorAverageMetricOther <- reactiveValues(outlineColor = "#FFFFFF")
  
  #####################################
  # Reactive value for the text color #
  #####################################
  textColorAverageMetricOther <- reactiveValues(textColor = "#FFFFFF")
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$figOptCombAvgMetrics, {
    
    # Determining the metric choices 
    allMetricNames <- colnames(finalAvgCombined$metricsFULL)
    
    # Creating the metric choice variable
    metricChoices <- allMetricNames[-c(1:5)]
    
    # Creating the pop-up
    showModal(modalDialog(
      title = "Figure Options",
      pickerInput("logScaleOtherAvgMetric", "Metrics to Show in Log Base 10:", c("None", metricChoices), selected = scaleYOtherAvgMetric$logScale, multiple = T),
      textInput("lowColor", "Color for the lowest values (#XXXXXX):", value = lowColorAverageMetricOther$lowColor),
      textInput("highColor", "Color for the highest values (#XXXXXX):", value = highColorAverageMetricOther$highColor),
      textInput("outlineColor", "Color for the outline of tiles (#XXXXXX):", value = outlineColorAverageMetricOther$outlineColor),
      textInput("textColor", "Color for the text (#XXXXXX):", value = textColorAverageMetricOther$textColor)
    ))
    
  })
  
  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleOtherAvgMetric, {
    
    scaleYOtherAvgMetric$logScale <- input$logScaleOtherAvgMetric
    
  })
  
  ##########################################
  # Update the reactive value - Low colors #
  ##########################################
  observeEvent(input$lowColor,{
    
    # Updating the number of date breaks
    lowColorAverageMetricOther$lowColor <- input$lowColor
    
  })
  
  ###########################################
  # Update the reactive value - High colors #
  ###########################################
  observeEvent(input$highColor,{
    
    # Updating the number of date breaks
    highColorAverageMetricOther$highColor <- input$highColor
    
  })
  
  ##############################################
  # Update the reactive value - Outline colors #
  ##############################################
  observeEvent(input$outlineColor,{
    
    # Updating the number of date breaks
    outlineColorAverageMetricOther$outlineColor <- input$outlineColor
    
  })
  
  ###########################################
  # Update the reactive value - Text colors #
  ###########################################
  observeEvent(input$textColor,{
    
    # Updating the number of date breaks
    textColorAverageMetricOther$textColor <- input$textColor
    
  })
  
  
#------------------------------------------------------------------------------#
# Plotting the average metrics -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section plots the average metrics based upon the entered files,  #
# the dashboard, and how the users filter the data.                            #
#------------------------------------------------------------------------------#
  
  ####################################################
  # Creating the reactive value to save the plots in #
  ####################################################
  AvgMetricsOtherPlots <- reactiveValues()
  
  ###########################################
  # Observing changes in reactive behaviors #
  ###########################################
  observe({
    
    ############################# 
    # Runs if data is available #
    #############################
    tryCatch({
      
      ######################################
      # List of triggering reactive values #
      ######################################
      AvgMetricsOtherPlotsTrigger <- reactive({
        
        # Triggering events 
        allEvents <- list(finalAvgCombined$metricsFULL,
                          scaleYOtherAvgMetric$logScale,
                          lowColorAverageMetricOther$lowColor,
                          highColorAverageMetricOther$highColor,
                          outlineColorAverageMetricOther$outlineColor,
                          textColorAverageMetricOther$textColor) 
        
        # Returning the list of reactive values 
        return(allEvents)
        
      })
      
      #########################################################
      # Function to produce the panel average metrics figures #
      #########################################################
      observeEvent(AvgMetricsOtherPlotsTrigger(), ignoreNULL = T, {
        
        ##################################
        # Running only if data is loaded #
        ##################################
        if(all(!is.null(finalAvgCombined$metricsFULL))){
          
          # Isolating to when one of the buttons is clicked 
          isolate({
            
            #######################################################
            # Function to produce the individual forecast figures #
            #######################################################
            avgMetricOtherpanelOutput <- avgMetricsFigureOther(avgData.input = finalAvgCombined$metricsFULL, 
                                                                scale.y.input = scaleYOtherAvgMetric$logScale, 
                                                                lowColor.input = lowColorAverageMetricOther$lowColor,
                                                                highColor.input = highColorAverageMetricOther$highColor,  
                                                                outlineColor.input = outlineColorAverageMetricOther$outlineColor, 
                                                                textColor.input = textColorAverageMetricOther$textColor)
            
            # Saving the output to the reactive value list
            AvgMetricsOtherPlots$figures <- avgMetricOtherpanelOutput
            
          }) # End of isolate
          
        }
        
      }) # End of 'observeEvent'
      
    }, error = function(e){
      
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'observe'
  
  
#------------------------------------------------------------------------------#
# Creating the previous and next arrows for the other metric figures -----------
#------------------------------------------------------------------------------#
# About: This section creates the reactive value, and previous and next arrow  #
# buttons for the metric other crude metric figures. It then gives              #
# functionality to the buttons as its related to going through the list of     #
# figures.                                                                     #
#------------------------------------------------------------------------------#
  
  #######################################################################
  # Creating the reactive value to be used with the other panel buttons #
  #######################################################################
  current_index_avgMetricPanel <- reactiveVal(1)
  
  #################################################
  # Going backwards if the previous button is hit #
  #################################################
  observeEvent(input$otheravgMetricPanelsPrevious, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Running if the current index is greater than one
      if(current_index_avgMetricPanel() > 1){
        
        # Changing the index of the reactive value
        current_index_avgMetricPanel(max(current_index_avgMetricPanel() - 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement
  
  ############################################
  # Going forwards if the next button is hit #
  ############################################
  observeEvent(input$otheravgMetricPanelsNext, {
    
    # Isolating the action to only when the button is clicked
    isolate({
      
      # Run if the current index is less than the length of the list
      if (current_index_avgMetricPanel() < length(AvgMetricsOtherPlots$figures)) {
        
        # Changing the index of the reactive value
        current_index_avgMetricPanel(min(current_index_avgMetricPanel() + 1))
        
      }
      
    }) # End of 'isolate' statement
    
  }) # End of 'observeEvent' statement 
  
  ########################################################
  # Resetting the index if the original data is filtered #
  ########################################################
  observeEvent(finalAvgCombined$metricsFULL, {
    
    # Resetting the index
    current_index_avgMetricPanel(1)
    
  })
  
#------------------------------------------------------------------------------#
# Rendering the average metrics figures ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the individual average metrics figures created   #
# in the above function.                                                       #
#------------------------------------------------------------------------------#
  
  ########################
  # Rendering the figure #
  ########################
  
  #############################
  # Try to produce the figure #
  #############################
  tryCatch({
    
    # Producing the panel plot
    output$avgMetricOtherPanel <- renderPlot({
      
      AvgMetricsOtherPlots$figures[[current_index_avgMetricPanel()]]
      
    })
    
    ########################
    # Runs if error occurs #
    ########################
  }, error = function(e){
    
    output$avgMetricOtherPanel <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Creating the title for individual avg figures  -------------------------------
#------------------------------------------------------------------------------#
# About: Creating the title for the individual average figures.                #
#------------------------------------------------------------------------------#
  
  #######################
  # Rendering the title #
  #######################
  output$avgMetricOtherPanelTitle <- renderText({
    
    ###########################
    # Runs if data is entered #
    ###########################
    tryCatch({
      
      # Rendering the title
      return(names(AvgMetricsOtherPlots$figures[current_index_avgMetricPanel()]))
      
      ##############################
      # Runs if no data is entered #
      ##############################
    }, error = function(e){
      
      # Returning a NULL
      NULL
      
    }) # End of 'tryCatch'
    
  })
  
#------------------------------------------------------------------------------#
# Resetting the reactive value with the individual average figures -------------
#------------------------------------------------------------------------------#
# About: This section resets the reactive value containing the avg metric      #
# figures when the original data is changed.                                   # 
#------------------------------------------------------------------------------#
  
  ########################################################################
  # Observing the change in the reactive value holding the original data #
  ########################################################################
  observeEvent(file(), {
    
    # Reset the reactive value to NULL when file() changes
    AvgMetricsOtherPlots$figures <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Resetting the reactive value containing avg metrics figures if error ---------
#------------------------------------------------------------------------------#
# About: This section resets the reactive value containing the avg metric      #
# figures when the original data is changed.                                   #
#------------------------------------------------------------------------------#
  
  #######################################
  # Observing changes in reactive value #
  #######################################
  observe({
    
    tryCatch({
      
      if(all(is.null(finalAvgCombined$metricsFULL))){
        
        # Resetting the reactive value to NULL
        AvgMetricsOtherPlots$figures <- NULL
        
      }
      
    }, error = function(e){
      
      NULL
    })
    
  })
  
#------------------------------------------------------------------------------#
# Creating the download handler for the panel figures --------------------------
#------------------------------------------------------------------------------#
# About: This section enables the downloading of the average metric figures to #
# the folder of the user's choosing.                                           #
#------------------------------------------------------------------------------#

  #################################################
  # Message that pops up with downloading options #
  #################################################

  #################################
  # Setting Figure Specifications #
  #################################
  observeEvent(input$downloadOtherCombinedMetricsFigAvg, {

    ################################
    # Figure specification options #
    ################################
    isolate({

      showModal(modalDialog(

        title = "Figure Specifications",
        numericInput("dpi", "Figure DPI:", value = 900),
        numericInput("width", "Figure Width:", value = 9),
        numericInput('height', 'Figure Height:', value = 5),
        pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
        pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
        downloadButton("downloadOtherAvgMetricsFig", "Download Forecast Figures"),
        easyClose = TRUE

      ))

    })

  })

  ##########################
  # Downloading the images #
  ##########################
  output$downloadOtherAvgMetricsFig <- downloadHandler(

    ####################
    # Filename for ZIP #
    ####################
    filename = function(){

      paste("model-comparison-panel-average-metrics.zip", sep = "")

    },

    ############################################
    # Determining what should be in the folder #
    ############################################
    content = function(file){

      # Removing the message
      removeModal()

      # Creating a temp directory for files
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))

      # Physically creating the directory
      dir.create(temp_directory)

      # Saving the ggplots
      for (plot_name in names(AvgMetricsOtherPlots$figures)) {

        # Plot
        plot_obj <- AvgMetricsOtherPlots$figures[[plot_name]]

        # If plot is found
        if (!is.null(plot_obj)) {

          # File name
          file_name <- glue("{plot_name}.{input$extFig}")

          # TIFF file
          if(input$extFig == "tiff"){
            ggsave(
              file.path(temp_directory, file_name),
              plot = plot_obj,
              dpi = input$dpi,
              width = input$width,
              height = input$height,
              units = input$units,
              device = input$extFig,
              compression = "lzw")

          }else{

            # All other image types
            ggsave(
              file.path(temp_directory, file_name),
              plot = plot_obj,
              dpi = input$dpi,
              width = input$width,
              height = input$height,
              units = input$units,
              device = input$extFig)

          }

        }

      }

      #####################
      # Create a zip file #
      #####################
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )

    },

    contentType = "application/zip"

  )




#------------------------------------------------------------------------------#
# Winkler Scores ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the Winkler Scores based upon the dashboard   #
# models (i.e., ARIMA, GLM, GAM, and Prophet) and the formatted forecasts      #
# read into the dashboard.                                                     #
#------------------------------------------------------------------------------#

######################################################
# Creating a reactive value to save winkler score in #
######################################################
winklerScoresModelCompare <- reactiveValues()

################################################
# Observing changes in the formatted forecasts #
################################################
observe({
  
  ########################################
  # Trying to run - If data is available #
  ########################################
  tryCatch({
    
    ###########################################
    # Locations included in the original data #
    ###########################################
    dataLocations <- colnames(file())[-1]
    
    # Events
    triggerWinklerOther <- reactive({
      
      events <- list(input$winklerOtherAvg, input$dataset2)
      
      return(events)
      
    })
    
    #####################################
    # Observing changes in the data set #
    #####################################
    observeEvent(triggerWinklerOther(), ignoreNULL = T, {
      
      ##################################
      # Running only if data is loaded #
      ##################################
      if(all(!is.null(listOtherForecasts$forecastData))){
    
        # Isolating to when the data is changed 
        isolate({
          
          #######################################
          # Running the winkler scores function #
          #######################################
          winkler.scores.output <- Winkler.Scores.Model.Comparison(formatted.forecast.Other = listOtherForecasts$forecastData, # Formatted forecast dashboard
                                                                   formatted.forecast.DASHBOARD = foremattedForecasts$forecasts, # Formatted forecast inputted
                                                                   calibrationPeriod.input = input$calibrationPeriod, # Calibration
                                                                   forecastHorizon.input = input$forecastHorizon, # Horizon
                                                                   locations.input = dataLocations, # Location list 
                                                                   date.type.input = dateValues$dates, # Date type
                                                                   avgWinler.input = input$winklerOtherAvg) # Winkler Scores
        
      
          # Saving the output to a reactive value
          winklerScoresModelCompare$scores <- winkler.scores.output
        
       }) # End of isolate
        
      } # End of if-else
      
    }) # End of 'observeEvent' 
    
  ###################################
  # Returns if the code can not run #
  ###################################
  }, error = function(e){
    
    # Returning a NULL
    NULL
    
  }) # End of 'tryCatch'
  
}) # End of 'observe'



#------------------------------------------------------------------------------#
# Clearing the output if an error occurs ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the Winkler scores output if an error occurs in   #
# the loaded data.                                                             #
#------------------------------------------------------------------------------#

#################################################################
# Observing changes in the reactive value containing the panels #
#################################################################
observe({
  
  #############################
  # Runs if data is available #
  #############################
  tryCatch({
    
    ##################################################################
    # Error to return if the files are loaded prior to the dashboard #
    ##################################################################
    if(winklerScoresModelCompare$scores == "ERROR1"){
      
      # Clearing reactive values holding winkler scores
      winklerScoresModelCompare$scores <- NULL
      
      # Clearing reactive value containing other formatted forecast data 
      listOtherForecasts$forecastData <- NULL
      
    #######################################################
    # Error to return if the column names are not correct #
    #######################################################
    }else if(winklerScoresModelCompare$scores == "ERROR2"){
      
      # Clearing reactive values holding winkler scores
      winklerScoresModelCompare$scores <- NULL
      
      # Clearing reactive value containing other formatted forecast data 
      listOtherForecasts$forecastData <- NULL
      
    ########################################################
    # Error to return if the name of the file is incorrect #
    ########################################################
    }else if(winklerScoresModelCompare$scores == "ERROR3"){
      
      # Clearing reactive values holding winkler scores
      winklerScoresModelCompare$scores <- NULL
      
      # Clearing reactive value containing other formatted forecast data 
      listOtherForecasts$forecastData <- NULL
      
    ###################################################
    # Error to return if there is an issue with dates #
    ###################################################
    }else if(winklerScoresModelCompare$scores == "ERROR4"){
      
      # Clearing reactive values holding winkler scores
      winklerScoresModelCompare$scores <- NULL
      
      # Clearing reactive value containing other formatted forecast data 
      listOtherForecasts$forecastData <- NULL
      
    ##########################################################
    # Error to return if there is an issue with the location #
    ##########################################################
    }else if(winklerScoresModelCompare$scores == "ERROR5"){
      
      # Clearing reactive values holding winkler scores
      winklerScoresModelCompare$scores <- NULL
      
      # Clearing reactive value containing other formatted forecast data 
      listOtherForecasts$forecastData <- NULL
      
    }
    
  #################################
  # Runs if data is not available #
  #################################
  }, error = function(e){
    
    NULL
    
  })
  
})  

 
#------------------------------------------------------------------------------#
# Clearing output --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the Winkler score output if the original data     #
# is changed.                                                                  #
#------------------------------------------------------------------------------#
  
  ###############################
  # Looking for the data change #
  ###############################
  observeEvent(file(), {
    
    # Clearing the output
    winklerScoresModelCompare$scores <- NULL
    
    # Resetting the indicator
    indicatorForFilterWinkler(0)
    
  })
  
  
#------------------------------------------------------------------------------#
# Creating the filtering options for the Winkler Scores data -------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options for the Winkler Scores     #
# data table. The filtering options are triggered when the 'Filtering Options' #
# button is clicked.                                                           #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################
  
  # To store the filtered Winkler Scores data
  finalWinklerCombined <- reactiveValues()
  
  # Model choices
  modelReactiveChoiceWinkler <- reactiveValues()
  
  # Selected models
  modelReactiveSelectWinkler <- reactiveValues()
  
  # Performance metric choice
  performanceReactiveChoiceWinkler <- reactiveValues()
  
  # Selected performance metric choices
  performanceReactiveSelectWinkler <- reactiveValues()
  
  # Location choices
  locationReactiveChoiceWinkler <- reactiveValues()
  
  # Selected locations
  locationReactiveSelectWinkler <- reactiveValues()
  
  # Calibration length choices
  calibrationReactiveChoiceWinkler <- reactiveValues()
  
  # Selected calibration lengths
  calibrationReactiveSelectWinkler <- reactiveValues()
  
  # Horizon choices
  horizonReactiveChoiceWinkler <- reactiveValues()
  
  # Selected horizon
  horizonReactiveSelectWinkler <- reactiveValues()
  
  # Indicator for which data to show 
  indicatorForFilterWinkler <- reactiveVal(0)
  
 ########################################
 # Observing changes in reactive values #
 ########################################
 observe({

   #####################################
   # Running if information is entered #
   #####################################
   tryCatch({

     # Running only if an error is not returned above
     if(all(winklerScoresModelCompare$scores %!in% c("ERROR1", "ERROR2", "ERROR3",
                                               "ERROR4", "ERROR5", !is.null(winklerScoresModelCompare$scores)))){

       # Winkler Scores data
       winklerScoreData <- winklerScoresModelCompare$scores

       #################
       # Model Options #
       #################

       # Choices
       modelReactiveChoiceWinkler$data <- unique(winklerScoreData$Model)

       # Selected
       modelReactiveSelectWinkler$data <- modelReactiveChoiceWinkler$data

       ############################
       # Performance type metrics #
       ############################

       # Choices
       performanceReactiveChoiceWinkler$data <- unique(winklerScoreData$`Performance Metric Type`)

       # Selected
       performanceReactiveSelectWinkler$data <- performanceReactiveChoiceWinkler$data

       ####################
       # Location Options #
       ####################

       # Choices
       locationReactiveChoiceWinkler$data <- unique(winklerScoreData$Location)

       # Selected
       locationReactiveSelectWinkler$data <- locationReactiveChoiceWinkler$data

       #######################
       # Calibration Options #
       #######################

       # Choices
       calibrationReactiveChoiceWinkler$data <- unique(winklerScoreData$Calibration)

       # Selected
       calibrationReactiveSelectWinkler$data <- calibrationReactiveChoiceWinkler$data

       ###################
       # Horizon Options #
       ###################

       # Choices
       horizonReactiveChoiceWinkler$data <- unique(winklerScoreData$Horizon)

       # Selected
       horizonReactiveSelectWinkler$data <- horizonReactiveChoiceWinkler$data

       #######################
       # Creating the pop-up #
       #######################
       observeEvent(input$filterWinklerOtherMetrics, ignoreInit = T,{
         
         # Changing the indicator to one (i.e., button has been clicked)
         isolate({indicatorForFilterWinkler(1)})

         # Isolating button click behavior
         isolate({

           # Button
           showModal(modalDialog(
             title = "Filtering Options",
             pickerInput("ModelWinklerOther", "Model:", c(modelReactiveChoiceWinkler$data), selected = c(modelReactiveSelectWinkler$data), multiple = T), # Model filtering
             pickerInput("perfTypeWinkler", "Performance Metric Type:", c(performanceReactiveChoiceWinkler$data), selected = c(performanceReactiveSelectWinkler$data), multiple = T), # Metric Type
             pickerInput("locationChoicesWinkler", "Location:", c(locationReactiveChoiceWinkler$data), selected = c(locationReactiveSelectWinkler$data), multiple = T), # Location
             pickerInput("calibrationChoicesWinkler", "Calibration:", c(calibrationReactiveChoiceWinkler$data), selected = c(calibrationReactiveSelectWinkler$data), multiple = T), # Calibration
             pickerInput("horizonChoicesWinkler", "Horizon:", c(horizonReactiveChoiceWinkler$data), selected = c(horizonReactiveSelectWinkler$data), multiple = T) # Horizon

           ))

         })

       })

       #####################################################
       # Function to filter Winkler Scores - Other Metrics #
       #####################################################
       filterOtherWinkler <- filterOtherWinkler(Winkler.input = winklerScoresModelCompare$scores, # Original metrics 
                                                WinklerModel.input = input$ModelWinklerOther, # Winkler model filter
                                                WinklerPerformance.input = input$perfTypeWinkler, # Winkler performance filter
                                                WinklerLocation.input = input$locationChoicesWinkler, # Winkler location filter 
                                                WinklerCalibration.input = input$calibrationChoicesWinkler, # Winkler calibration filter
                                                WinklerHorizon.input = input$horizonChoicesWinkler, # Winkler horizon filter
                                                indicatorToShow = indicatorForFilterWinkler()) # Indicator to show


       # Saving the results to the reactive value
       finalWinklerCombined$metricsFULL <- filterOtherWinkler

     } # End of 'else'

    ####################################
    # Running if data is NOT available #
    ####################################
    }, error = function(e){

      NULL
    })

 })
  
#------------------------------------------------------------------------------#
# Rendering the data table with the winkler scores data ------------------------
#------------------------------------------------------------------------------#
# About: This section renders the data table with the winkler data to the main #
# dashboard.                                                                   #
#------------------------------------------------------------------------------#
  
  ############################
  # Rendering the data frame #
  ############################
  output$winklerScoresOther <- renderDataTable({
    
    ###########################
    # Runs if no errors occur #
    ###########################
    tryCatch({
      
      # Returning the data frame
      return(datatable(finalWinklerCombined$metricsFULL))
      
    ###########################
    # Runs if an error occurs #
    ###########################
    }, error = function(e){
      
      # Returning a null 
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'renderDataTable'  
  
 
#------------------------------------------------------------------------------#
# Downloading the Winkler data as a '.csv' -------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the Winkler Scores as a '.csv' file to              #
# the directory of their choosing.                                             #
#------------------------------------------------------------------------------#
  
  output$downloadWinklerMetrics <- downloadHandler(
    
    ####################################
    # Function to create the file-name #
    ####################################
    filename = function() {
      
      # File name
      paste("model-compare-Winkler-Scores-", input$dataset, sep = "")
      
    },
    
    #############################
    # Function to save the file #
    #############################
    content = function(file) {
      
      # Saving the file
      write.csv(finalWinklerCombined$metricsFULL, file, row.names = FALSE)
      
    }
    
  ) # End of download button 
  
  
#------------------------------------------------------------------------------#
# Creating the filtering options for the Skill Scores data ---------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options for the Skill Scores       #
# data table. The filtering options are triggered when the 'Filtering Options' #
# button is clicked.                                                           #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################
  
  # To store the filtered Skill Scores data
  finalSSCombined <- reactiveValues()
  
  # Model choices - Baseline
  modelReactiveChoiceBaseline <- reactiveValues()
  
  # Selected models - Baseline
  modelReactiveSelectBaseline <- reactiveValues()
  
  # Model choices - Comparison
  modelReactiveChoiceComparison <- reactiveValues()
  
  # Selected models - Comparison
  modelReactiveSelectComparison <- reactiveValues()
  
  # Performance Metrics Choices 
  performanceReactiveChoiceSS <- reactiveValues()
  
  # Performance Metrics Selected 
  performanceReactiveSelectSS <- reactiveValues()
  
  # Location choices
  locationReactiveChoiceSS <- reactiveValues()
  
  # Selected locations
  locationReactiveSelectSS <- reactiveValues()
  
  # Calibration length choices
  calibrationReactiveChoiceSS <- reactiveValues()
  
  # Selected calibration lengths
  calibrationReactiveSelectSS <- reactiveValues()
  
  # Horizon choices
  horizonReactiveChoiceSS <- reactiveValues()
  
  # Selected horizon
  horizonReactiveSelectSS <- reactiveValues() 
  
  # Indicator for which metrics to show
  filterSSIndicator <- reactiveVal(0)
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    #####################################
    # Running if information is entered #
    #####################################
    tryCatch({
      
      # Running only if an error is not returned above
      if(all(averageMetricsDataOther$data %!in% c("ERROR1", "ERROR2", "ERROR3",
                                                      "ERROR4", "ERROR5", !is.null(averageMetricsDataOther$data)))){
        
        # Average Metrics Data
        avgMetData <- averageMetricsDataOther$data
        
        ############################
        # Model Options - Baseline #
        ############################
        
        # Choices
        modelReactiveChoiceBaseline$data <- c(unique(avgMetData$Model))
        
        # Selected
        modelReactiveSelectBaseline$data <- modelReactiveChoiceBaseline$data
        
        ##############################
        # Model Options - Comparison #
        ##############################

        # Choices
        modelReactiveChoiceComparison$data <- c(unique(avgMetData$Model))

        # Selected
        modelReactiveSelectComparison$data <- modelReactiveChoiceComparison$data

        ############################
        # Performance type metrics #
        ############################

        # Choices
        performanceReactiveChoiceSS$data <- c(unique(avgMetData$`Performance Metric Type`))

        # Selected
        performanceReactiveSelectSS$data <- performanceReactiveChoiceSS$data

        ####################
        # Location Options #
        ####################

        # Choices
        locationReactiveChoiceSS$data <- c(unique(avgMetData$Location))

        # Selected
        locationReactiveSelectSS$data <- locationReactiveChoiceSS$data

        #######################
        # Calibration Options #
        #######################

        # Choices
        calibrationReactiveChoiceSS$data <- unique(avgMetData$Calibration)

        # Selected
        calibrationReactiveSelectSS$data <- calibrationReactiveChoiceSS$data

        ###################
        # Horizon Options #
        ###################

        # Choices
        horizonReactiveChoiceSS$data <- unique(avgMetData$Horizon)

        # Selected
        horizonReactiveSelectSS$data <- horizonReactiveChoiceSS$data

        #######################
        # Creating the pop-up #
        #######################
        observeEvent(input$filterSkillScoresOtherMetrics, ignoreInit = T,{
          
          # Setting the indicator
          isolate({filterSSIndicator(1)})
        
            # Button
            showModal(modalDialog(
              title = "Filtering Options",
              pickerInput("baselineModels", "Baseline Model(s):", c(modelReactiveChoiceBaseline$data), selected = c(modelReactiveSelectBaseline$data), multiple = T), # Model filtering - Baseline
              pickerInput("compareModels2", "Comparison Model(s):", c(modelReactiveChoiceComparison$data), selected = c(modelReactiveSelectComparison$data), multiple = T), # Model filtering - Comparison
              pickerInput("perfTypeSS", "Performance Metric Type:", c(performanceReactiveChoiceSS$data), selected = c(performanceReactiveSelectSS$data), multiple = T), # Metric Type
              pickerInput("locationInputSelect", "Location:", c(locationReactiveChoiceSS$data), selected = c(locationReactiveSelectSS$data), multiple = T), # Location
              pickerInput("calibrationChoicesSS", "Calibration:", c(calibrationReactiveChoiceSS$data), selected = c(calibrationReactiveSelectSS$data), multiple = T), # Calibration
              pickerInput("horizonChoicesSS", "Horizon:", c(horizonReactiveChoiceSS$data), selected = c(horizonReactiveSelectSS$data), multiple = T) # Horizon

              ))
  
          
        })
        
        ################################
        # Calculating the skill scores #
        ################################
        skillScores <- skillScoresOther(winkler.input = winklerScoresModelCompare$scores, # Winkler Scores 
                                        averageMetrics.input = averageMetricsDataOther$data, # Average Metrics 
                                        crudeMetrics.input = crudeMetricsDataOther$data, # Crude metrics
                                        averageIndicator.input = input$seeAvgSSOther, # Average metrics Indicator
                                        baseline.input = input$baselineModels, # Baseline models  
                                        compare.input = input$compareModels2, # Comparison models 
                                        filter.input = filterSSIndicator(), # Filtering input
                                        performance.input = input$perfTypeSS, # Performance metrics input
                                        location.input = input$locationInputSelect, # Location input 
                                        calibration.input = input$calibrationChoicesSS, # Calibration input 
                                        horizon.input = input$horizonChoicesSS) # Forecasting horizon input
                         
        


        # Saving the results to the reactive value
        finalSSCombined$metricsFULL <- skillScores
        
      } # End of 'else'
      
    ####################################
    # Running if data is NOT available #
    ####################################
    }, error = function(e){
      
      NULL
    })
    
  })
  
#------------------------------------------------------------------------------#
# Clearing output --------------------------------------------------------------  
#------------------------------------------------------------------------------#
# About: This section clears the skill score output if the original data     #
# is changed.                                                                  #
#------------------------------------------------------------------------------#
  
  ###############################
  # Looking for the data change #
  ###############################
  observeEvent(file(), {
    
    # Clearing the output
    finalSSCombined$metricsFULL <- NULL
    
    # Resetting the indicator
    filterSSIndicator(0)
    
  })
  
#------------------------------------------------------------------------------#
# Rendering the data table with the skill scores data -------------------------
#------------------------------------------------------------------------------#
# About: This section renders the data table with the skill scores data to the #
# main dashboard.                                                              #
#------------------------------------------------------------------------------#
  
  ############################
  # Rendering the data frame #
  ############################
  output$skillScoresOtherOUTPUT <- renderDataTable({
    
    ###########################
    # Runs if no errors occur #
    ###########################
    tryCatch({
      
      # Returning the data frame
      return(datatable(finalSSCombined$metricsFULL))
      
    ###########################
    # Runs if an error occurs #
    ###########################
    }, error = function(e){
      
      # Returning a null 
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'renderDataTable'  

#------------------------------------------------------------------------------#
# Downloading the skill scores data as a '.csv' --------------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the skill Scores as a '.csv' file to                #
# the directory of their choosing.                                             #
#------------------------------------------------------------------------------#
  
  output$downloadSSMetricsOther <- downloadHandler(
    
    ####################################
    # Function to create the file-name #
    ####################################
    filename = function() {
      
      # File name
      paste("model-compare-Skill-Scores-", input$dataset, sep = "")
      
    },
    
    #############################
    # Function to save the file #
    #############################
    content = function(file) {
      
      # Saving the file
      write.csv(finalSSCombined$metricsFULL, file, row.names = FALSE)
      
    }
    
  ) # End of download button 
  
  
}

shinyApp(ui = ui, server = server)
