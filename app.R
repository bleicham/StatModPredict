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
source("skill.scores.AGGP.R")
source("skill.scores.figures.AGGP.R")

#------------------------------------------------------------------------------#
#                             Needed Packages                                  #
#------------------------------------------------------------------------------#
pacman::p_load(MASS, shiny, shinydashboard, shinyWidgets, bslib, plotly, anytime,
               shinyalert, shinyjs, shinybusy, editData, shinyBS, DT, stringr,
               tidyverse, forstringr, mgcv, processx, ggpubr, shinyalert, forecast, 
               prophet, zip, glue, shinyjqui, patchwork, ggplot2, zoo, gridExtra,
               viridis, qdapRegex)

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
                      
 title = span("Forecasting Toolbox",  # Title 
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
                      min = 1 # Setting the minimum possible calibration period to zero
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
         startExpanded = F
         
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
             
             # Row One - P parameter 
             splitLayout(
               
               # Width of cells
               cellWidths = c("49%", "49%"),
               
               # Min P 
               uiOutput("pMin"),
               
               # Max P
               uiOutput("pMax")
             ),
             
             # Row Two - Q Parameter 
             splitLayout(
               
               # Width of cells 
               cellWidths = c("49%", "49%"),
               
               # Min Q
               uiOutput("qMin"),
               
               # Max Q
               uiOutput("qMax")
             )
             
             ), # End of Fluid Row 
           
           # Differences parameter 
           uiOutput("differences")
           
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
               
                 ################################################
                 # Conditiona Panel: Rendering title for panels #
                 ################################################
                 conditionalPanel(
                   
                   # Condition
                   condition = "input.panelModelsForecasts",
                 
                   # Rendering the title 
                   textOutput("panelForecastTitle")
                   
                 ) # End of conditional panel
                 
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
                     
                     #################################
                     # Creating the locations button #
                     #################################
                     div(uiOutput("locationsForecastFigs")), 
                     
                     ###################################
                     # Conditional panel: Model Filter #
                     ###################################
                     conditionalPanel(
                       
                       # Condition
                       condition = "!input.panelModelsForecasts",
                       
                       # Creating the models
                       div(style = "margin-left: 10px;", uiOutput("modelsForecastFigs"))
                       
                     ), # End of conditional panel
                     
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
                  
                  # Rendering the location button 
                  div(uiOutput("locationsQuantile"), style = "margin-right:10px"),
                  
                  # Rendering the model button
                  div(uiOutput("modelQuantile"), style = "margin-right:10px")
                  
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
              
              # Check-mark to see data
              div(style = "margin-right: 10px", checkboxInput("timeseriesCheckBox", "Show Underlying Data")),
              
              # Edit legend names 
              div(style = "margin-right: 10px", actionButton("editLegendLabels", "Edit Legend Labels")), 
              
              # Check mark for forecast lines
              div(checkboxInput("forecastLines", "Show Forecast Dates"))
              
          ) # End of main style row
          
        ) # End of alignment column
        
      ), # End of row for figure options
      
      #####################################################
      # Showing the data rather than the time series plot #
      #####################################################
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
                  
                  # Creating the download button 
                  div(style = "margin-right: 10px",
                      downloadButton("download_AvgMetrics", "Download Average Metrics"))
                  
                ), # End of condition
                
                ######################################
                # Conditional Panel: Download Figure #
                ######################################
                conditionalPanel(
                  
                  # Condition
                  condition = "input.AvgFigure", 
                  
                  #Download Button
                  div(style = "margin-right: 10px",
                      actionButton("download_AvgmetricsFig", "Download Average Metrics Figure", icon = icon("download")))
                  
                ), # End of condition
                
                ######################
                # Location drop-down #
                ######################
                div(uiOutput("locationsAvgMetrics"), style = "margin-right:10px"),
                
                ####################
                # Models drop-down #
                ####################
                div(uiOutput("modelAvgMetrics"), style = "margin-right:10px"),
                
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
                  
                  # Download Button
                  div(style = "margin-right: 10px",
                      downloadButton("download_metrics", "Download Crude Metrics"))
                  
                ), # End of condition
                
                ##############################################
                # Condition: Show Download Button for Figure #
                ##############################################
                conditionalPanel(
                  
                  # Condition
                  condition = "input.crudeFigure",
                  
                  # Download Button
                  div(style = "margin-right: 10px",
                      actionButton("download_metricsFig", "Download Crude Metrics Figure", icon = icon("download")))
                  
                ), # End of condition
                
                ######################
                # Location drop-down #
                ######################
                div(uiOutput("locationsMetrics"), style = "margin-right:10px"),
                
                ####################
                # Models drop-down #
                ####################
                div(uiOutput("modelMetrics"), style = "margin-right:10px"),
                
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
# Box 3 and 4: Winkler Scores and Skill Score ----------------------------------
#------------------------------------------------------------------------------#
fluidRow(
  
  #######################################
  # Creating the box for Winkler Scores #
  #######################################
  tabBox(
    
    # Box title 
    title = NULL, 
    
    # ID of the box 
    id = "otherMeasures",
    
    # Width of the box 
    width = 12,
    
    ################################
    # Rendering the Winkler Scores #
    ################################
    tabPanel(id = "winklerScores",
             
             # Title of box 
             title = "Winkler Scores", 
             
             ###############################################
             # Row 1: Rendering the Winkler Scores Figures #
             ###############################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column(
                 
                 width = 12, 
                 
                 ########################################################
                 # Conditional Panel: Rendering the Winkler Scores Data #
                 ########################################################
                 conditionalPanel(
                   
                   # Condition
                   condition = "!input.WinklerFigure",
                   
                   # Rendering the data frame
                   dataTableOutput("winklerDataTableAGGP")
                   
                 ), # End of condition for data
                 
                 ########################################################
                 # Conditional Panel: Rendering the Winkler Scores Data #
                 ########################################################
                 conditionalPanel(
                   
                   # Condition
                   condition = "input.WinklerFigure",
                   
                   # Rendering the data frame
                   plotOutput("winklerFigureAGGP")
                   
                 ), # End of condition for data
                 
                 
               ) # End of Alignment Column 
               
             ), # End of row creating the Winkler Scores figures 
             
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
                     conditionalPanel(
                       
                       # Condition
                       condition = "!input.WinklerFigure",
                       
                       # Creating the download button - Data
                       div(downloadButton("downloadWinkerData", "Download Scores", style = "margin-right: 10px"))
                       
                     ),
                     
                     #########################################
                     # Creating the download button - Figure #
                     #########################################
                     conditionalPanel(
                       
                       # Condition
                       condition = "input.WinklerFigure",
                       
                       # Creating the download button - Data
                       div(actionButton("downloadWinklerAGGP", "Download Figures", style = "margin-right: 10px"))
                       
                     ),
                     
                     ###################################
                     # Creating the location drop-down #
                     ###################################
                     div(uiOutput("locationsWinklerMain"), style = "margin-right: 10px"),
                     
                     ################################
                     # Creating the model drop-down #
                     ################################
                     div(uiOutput("modelsWinklerMain"),  style = "margin-right: 10px"),
                     
                     #######################################
                     # Creating the check-mark for figures #
                     #######################################
                     div(checkboxInput("WinklerFigure", "Show Figure"))
                     
                 ) # End of style
                 
               ) # End of alignment column 
               
             ) # End of options row
             
    ), # End of Winkler Scores Box
    
    ################################
    # Rendering the Winkler Scores #
    ################################
    tabPanel(id = "skillScores",
             
             # Title of box 
             title = "Skill Scores",
             
             #############################################
             # Row 1: Rendering the skill Scores Figures #
             #############################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column(
                 
                 width = 12, 
                 
                 #############################
                 # Check for average metrics #
                 #############################
                 div(checkboxInput("avgSSOptions", "Use Average Metrics"), 
                     style = "margin-right: 10px;justify-content: flex-end")
                 
               )
               
             ), # End of Row 1
             
             #############################################
             # Row 2: Rendering the skill Scores Figures #
             #############################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column(
                 
                 width = 12, 
                 
                 ######################################################
                 # Conditional Panel: Rendering the Skill Scores Data #
                 ######################################################
                 conditionalPanel(
                   
                   # Condition
                   condition = "!input.SSFig",
                   
                   # Rendering the data frame
                   dataTableOutput("skillScoresAGGPData")
                   
                 ), # End of condition for data
                 
                 ########################################################
                 # Conditional Panel: Rendering the Skill Scores Figure #
                 ########################################################
                 conditionalPanel(
                   
                   # Condition
                   condition = "input.SSFig",
                   
                   # Rendering the data frame
                   plotOutput("SSFigureAGGP")
                   
                 ), # End of condition for data
                 
               ) # End of Alignment Column 
               
             ), # End of row creating the Winkler Scores figures 
             
             ####################################
             # Row 3: Creating the user options #
             ####################################
             fluidRow(
               
               ####################
               # Alignment column #
               ####################
               column( 
                 
                 # Column width 
                 width = 11, 
                 
                 # Overall style for row 
                 div(style = "display:flex; vertical-aline: top",
                     
                     #######################################
                     # Creating the download button - Data #
                     #######################################
                     conditionalPanel(
                       
                       # Condition
                       condition = "!input.SSFig",
                       
                       # Creating the download button - Data
                       div(downloadButton("downloadSSData", "Download Scores", style = "margin-right: 10px"))
                       
                     ),
                     
                     #########################################
                     # Creating the download button - Figure #
                     #########################################
                     conditionalPanel(
                       
                       # Condition
                       condition = "input.SSFig",
                       
                       # Creating the download button - Data
                       #div(actionButton("downloadSSAGGP", "Download Figures", style = "margin-right: 10px"))
                       
                     ),
                     
                     ###################################
                     # Creating the location drop-down #
                     ###################################
                     div(uiOutput("locationsSSMain"), style = "margin-right: 10px"),
                     
                     ################################
                     # Creating the model drop-down #
                     ################################
                     div(uiOutput("BenchmarkSSMain"),  style = "margin-right: 10px"),
                     
                     ###########################################
                     # Creating the comparison model drop-down #
                     ###########################################
                     div(uiOutput("ComparisonSSMain"),  style = "margin-right: 10px"),
                     
                     #######################################
                     # Creating the check-mark for figures #
                     #######################################
                     div(checkboxInput("SSFig", "Show Figure"))
                     
                 ) # End of style
                 
               ), # End of column 
               
               ##################################################
               # Conditional Panel: Creating arrows if crude SS #
               ##################################################
               
               # Alignment column
               column(
                 
                 # Width 
                 width = 1,
                 
                 # Conditional panel 
                 conditionalPanel(
                   
                   # Condition
                   condition = "!input.avgSSOptions & input.SSFig",
                   
                   # Creating the buttons 
                   div(style = "display: flex; justify-content: flex-end; align-items: center;",
                       actionButton(inputId = "PreviousSS", label = icon("arrow-left")),
                       actionButton(inputId = "NextSS", label = icon("arrow-right")))
                   
                 ), # End of condition
                 
               ) # End of alignment column 
               
             ) # End of options row
             
    ) # End of tab box
    
  ) # End of "Tabbox"
  
) # End of fluidRow


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
                      
                      ################################
                      # Creating the download button #
                      ################################
                      div(
                        
                        style = "display: flex; justify-content: flex-start; align-items: center;",
                        actionButton("downloadOtherForecastsFigs", "Download Figures", style = "margin-right: 10px")
                      
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
                     div(
                       
                       style = "display: flex; justify-content: flex-start; align-items: center;",
                       actionButton("downloadOtherForecastsPanels", "Download Figures", style = "margin-right: 10px")
                       
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
    )
    )
    
  
  

  

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
# Reading in the data frame ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section reads in the user-selected data frame from the  #
# working directory. It then saves the data under the reactive element 'file'. #   
# The file is selected using the 'fileInput' picker.                           #
#------------------------------------------------------------------------------#
  
  file <- reactive({
    
    tryCatch({
    
    ############################################
    # Name of file from the 'fileInput' picker #
    ############################################
    file1 <- input$dataset
    
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
    
    ##################################################
    # If-Else Statement to determined what to render #
    ##################################################
    if(input$timeseriesCheckBox){
      
      ########################################
      # Creating the button to download data #
      ########################################
      output$downloadTimeseries <- renderUI({
        
        # Download button 
        downloadButton("download_timeseries", "Download Timeseries Data")
        
      }) # End of render UI
      
    #######################################################
    # Returning Null (no button) if check-mark is not hit #
    #######################################################
    }else{
      
      ######################################
      # Making the download button go away #
      ######################################
      output$downloadTimeseries <- 
        
        # Download button 
        NULL
        
       # End of render UI()
      
    } # End of else 
    
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
      data1 <- file()
      
      #############################################################
      # Filtering the data shown based on user selected locations #
      #############################################################

      if(!is.null(data1)){
        
          data.for.table <- data1 %>%
            dplyr::select(names(data1)[1], all_of(input$locations))
          
      }else{
        
        data.for.table <- NULL
      }

        ############################################
        # Saving the results to the reactive value #
        ############################################
        timeseriesData$dataList <- data.for.table

    ###############################
    # Error portion of 'tryCatch' #
    ###############################
    }, error = function(e){

      NULL

    }) # End of 'tryCatch'

  }) # End of 'observe'

#------------------------------------------------------------------------------#
# Rendering the data table for the timeseries ----------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the data table if the 'check-mark' is hit to     #
# show the data frame.                                                         #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    ##############################
    # Running if no errors occur #
    ##############################
    tryCatch({
      
      #################################
      # Running if "Run" has been hit #
      #################################
      observeEvent(input$run, {
        
        # Isolating the behavior
        isolate({
          
          # Data to render 
          data1 <-  timeseriesData$dataList
          
        })
        
        #########################################
        # Running if the check has been clicked #
        #########################################
        observeEvent(input$timeseriesCheckBox, {
          
          ###########################################
          # Rendering the data if the check is true #
          ###########################################
          if(input$timeseriesCheckBox){
            
            # Isolating behaviors until the button is clicked
            isolate({
              
              # Render statement
              output$timeseries <- renderDataTable({data1})
              
            }) # End of 'isolate'
            
            ######################################
            # Rendering NULL if check is not hit #
            ######################################
          }else{
            
            # Isolating behaviors
            isolate({
              
              # Render statement
              output$timeseries <- renderDataTable({NULL})
              
            }) # End of isolate
            
          } # End of NULL rendering
          
        }) # End of "check" observe event
        
      }) # End of "run" observe event 
      
    ###########################################
    # Returns NULL the above code can not run #
    ###########################################
    }, error = function(e){
      
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'observe'
  
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
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editLegendLabels, {
    
    showModal(modalDialog(
      title = "Coming Soon!",
     
    ))
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
                                                     forecastDatesEnd = input$forecast.period[2]) # End of slider
      
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
      
      #################################
      # Running if "Run" has been hit #
      #################################
      observeEvent(input$run | input$forecastLines, {
        
        # Isolating the behavior
        isolate({
          
          # Data to render 
          TimeseriesInteractive <-  timeseriesFigureList$figureInteractive
          
        })
        
          ################################################
          # Rendering a NULL object if the check is true #
          ################################################
          if(length(TimeseriesInteractive[[1]]) == 0 || is.null(TimeseriesInteractive[[1]]) || is.character(TimeseriesInteractive[[1]])){
            
            # Isolating behaviors until the button is clicked
            isolate({
              
              # Render statement
              output$timeseriesPlot <- NULL
              
            }) # End of 'isolate'
            
          #######################################################
          # Rendering the timeseries figure if check is not hit #
          #######################################################
          }else{
            
            # Isolating behaviors
            isolate({

              # Render statement
              output$timeseriesPlot <- renderPlotly({
                
                # Plot to return
                timeFig <- ggplotly(TimeseriesInteractive, tooltip = "text") %>%
                  config(edits = list(axisTitleText= TRUE)) 

                # Returning the figure
                return(timeFig)

                })

            }) # End of isolate
            
          } # End of NULL rendering
        
      }) # End of "run" observe event 
      
      #############################################
      # Determining what the new y-axis should be #
      #############################################
      observeEvent(event_data("plotly_relayout"), {

        # Y-axis title
        yAxisTitle <- event_data("plotly_relayout")

        print(yAxisTitle)

        # Saving the resulting character in a reactive value
        yAxisTitleObj$titleFig <- as.character(yAxisTitle[1])

        # Showing the new plot
        plotlyProxy("plot", session) %>%
          plotlyProxyInvoke("relayout", list(yaxis = list(yaxis = list(title = yAxisTitle[[1]]))))

      })
      
      ###########################################
      # Returns NULL the above code can not run #
      ###########################################
    }, error = function(e){
      
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of 'observe'

#------------------------------------------------------------------------------#
# Handling when the data set is changed ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section of code clears any existing output when the data set is  #
# changed by the user.                                                         #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive output #
  ########################################
  observe({
    
    ##############################
    # Running if no errors occur #
    ##############################
    tryCatch({
      
      # Clearing output if the data is changed 
      if(names(timeseriesData$dataList)[-1] %!in% c(input$locations) | length(input$locations) == 0){
        
        # Clearing plot output 
        output$timeseriesPlot <- NULL
        
        # Clearing timeseries data output 
        output$timeseries <- NULL
        
        } # End of "else" 
      
      ###########################
      # Running if error occurs #
      ###########################
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
      
      # Updating the figure y-axis title if needed
      if(!is.null(yAxisTitleObj$titleFig)){
        
        figure <- timeseriesFigureList$figureStatic +
          labs(y = yAxisTitleObj$titleFig)
        
      }else{
        
        figure <- timeseriesFigureList$figureStatic 
        
      }
      
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
        data_min_date <<- as.numeric(min(na.omit(as.numeric(file()[,1]))))
        
        # Latest possible date
        data_max_date <<- as.numeric(max(na.omit(as.numeric(file()[,1]))))
        
        ##########################################
        # Create the sliderInput for the UI Side #
        ##########################################
        return(sliderInput("forecast.period",
                           label = tags$span("Forecasting Date(s) ", # Input label
                                             tags$i(class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF;",
                                                    title = "The forecasting period corresponds the last week of data included in the calibration period (i.e., the week the forecast is conducted).")
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
                                                    title = "The forecasting period corresponds the last week of data included in the calibration period (i.e., the week the forecast is conducted).")
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
                         label = tags$span("Forecasting Period(s) ", # Input label
                                           tags$i(class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF;",
                                                  title = "The forecasting period corresponds the last week of data included in the calibration period (i.e., the week the forecast is conducted).")
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
                  label = tags$span("Calibration period:", # Input label
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
        label = tags$span("Calibration period:", # Input label
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
  
  ###############################################
  # Creating the UI parameter outputs - P/p Min #
  ###############################################
  output$pMin <- renderUI({
    
    ##############################################################################
    # Determines which parameter to show based on seasonality and selected model #
    ##############################################################################
    
    # Label for parameter entry 
    label <- switch(input$seasonality,
                    "1" = "p Min",
                    "P Min")
    
    # Value for parameter entry
    value <- switch(input$seasonality,
                    "1" = 0,
                    1)
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("pMin", label = label, value = value)
    
    }) # End of 'renderUI' statment 
  
  ###############################################
  # Creating the UI parameter outputs - P/p Min #
  ###############################################
  output$pMax <- renderUI({
    
    ##############################################################################
    # Determines which parameter to show based on seasonality and selected model #
    ##############################################################################
    
    # Label for parameter entry 
    label <- switch(input$seasonality,
                    "1" = "p Max",
                    "P Max")
    
    # Value for parameter entry
    value <- switch(input$seasonality,
                    "1" = 10,
                    3)
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("pMax", label = label, value = value)
    
    }) # End of 'renderUI' statement
  
  ###############################################
  # Creating the UI parameter outputs - Q/q Min #
  ###############################################
  output$qMin <- renderUI({
    
    ##############################################################################
    # Determines which parameter to show based on seasonality and selected model #
    ##############################################################################
    
    # Label for parameter entry 
    label <- switch(input$seasonality,
                    "1" = "q Min",
                    "Q Min")
    
    # Value for parameter entry
    value <- switch(input$seasonality,
                    "1" = 0,
                    1)
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("qMin", label = label, value = value)
    
    }) # End of 'renderUI' statement
  
  ###############################################
  # Creating the UI parameter outputs - Q/q Min #
  ###############################################
  output$qMax <- renderUI({
    
    ##############################################################################
    # Determines which parameter to show based on seasonality and selected model #
    ##############################################################################
    
    # Label for parameter entry 
    label <- switch(input$seasonality,
                    "1" = "q Max",
                    "Q Max")
    
    # Value for parameter entry
    value <- switch(input$seasonality,
                    "1" = 5,
                    3)
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("qMax", label = label, value = value)
    
    }) # End of 'renderUI' statement
  
  ###################################################
  # Creating the UI parameter outputs - Differences #
  ###################################################
  output$differences <- renderUI({
    
    ##############################################################################
    # Determines which parameter to show based on seasonality and selected model #
    ##############################################################################
    
    # Label for parameter entry 
    label <- switch(input$seasonality,
                    "1" = "Non-seasonal Differences",
                    "Seasonal Differences")
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("differences", label = label, value = 2)
    
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
                                                      input$differences), # ARIMA parameters 
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
               
               # Adding it to the reactive value
               ProphetList$prophet <- prophetList$Forecasts
               
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
# Creating the location drop down for the quantile forecast box ----------------
#------------------------------------------------------------------------------#
# About: This section creates the drop-down for locations read in from the     #
# crude data. The location drop-down is then used to filter the shown quantile #
# forecasts and what quantile forecast are saved.                              #
#------------------------------------------------------------------------------#
   
   ##########################################################
   # Creating the location drop down for quantile forecasts #
   ##########################################################
   output$locationsQuantile <- renderUI({
     
     pickerInput("locationQuantiles", # Input ID for the location drop down 
                 label = NULL, # No label for the drop down 
                 choices = c(input$locations), # Choices 
                 selected = c(input$locations), # Pre-selected choices 
                 width = "175px", # Width of drop down
                 multiple = T) # Allowing multiple options 
   })
   

   
#------------------------------------------------------------------------------#
# Creating the model drop down for the quantile forecast box -------------------
#------------------------------------------------------------------------------#
# About: This section creates the drop-down for available models. The options  #
# available here match that of what is selected by the user. The model drop    #
# down is then used to filter the shown quantile forecast and what quantile    #
# forecasts are saved.                                                         #
#------------------------------------------------------------------------------#
   
   #######################################################
   # Creating the model drop down for quantile forecasts #
   #######################################################
   output$modelQuantile <- renderUI({
     
     # Outputs if no model on the side is selected 
     if(is.null(input$modelType)){
       
       # Picker input 
       pickerInput("modelQuantiles", # Input ID for the location drop down 
                   label = NULL, # No label for the drop down
                   selected = "Please select a model", # Model type 
                   choices = "Please select a model", # Choices 
                   width = "175px", # Width of drop down 
                   multiple = T) # Allowing multiple options
       
     ####################################
     # Outputs if model(s) are selected #
     ####################################
     }else{
       
       pickerInput("modelQuantiles", # Input ID for the location drop down 
                   label = NULL, # No label for the drop down
                   selected = c(input$modelType), # Model type 
                   choices = c(input$modelType), # Choices 
                   width = "175px", # Width of drop down 
                   multiple = T) # Allowing multiple options
       
     } # End of 'else'
     
   }) # End of render 
   

#------------------------------------------------------------------------------#
# Forming the quantile forecasts -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section formats the quantile forecasts that are shown on the     #
# main dashboard page.                                                         #
#------------------------------------------------------------------------------#
   
   #########################################
   # Creating a vector for reactive values #
   #########################################
   quantileListToShow <- reactiveValues()
   
   ##################################################
   # Observing changes in reactive and input values #
   ##################################################
   observe({
     
       ############################################
       # Runs if data is entered in the dashboard #
       ############################################
       tryCatch({
         
         ###########################
         # Quantile forecasts list #
         ###########################
         quantile.forecast.loop <- c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet)
         
         # Empty list for filtered forecasts 
         showList <- list()
         
         ##############################################
         # Looping through list of quantile forecasts #
         ##############################################
         for(i in 1:length(quantile.forecast.loop)){
           
           # Pulling the name of the indexed quantile forecast
           nameForecast <- names(quantile.forecast.loop[i])
           
           # Model name
           modelNames <- strsplit(nameForecast, "[-]")[[1]][1]
           
           ##########################################################
           # Skipping to the next loop if the model is not selected #
           ##########################################################
           if(modelNames %!in% input$modelQuantiles){
             
             # Returning an NA
             showList[[i]] <- NA
             
             # Skipping to the next loop iteration
             next
             
           }
           
           # Sub-setting location/group name 
           locationGroupNames <- strsplit(nameForecast, "[-]")[[1]][2]
           
           # Extracting the data frame
           data <- quantile.forecast.loop[[i]]
           
           # Changing the name of the first column
           colnames(data)[1] <- "median"
           
           ######################################################
           # Determining if it should be added to the show list #
           ######################################################
           if(locationGroupNames %in% c(input$locationQuantiles)){
             
             # Saving the data in the list
             showList[[i]] <- data 
             
             # Renaming the data
             names(showList)[[i]] <- nameForecast
             
           #######################################################
           # If the list element is not selected, an NA is added #
           #######################################################
           }else{
             
             # Returning an NA
             showList[[i]] <- NA
             
             # Skipping to the next loop iteration
             next
             
           } # End of location filtering 
           
         } # End of loop going through quantile forecasts 
         
         #########################################################
         # Isolating the behavior to only when the button is hit #
         #########################################################
         isolate({
           
           ##########################################
           # Final list to show in the quantile box #
           ##########################################
           finalList <- showList[!is.na(showList)]
           
           # Saving it in a reactive value
           quantileListToShow$listQuantiles <- finalList
           
         })
         
       ##################################################
       # Runs if no information is entered in dashboard #
       ##################################################
       }, error = function(e){
         
         # Returning a NULL
         NULL
         
       }) # End of 'TRYCATCH' statement 
     
   }) # End of 'observe' statement 
   
   
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
       if (current_index() < length(quantileListToShow$listQuantiles)) {
         
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
# Handling when the data set is changed ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section of code clears any existing output when the data set is  #
# changed by the user.                                                         #
#------------------------------------------------------------------------------#
   
   ########################################
   # Observing changes in reactive output #
   ########################################
   observe({
     
     ##############################
     # Running if no errors occur #
     ##############################
     tryCatch({
       
       # Clearing output if the data is changed 
       if(names(timeseriesData$dataList)[-1] %!in% c(input$locations) | length(input$locations) == 0){
         
         current_index(1)
         
       } # End of "else" 
       
       ###########################
       # Running if error occurs #
       ###########################
     }, error = function(e){
       
       NULL
       
     }) # End of 'tryCatch'
     
   }) # End of 'observe'
     
   #######################################################
   # Rendering the current quantile data frame box title #
   #######################################################
   output$quantileTitle <- renderText({
     
     # Producing nothing if no locations are chosen
     if(length(input$locationQuantiles) == 0 || length(input$modelQuantiles) == 0 | length(input$locations) == 0){
       
       return(NULL)
       
       # Runs if at least one location is selected
     }else{
       
       # Rendering the data table box title 
       return(paste0(names(quantileListToShow$listQuantiles[current_index()])))
       
     }
     
   }) # End of render statement for quantile forecasts
   
   
   #############################################
   # Rendering the current quantile data frame #
   #############################################
   output$quantileForecasts <- renderDataTable({
     
     # Producing nothing if no locations are chosen
     if(length(input$locationQuantiles) == 0 || length(input$modelQuantiles) == 0 || length(input$locations) == 0){
       
       return(NULL)
       
       # Runs if at least one location or model is selected 
     }else{

       # Rendering the data table 
       return(datatable(quantileListToShow$listQuantiles[[current_index()]],
                        options = list(scrollX = T))) # Restricts the size of the box 
     }
     
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
       for (plot_name in names(quantileListToShow$listQuantiles)) {
         
         # Plot 
         plot_obj <- data.frame(quantileListToShow$listQuantiles[[plot_name]])
         
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
# Creating the location drop down for the forecast figures/data box ------------
#------------------------------------------------------------------------------#
# About: This section creates the location/group drop down for the forecast    #
# figures. Users can select which groups/locations they would like to see      #
# forecasts for. The selected forecasts/figures are then included in the final #
# '.zip' file which can be downloaded by the user.                             #
#------------------------------------------------------------------------------#
   
   ####################################
   # Creating the locations drop down #
   ####################################
   output$locationsForecastFigs <- renderUI({
     
     pickerInput("locationFigures", # ID
                 label = NULL, # No label
                 choices = c(input$locations), # Choices 
                 selected = c(input$locations), # Selected automatically
                 multiple = T, # Allowing multiple options 
                 width = "175px") # Width of picker 
     
   }) # End of render 'UI'
   
   
#------------------------------------------------------------------------------#
# Creating the model drop down for the forecast figures/data box ---------------
#------------------------------------------------------------------------------#
# About: This section creates the drop-down for available models. The options  #
# available here match that of what is selected by the user. The model drop    #
# down is then used to filter the shown formatted forecast/figures and what is #
# saved in the '.zip' file.                                                    #
#------------------------------------------------------------------------------#
   
   ##########################################################
   # Creating the model drop down for forecast figures/data #
   ##########################################################
   output$modelsForecastFigs <- renderUI({
     
     # Outputs if no model on the side is selected 
     if(is.null(input$modelType)){
       
       # Picker input 
       pickerInput("modelsForecastFigs", # Input ID for the location drop down 
                   label = NULL, # No label for the drop down
                   selected = "Please select a model", # Model type 
                   choices = "Please select a model", # Choices 
                   width = "175px", # Width of picker 
                   multiple = T) # Allowing multiple options
       
     ####################################
     # Outputs if model(s) are selected #
     ####################################
     }else{
       
       pickerInput("modelsForecastFigs", # Input ID for the location drop down 
                   label = NULL, # No label for the drop down
                   selected = c(input$modelType), # Model type 
                   choices = c(input$modelType), # Choices 
                   width = "175px", # Width of picker 
                   multiple = T) # Allowing multiple options
       
     } # End of 'if-else' creating the picker 
     
   }) # End of render 'UI'
   
   
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
                                                                    horizon.input = input$forecastHorizon) # Horizon 
               
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
# Preparing the individual forecast files for rendering ------------------------
#------------------------------------------------------------------------------#
# About: This section filters the list of formatted forecasts to show what the #
# user selects in the drop down for the formatted forecasts.                   #
#------------------------------------------------------------------------------#
   
   ##############################################################
   # Initialize reactiveValues to store the formatted forecasts #
   ##############################################################
   FormattedForecastListToShow <- reactiveValues()
   
   ##############################################
   # Observing changes in the reactive elements #
   ##############################################
   observe({
     
     ###########################
     # Runs if data is entered #
     ###########################
     tryCatch({
       
       ###########################################
       # Calling the list of formatted forecasts #
       ###########################################
       indexedFForcasted <- foremattedForecasts$forecasts
       
       ######################
       # Preparing for loop #
       ######################
       
       # Empty list for filtered forecasts 
       showList <- list()
       
       ###############################################
       # Looping through list of formatted forecasts #
       ###############################################
       for(i in 1:length(indexedFForcasted)){
         
         # Pulling the name of the indexed formatted forecast
         nameForecast <- names(indexedFForcasted[i])
         
         # Sub-setting location/group name
         locationGroupNames <- strsplit(nameForecast, "[-]")[[1]][2]
         
         # Sub-setting location/group name
         modelNames <- strsplit(nameForecast, "[-]")[[1]][1]
         
         ##########################################################
         # Skipping to the next loop if the model is not selected #
         ##########################################################
         if(modelNames %!in% input$modelsForecastFigs){
           
           # Returning an NA
           showList[[i]] <- NA

           # Skipping to the next loop iteration
           next
           
         }
         
         # Extracting the data frame
         data <- indexedFForcasted[[i]]
         
         # Determining if it should be added to the show list
         if(locationGroupNames %in% c(input$locationFigures)){
           
           # Saving the data in the list
           showList[[i]] <- data
           
           # Renaming the data
           names(showList)[[i]] <- nameForecast
           
         }else{
           
           # Returning an NA
           showList[[i]] <- NA
           
         } # End of 'if-else' for filtering locations
         
       } # End of loop going through formatted forecasts 
       
       # Final list to show
       finalList <- showList[!is.na(showList)]
       
       # Saving it in a reactive value
       FormattedForecastListToShow$listFormatted <- finalList
       
     ##################################
     # Runs if no inputs are selected #
     ##################################
     }, error = function(e){
       
       # Returning a NULL
       NULL
       
     }) # End of 'tryCatch' statement 
     
   }) # End of observed statement 
   

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
       if (current_index_formatted() < length(FormattedForecastListToShow$listFormatted) & current_index_formatted() < length(FiguresForecastListToShow$listFormatted)) {
         
         # Changing the index of the reactive value
         current_index_formatted(min(current_index_formatted() + 1))
         
       }
       
     }) # End of 'isoalte' statement
     
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
     if(length(input$locationFigures) == 0 || length(input$modelsForecastFigs) == 0){
       
       # Returning NULL
       return(NULL)
       
     # Runs if at least one location or model is selected
     }else{
       
       # Rendering the title of the formatted forecast box 
       return(paste0(names(FormattedForecastListToShow$listFormatted[current_index_formatted()])))
       
     } # End of 'if-else' creating the title 
     
   }) # End of render statement for quantile forecasts
   
   
   #######################################################
   # Rendering the current formatted forecast data frame #
   #######################################################
   output$Formatted.Forecast <- renderDataTable({
     
     # Producing nothing if no locations or models are chosen
     if(length(input$locationFigures) == 0 || length(input$modelsForecastFigs) == 0){
       
       # Returning a NULL
       return(datatable(NULL))
       
     # Runs if at least one location or model is selected
     }else{
       
       # Rendering the data table
       return(datatable(FormattedForecastListToShow$listFormatted[[current_index_formatted()]],
                        options = list(scrollX = T)))
       
     } # End of 'if-else'
     
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
       for (plot_name in names(FormattedForecastListToShow$listFormatted)) {
         
         # Plot 
         plot_obj <- FormattedForecastListToShow$listFormatted[[plot_name]]
         
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
           
           ###############################################
           # Function to create list of forecast figures #
           ###############################################
           
           # 'Isolate' statement 
           isolate({
             
             ######################
             # Individual figures #
             ######################
             individual <- forecast.figures(formatted.forecast.input = foremattedForecasts$forecasts, 
                                            data.type.input = dateValues$dates)
             
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
             
             
           }) # End of 'isolate' statement 
           
         #######################################
         # Runs when no information is entered #
         #######################################
         }, error = function(e){
           
           # Returns a NULL
           NULL
           
         }) # End of 'tryCatch' statement 
         
       }) # End of 'isolate' statement 
       
     }) # End of 'observeEvent' statement 
     
   }) # End of 'observe' statement 
   
   
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
           
           #####################################################
           # Function to create list of forecast figure panels #
           #####################################################
           
           # 'Isolate' statement 
           isolate({
             
             #################
             # Panel figures #
             #################
             panelOutput <- panel.forecast.figures(formatted.forecast.input = foremattedForecasts$forecasts, # Formatted figures
                                                   data.type.input = dateValues$dates) # Date type 
             
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
         
       }) # End of 'isolate' statement 
       
     }) # End of 'observeEvent' statement 
     
   }) # End of 'observe' statement 
   
   
#------------------------------------------------------------------------------#
# Preparing the individual/panel forecast figures for rendering ----------------
#------------------------------------------------------------------------------#
# About: This section prepares the lists of individual and panel forecast      #
# figures that are later rendered in the dashboard. Additionally, this section #
# takes in the user inputs for locations and models for the individual         #
# forecasts to filter what figures are shown.                                  #
#------------------------------------------------------------------------------#
   
   ##############################################################
   # Initialize reactiveValues to store the formatted forecasts #
   ##############################################################
   FiguresForecastListToShow <- reactiveValues()
   
   ##############################################
   # Observing changes in the reactive elements #
   ##############################################
   observe({
     
     ###########################
     # Runs if data is entered #
     ###########################
     tryCatch({
       
       ###################################################
       # Calling the list of individual forecast figures #
       ###################################################
       if(input$panelModelsForecasts == F){
         
         # Saving the list of individual forecast figures under a new name
         indexedFigure <- figuresForecast$figure
         
         ######################
         # Preparing for loop #
         ######################
         
         # Empty list for filtered figures
         showList <- list()
         
         ############################################
         # Looping through list of forecast figures #
         ############################################
         for(i in 1:length(indexedFigure)){
           
           # Pulling the name of the indexed formatted forecast
           nameFigure <- names(indexedFigure[i])
           
           # Sub-setting location/group name
           locationGroupNames <- strsplit(nameFigure, "[-]")[[1]][2]
           
           # Sub-setting model
           modelNames <- strsplit(nameFigure, "[-]")[[1]][1]
           
           ##########################################################
           # Skipping to the next loop if the model is not selected #
           ##########################################################
           if(modelNames %!in% input$modelsForecastFigs){
             
             # Returning an NA
             showList[[i]] <- NA
             
             # Skipping to the next loop iteration
             next
             
           }
           
           # Extracting the figure
           figure <- indexedFigure[[i]]
           
           # Determining if it should be added to the show list
           if(locationGroupNames %in% c(input$locationFigures)){
             
             # Saving the data in the list
             showList[[i]] <- figure
             
             # Renaming the data
             names(showList)[i] <- nameFigure
           
           ################################
           # Runs if model is not in list #
           ################################
           }else{
             
             # Saves the list as NA
             showList[[i]] <- NA
             
           } # End of 'if-else' for producing the show list 
           
           # Final list to show of individual forecast figures 
           finalList <- showList[!is.na(showList)]
           
           } # End of loop going through forecast figures 
         
       #####################################
       # Runs if multi-panel plot is shown #
       #####################################
       }else{
         
         # Renaming the list of panel figures 
         indexedFigure <- figuresForecastPanel$figure
         
         ######################
         # Preparing for loop #
         ######################
         
         # Empty list for filtered panel figures
         showList <- list()
         
         ##################################################
         # Looping through list of panel forecast figures #
         ##################################################
         for(i in 1:length(indexedFigure)){
           
           # Pulling the name of the indexed panel figure 
           nameFigure <- names(indexedFigure[i])
           
           # Sub-setting location/group name
           locationGroupNames <- strsplit(nameFigure, "[-]")[[1]][1]
           
           # Extracting the figure
           figure <- indexedFigure[[i]]
           
           # Determining if it should be added to the show list
           if(locationGroupNames %in% c(input$locationFigures)){
             
             # Saving the data in the list
             showList[[i]] <- figure
             
             # Renaming the data
             names(showList)[i] <- nameFigure
           
           # Runs if the figure should not be added to the list  
           }else{
             
             # NA placeholder 
             showList[[i]] <- NA
             
           } # End of 'if-else' for location filtering 
           
         } # End of loop going through forecast panel figures 
         
         # Final list to show
         finalList <- showList[!is.na(showList)]
         
         } # End of 'else'
       
       #####################################################
       # Saving the correct final list in a reactive value #
       #####################################################
       FiguresForecastListToShow$listFormatted <- finalList

     ##############################
     # Runs if inputs are missing #
     ##############################
     }, error = function(e){
       
       # Returns a NULL
       NULL
       
     }) # End of 'tryCatch' statement
     
   }) # End of observed statement
  
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
       
       # Producing nothing if no locations are chosen
       if(length(input$locationFigures) == 0 || length(FiguresForecastListToShow$listFormatted) == 0){
         
         NULL
         
         # Runs if at least one location is selected
       }else{
         
         # Rendering the title
         return(names(FiguresForecastListToShow$listFormatted[current_index_formatted()]))
         
       } # End of 'if-else'
       
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
       
       # Producing nothing if no locations are chosen
       if(length(input$locationFigures) == 0 || length(FiguresForecastListToShow$listFormatted) == 0){
         
         NULL
         
       # Runs if at least one location is selected
       }else{
         
         # Rendering the figure
         return(ggplotly(FiguresForecastListToShow$listFormatted[[current_index_formatted()]], tooltip = "text"))
         
       } # End of 'if-else'
     
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
       for (plot_name in names(FiguresForecastListToShow$listFormatted)) {
         
         # Plot 
         plot_obj <- FiguresForecastListToShow$listFormatted[[plot_name]]
         
         # If plot is found 
         if (!is.null(plot_obj)) {
           
           # File name 
           file_name <- glue("{plot_name}.{input$extFig}")
           
           # TIFF file 
           if(input$extFig == ".tiff"){
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
# Creating the location drop down for the crude model metrics ------------------
#------------------------------------------------------------------------------#
# About: This section creates the location/group drop down for the crude       #
# metrics. The users can select which groups/locations they would like to see  #
# forecasts for. The selected forecasts are then included in the final '.zip'  #
# file which can be downloaded by the user.                                    #
#------------------------------------------------------------------------------#
   
   ##################################
   # Rending the location drop down #
   ##################################
   output$locationsMetrics <- renderUI({
     
     pickerInput("locationMetrics", # ID for picker 
                 label = NULL, # Removing the label 
                 choices = c(input$locations), # Choices for picker 
                 selected = c(input$locations), # Selected locations/group 
                 multiple = T, # Allowing multiple options 
                 width = "175px") # Width of picker 
     
     }) # End of 'renderUI'


#------------------------------------------------------------------------------#
# Creating the model drop down for the crude model metrics ---------------------
#------------------------------------------------------------------------------#
# About: This section creates the drop-down for available models. The options  #
# available here match that of what is selected by the user. The model drop    #
# down is then used to filter the shown model metrics and what metrics are     #
# saved.                                                                       #
#------------------------------------------------------------------------------#
   
   ##################################################
   # Creating the model drop down for crude metrics #
   ##################################################
   output$modelMetrics <- renderUI({
     
     ###############################################
     # Outputs if no model on the side is selected #
     ###############################################
     if(is.null(input$modelType)){
       
       pickerInput("modelMetrics", # Input ID for the location drop down 
                   label = NULL, # No label for the drop down
                   selected = "Please select a model", # Model type 
                   choices = "Please select a model", # Choices 
                   width = "175px", # Width of picker 
                   multiple = T) # Allowing multiple options
       
       ####################################
       # Outputs if model(s) are selected #
       ####################################
     }else{
       
       # Runs if working with model fit 
       if(input$metricsToShow == "Model Fit"){
         
         # Options to show
         optionsPicker <- c(input$modelType[input$modelType != "ARIMA"])
         
         # Runs if working with forecast metrics 
       }else{
         
         # Options to show
         optionsPicker <- c(input$modelType)
         
       }
       
       pickerInput("modelMetrics", # Input ID for the location drop down 
                   label = NULL, # No label for the drop down
                   selected = optionsPicker, # Model type 
                   choices = optionsPicker, # Choices 
                   width = "175px", # Width of picker 
                   multiple = T) # Allowing multiple options
       
     } # End of else 
     
   }) # End of 'render' 
   
 
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
         
         shinyalert("Model fit metrics are not avaliable for ARIMA models", type = "error")
         
       } # End of 'if' producing error
       
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
           
           shinyalert("Not enough data to evaluate forecasting performance." , type = "error")
           metrics <- NULL
           modelMetricsCrude$metricsList <- NULL
           modelCrudePlot$figures <- NULL
           avgMetrics$metricsList <- NULL
           modelAvgPlot$figures <- NULL
           
         }else{
         
           # Using forecast metrics 
           metrics <- forecastMetricsListCrude$forecastMetrics
           
         }
        
       }

       ##############################################################
       # Filtering the crude metrics by selected location and model #
       ##############################################################
       finalList <- metrics %>%
         dplyr::filter(Model %in% c(input$modelMetrics),
                       Location %in% c(input$locationMetrics)) 
       
       ###########################
       # Changing the file names #
       ###########################
       names(finalList) <- c("", "Model", "Date", "MSE", "MAE", "PI", "WIS")
       
       ###############################################
       # Adding the final list to the reactive value #
       ###############################################
       modelMetricsCrude$metricsList <- finalList
       
       
     ###################################
     # Runs if no information is added #
     ###################################
     }, error = function(e){
       
       # Returns a NULL
       NULL
       
     }) # End of 'tryCatch'
     
   }) # End of 'observe'


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
     
     # Producing nothing if no locations are chosen
     if(length(input$locationMetrics) == 0 || length(input$modelMetrics) == 0 || length(modelMetricsCrude$metricsList) == 0){
       
       # Returning a NULL data frame 
       return(datatable(NULL))
       
       # Runs if at least one location is selected
     }else{
       
       # Rendering the data table
       return(datatable(data.frame(modelMetricsCrude$metricsList), 
                        options = list(scrollX = T)))
       
     }
     
   }) # End of rendering the title 
   
   
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
       write.csv(data.frame(modelMetricsCrude$metricsList), file, row.names = FALSE)
       
     }
     
   ) # End of download button 
   
   
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
       metricPanelCrude <- CrudeMetricsFigure(crudeMetrics = modelMetricsCrude$metricsList, 
                                              dateType = dateValues$dates)
       
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
     
     # Producing nothing if no locations are chosen
     if(length(input$locationMetrics) == 0 || length(input$modelMetrics) == 0 || length(modelCrudePlot$figures) == 0){
       
       # Returning a NULL data frame 
       return((NULL))
       
       # Runs if at least one location is selected
     }else{
       
       # Rendering the data table
       return(names(modelCrudePlot$figures)[current_index_Metrics()])
       
     }
     
     
   }) # End of 'renderPlot'


#------------------------------------------------------------------------------#
# Rendering each of the figures ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates each of the figures shown in the main UI         #
# interface.                                                                   #
#------------------------------------------------------------------------------#
   
   output$CrudeMetricsFigure <- renderPlot({
     
     # Producing nothing if no locations are chosen
     if(length(input$locationMetrics) == 0 || length(input$modelMetrics) == 0 || length(modelCrudePlot$figures) == 0){
       
       # Returning a NULL data frame 
       return((NULL))
       
       # Runs if at least one location is selected
     }else{
       
       # Rendering the data table
       return(modelCrudePlot$figures[[current_index_Metrics()]])
       
     }
     
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
         downloadButton("downloadCrudeMetrics", "Download Crude Metrics Figure(s)"),
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
   output$downloadCrudeMetrics <- downloadHandler(
     
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
           if(input$extFig == ".tiff"){
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
# Creating the location drop down for the average model metrics ----------------
#------------------------------------------------------------------------------#
# About: This section creates the location/group drop down for the average     #
# metrics. The users can select which groups/locations they would like to see  #
# forecasts for. The selected forecasts are then included in the final '.zip'  #
# file which can be downloaded by the user.                                    #
#------------------------------------------------------------------------------#
   
   ##################################
   # Rending the location drop down #
   ##################################
   output$locationsAvgMetrics <- renderUI({
     
     pickerInput("locationsAvgMetrics", # ID for picker 
                 label = NULL, # Removing the label 
                 choices = c(input$locations), # Choices for picker 
                 selected = c(input$locations), # Selected locations/group 
                 multiple = T, # Allowing multiple options 
                 width = "175px") # Width of picker 
     
   }) # End of 'renderUI'
   
   
#------------------------------------------------------------------------------#
# Creating the model drop down for the average model metrics -------------------
#------------------------------------------------------------------------------#
# About: This section creates the drop-down for available models. The options  #
# available here match that of what is selected by the user. The model drop    #
# down is then used to filter the shown model metrics and what metrics are     #
# saved.                                                                       #
#------------------------------------------------------------------------------#
   
   ##################################################
   # Creating the model drop down for crude metrics #
   ##################################################
   output$modelAvgMetrics <- renderUI({
     
     ###############################################
     # Outputs if no model on the side is selected #
     ###############################################
     if(is.null(input$modelType)){
       
       pickerInput("modelAvgMetrics", # Input ID for the location drop down 
                   label = NULL, # No label for the drop down
                   selected = "Please select a model", # Model type 
                   choices = "Please select a model", # Choices 
                   width = "175px", # Width of picker 
                   multiple = T) # Allowing multiple options
       
     ####################################
     # Outputs if model(s) are selected #
     ####################################
     }else{
       
       # Runs if working with model fit 
       if(input$metricsToShow == "Model Fit"){
         
         # Options to show
         optionsPicker <- c(input$modelType[input$modelType != "ARIMA"])
        
       # Runs if working with forecast metrics 
       }else{
         
         # Options to show
         optionsPicker <- c(input$modelType)
         
       }
       
       pickerInput("modelAvgMetrics", # Input ID for the location drop down 
                   label = NULL, # No label for the drop down
                   selected = optionsPicker, # Model type 
                   choices = optionsPicker, # Choices 
                   width = "175px", # Width of picker 
                   multiple = T) # Allowing multiple options
       
     } # End of else 
     
   }) # End of 'render' 
   
   
#------------------------------------------------------------------------------#
# Calculating average metrics --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the average metrics over forecast periods.    #
# Therefore, each location and model has a set of metrics assigned to it.      #
#------------------------------------------------------------------------------#
   
   ######################################
   # Reactive value for average metrics #
   ######################################
   avgMetrics <- reactiveValues()
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
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
       ######################################
       }else{
         
         # Using forecast metrics
         metrics <- forecastMetricsListCrude$forecastMetrics
         
       }
       
       #####################################################
       # Running the function to calculate average metrics #
       #####################################################
       averageMetrics <- metrics %>%
         dplyr::filter(Location %in% c(input$locationsAvgMetrics), # Filtering locations
                       Model %in% c(input$modelAvgMetrics)) %>% # Filtering models 
         dplyr::group_by(Model, Location) %>% # Grouping by location and model
         dplyr::mutate(avgMSE = mean(meanMSE), # Avg. MSE
                       avgMAE = mean(meanMAE), # Avg. MAE
                       avgPI = mean(mean95PI), # Avg. PI
                       avgWIS = mean(meanWIS)) %>% # Avg. WIS
         dplyr::select(Model, Location, avgMSE, avgMAE, avgWIS, avgPI) %>% # Selecting needed variables
         distinct(Model, Location, .keep_all = T) # Remove duplicate rows


       # Adding the metrics to the reactive value
       avgMetrics$metricsList <- averageMetrics
       
     ###################################
     # Runs if inputs are not selected #
     ###################################
     }, error = function(e){
       
       # Null
       NULL
       
     }) # End of 'tryCatch'
     
   }) # End of 'observe'

#------------------------------------------------------------------------------#
# Rendering the data frame containing the average metrics ----------------------
#------------------------------------------------------------------------------#
# About: This section creates the data frame shown in the user UI.             #
#------------------------------------------------------------------------------#
   
   ##########################
   # Rending the data table #
   ##########################
   output$AvgMetricsData <- renderDataTable({
     
     # Producing nothing if no locations are chosen
     if(length(input$locationsAvgMetrics) == 0 || length(input$modelAvgMetrics) == 0 || length(avgMetrics$metricsList) == 0){
       
       # Returning a NULL data frame
       return(datatable(NULL))
       
       # Runs if at least one location is selected
     }else{
       
       # Rendering the data table
       return(datatable(as.data.frame(avgMetrics$metricsList),
                        options = list(scrollX = T)))
       
     }
     
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
         write.csv(data.frame(avgMetrics$metricsList), file, row.names = FALSE)
         
       }
       
     ) # End of download button 

#------------------------------------------------------------------------------#
# Creating the timeseries plot for average metrics -----------------------------
#------------------------------------------------------------------------------#
# About: This section plots the average metrics over time as a timeseris plot. #
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
       metricPanelAverage <- AverageMetricsPanel(avgMetrics.input = avgMetrics$metricsList,
                                                 dateType.input = dateValues$dates)

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

     # Producing nothing if no locations are chosen
     if(length(modelAvgPlot$figures) == 0){

       # Returning a NULL data frame
       return((NULL))

     # Runs if at least one location is selected
     }else{

       # Rendering the data table
       return((modelAvgPlot$figures))

     }

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
# Location Drop-Down for Winkler Scores ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the locations drop-down for filtering the        #
# Winker scores by location. This filter applies both to the data and figure   #
# boxes shown in the main dashboard.                                           #
#------------------------------------------------------------------------------#
   
   ##################################
   # Rending the location drop down #
   ##################################
   output$locationsWinklerMain <- renderUI({
     
     pickerInput("locationsWinklerMain", # ID for picker 
                 label = NULL, # Removing the label 
                 choices = c(input$locations), # Choices for picker 
                 selected = c(input$locations), # Selected locations/group 
                 multiple = T, # Allowing multiple options 
                 width = "175px") # Width of picker 
     
   }) # End of 'renderUI'
   
#------------------------------------------------------------------------------#
# Creating the model drop down for Winkler Scores ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the models drop-down for filtering the Winkler   #
# scores by model. This filter applied both to the data and figure boxed shown #
# in the main dashboard.                                                       #
#------------------------------------------------------------------------------#
   
   ##################################################
   # Creating the model drop down for crude metrics #
   ##################################################
   output$modelsWinklerMain <- renderUI({
     
     ###############################################
     # Outputs if no model on the side is selected #
     ###############################################
     if(is.null(input$modelType)){
       
       pickerInput("modelsWinklerMain", # Input ID for the model drop down 
                   label = NULL, # No label for the drop down
                   selected = "Please select a model", # Model type 
                   choices = "Please select a model", # Choices 
                   width = "175px", # Width of picker 
                   multiple = T) # Allowing multiple options
       
     ####################################
     # Outputs if model(s) are selected #
     ####################################
     }else{
       
       # Runs if working with model fit 
       if(input$metricsToShow == "Model Fit"){
         
         # Options to show
         optionsPicker <- c(input$modelType[input$modelType != "ARIMA"])
         
         # Runs if working with forecast metrics 
         }else{
         
         # Options to show
         optionsPicker <- c(input$modelType)
         
         }
       
       ##############################
       # Rendering the picker input #
       ##############################
       pickerInput("modelsWinklerMain", # Input ID for the model drop down 
                   label = NULL, # No label for the drop down
                   selected = optionsPicker, # Model type 
                   choices = optionsPicker, # Choices 
                   width = "175px", # Width of picker 
                   multiple = T) # Allowing multiple options
       
     } # End of else 
     
   }) # End of 'render' 
   
   
#------------------------------------------------------------------------------#
# Winkler Scores ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates Winkler scores for the produced ARIMA, GLM,   #
# GAM, Prophet models. The Winkler scores provide a quantitative measure of    #
# comparison for prediction interval coverage.                                 #
#------------------------------------------------------------------------------#
   
   ######################################################
   # Creating a reactive value to save winkler score in #
   ######################################################
   winklerScoresAGGP <- reactiveValues()
   
   ################################################
   # Observing changes in the formatted forecasts #
   ################################################
   observe({
     
     ########################################
     # Trying to run - If data is available #
     ########################################
     tryCatch({
       
       #######################################
       # Running the winkler scores function #
       #######################################
       winkler.scores.output <- winkler.scores.AGGP(formattedForecasts = foremattedForecasts$forecasts)
       
       # Saving the output to a reactive value
       winklerScoresAGGP$scores <- winkler.scores.output
     
     ###################################
     # Returns if the code can not run #
     ###################################
     }, error = function(e){
       
       # Returning a NULL
       NULL
       
     }) # End of 'tryCatch'
     
   }) # End of 'observe'
   
#------------------------------------------------------------------------------#
# Filtering the Winkler scores based on user input -----------------------------
#------------------------------------------------------------------------------#
# About: This section filters the Winkler Scores based on the user inputs for  #
# locations and models. Additionally, it removes not-needed variables and      #
# prepares the final data for export to the UI.                                #
#------------------------------------------------------------------------------#
   
   #########################################################
   # Reactive value to save the filtered winkler scores in #
   #########################################################
   winklerFinalAGGP <- reactiveValues()

   ##########################################
   # Filtering the data based on user input #
   ##########################################
   observe({

     # Reading in the list of Winkler scores
     winklerScores <- winklerScoresAGGP$scores

     # Runs if a location is selected
     if(length(input$locationsWinklerMain) == 0 | is.null(input$locationsWinklerMain) | 
        length(input$modelsWinklerMain) == 0 | is.null(input$modelsWinklerMain)){
       
       # Returning NULL 
       winklerFinalAGGP$scores <- NULL

     }else{
       
       # Filtering the data 
       winklerFiltered <- winklerScores %>%
         dplyr::filter(Location %in% c(input$locationsWinklerMain)) %>% # Filtering locations
         dplyr::filter(Model %in% c(input$modelsWinklerMain)) # Filtering models 
       
       # Data to show
       winklerFinalAGGP$scores <- winklerFiltered
       
     }
     
   }) # End of "observe"
   
#------------------------------------------------------------------------------#
# Rendering the Winkler Scores data frame --------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes in the winkler scores calculated above and returns #
# them to the main UI as a data table.                                         #
#------------------------------------------------------------------------------#
   
   #########################################
   # Reactive value for final data to save #
   #########################################
   winklerSAVE <- reactiveValues()
   
   ###########################################
   # Rendering the Winkler Scores Data Frame #
   ###########################################
   output$winklerDataTableAGGP <- renderDataTable({
     
     ######################################################
     # Returning NULL if no Winkler scores are calculated #
     ######################################################
     if(nrow(winklerScoresAGGP$scores) == 0 | is.null(winklerScoresAGGP$scores)){
       
       return(NULL)
      
     ###################################################################
     # Returning the calibration period Winkler scores when applicable #
     ###################################################################
     }else if(input$metricsToShow == "Model Fit"){
       
       # Preparing the final data set
       winklerFinal <- winklerFinalAGGP$scores %>%
         dplyr::filter(CalibrationIndicator == 1) %>% # Keeping only calibration rows 
         dplyr::select(-CalibrationIndicator, -Date, -`Winkler Score`) # Removing the not needed variable 
       
       # Saving the data to the reactive value
       winklerSAVE$final <- winklerFinal
       
       # Returning the resulting data frame 
       return(datatable(winklerFinal))
       
     #########################################################
     # Returning the forecast Winkler scores when applicable #
     #########################################################
     }else{
       
       # Preparing the final data set
       winklerFinal <- winklerFinalAGGP$scores %>%
         dplyr::filter(CalibrationIndicator == 0) %>% # Keeping only calibration rows 
         dplyr::select(-CalibrationIndicator, -Date, -`Winkler Score`) # Removing the not needed variable 
       
       # Saving the data to the reactive value
       winklerSAVE$final <- winklerFinal
       
       # Returning the resulting data frame 
       return(datatable(winklerFinal))
       
     }
     
     }) # End of Winkler scorees render
   
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
       paste("winkler-scores-", input$dataset, sep = "")
       
     },
     
     #############################
     # Function to save the file #
     #############################
     content = function(file) {
       
       # Saving the file
       write.csv(winklerSAVE$final, file, row.names = FALSE)
       
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
       winklerFigures <- winkler.figure.AGGP(scoresFigure = winklerSAVE$final, # List of winkler scores 
                                             locationWinklerFig = input$locationsWinklerMain, # Locations 
                                             modelsWinklerFig = input$modelsWinklerMain) # Models
       
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
   observeEvent(input$downloadWinklerAGGP, {
     
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
# Location Drop-Down for Skill Scores ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the locations drop-down for filtering the        #
# skill scores by location. This filter applies both to the data and figure    #
# boxes shown in the main dashboard.                                           #
#------------------------------------------------------------------------------#
   
   ##################################
   # Rending the location drop down #
   ##################################
   output$locationsSSMain <- renderUI({
     
     pickerInput("locationsSSMain", # ID for picker 
                 label = NULL, # Removing the label 
                 choices = c(input$locations), # Choices for picker 
                 selected = c(input$locations), # Selected locations/group 
                 multiple = T, # Allowing multiple options 
                 width = "175px") # Width of picker 
     
   }) # End of 'renderUI'   
   
#------------------------------------------------------------------------------#
# Creating the benchmark model drop down for skill Scores ----------------------
#------------------------------------------------------------------------------#
# About: This section creates the benchmark model drop-down for calculating    #
# skill scores for either the average or crude metrics. This filter applied    #
# both to the data and figure boxed shown in the main dashboard.               #
#------------------------------------------------------------------------------#
   
   ##################################################
   # Creating the model drop down for crude metrics #
   ##################################################
   output$BenchmarkSSMain <- renderUI({
     
     ###############################################
     # Outputs if no model on the side is selected #
     ###############################################
     if(is.null(input$modelType)){
       
       pickerInput("BenchmarkSSMain", # Input ID for the model drop down 
                   label = NULL, # Label
                   selected = "Benchmark Model:", # Model type 
                   choices = "Benchmark Model", # Choices 
                   width = "175px", # Width of picker 
                   multiple = F) # Allowing multiple options
       
     ####################################
     # Outputs if model(s) are selected #
     ####################################
     }else{
       
       # Runs if working with model fit 
       if(input$metricsToShow == "Model Fit"){
         
         # Options to show
         optionsPicker <- c(input$modelType[input$modelType != "ARIMA"])
         
       # Runs if working with forecast metrics 
       }else{
         
         # Options to show
         optionsPicker <- c(input$modelType)
         
       }
       
       ##############################
       # Rendering the picker input #
       ##############################
       pickerInput("BenchmarkSSMain", # Input ID for the model drop down 
                   label = "Benchmark Model", 
                   inline = T, 
                   selected = "Benchmark Model", # Model type 
                   choices = optionsPicker, # Choices 
                   width = "fit", # Width of picker 
                   multiple = F) # Allowing multiple options
       
     } # End of else 
     
   }) # End of 'render'     
   
#------------------------------------------------------------------------------#
# Creating the comparison model(s) drop down for skill Scores ------------------
#------------------------------------------------------------------------------#
# About: This section creates the comparison model drop-down for calculating   #
# skill scores for either the average or crude metrics. This filter applied    #
# both to the data and figure boxed shown in the main dashboard.               #
#------------------------------------------------------------------------------#
   
   ##################################################
   # Creating the model drop down for crude metrics #
   ##################################################
   output$ComparisonSSMain <- renderUI({
     
     ###############################################
     # Outputs if no model on the side is selected #
     ###############################################
     if(is.null(input$modelType)){
       
       pickerInput("ComparisonSSMain", # Input ID for the model drop down 
                   label = NULL, # Label
                   selected = "Comparison Model(s)", # Model type 
                   choices = "Comparison Model(s)", # Choices 
                   width = "175px", # Width of picker 
                   multiple = T) # Allowing multiple options
       
       ####################################
       # Outputs if model(s) are selected #
       ####################################
     }else{
       
       # Runs if working with model fit 
       if(input$metricsToShow == "Model Fit"){
         
         # Filtering out the benchmark model
         filteredLocations <- input$modelType[input$modelType != input$BenchmarkSSMain]
         
         # Options to show
         optionsPicker <- c(filteredLocations[filteredLocations != "ARIMA"])
         
         # Runs if working with forecast metrics 
       }else{
         
         # Filtering out the benchmark model
         filteredLocations <- input$modelType[input$modelType != input$BenchmarkSSMain]
         
         # Options to show
         optionsPicker <- c(filteredLocations)
         
       }
       
       ##############################
       # Rendering the picker input #
       ##############################
       pickerInput("ComparisonSSMain", # Input ID for the model drop down 
                   label = "Comparison Model(s):", 
                   selected = "Comparison Model(s)", # Model type 
                   choices = optionsPicker, # Choices 
                   width = "fit", # Width of picker 
                   inline = T, # Inline label 
                   multiple = T) # Allowing multiple options
       
     } # End of else 
     
   }) # End of 'render' 
      
         
#------------------------------------------------------------------------------#
# Producing the skill scores ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the skill scores for the average or crude     #
# metrics. The user can select the benchmark and comparison models, and also   #
# filter the scores by location.                                               #
#------------------------------------------------------------------------------#
   
   ##########################################
   # Reactive value to save skill scores in #
   ##########################################
   skillScoresAGGP <- reactiveValues()
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     ###########################
     # Runs if no errors occur #
     ###########################
     tryCatch({
       
       #####################################
       # Runs if the model fit is selected #
       #####################################
       if(input$metricsToShow == "Model Fit"){
         
         # Model fit metrics 
         CrudeMetrics <- modelFitMetricsList$fitMetrics
         
         # Winkler scores for model fit metrics 
         winklerScoresInput <- winklerScoresAGGP$scores %>%
           dplyr::filter(CalibrationIndicator == 1)
         
       ########################################
       # Runs if forecast metrics is selected #
       ########################################
       }else{
         
         # Forecast performance metrics
         CrudeMetrics <- forecastMetricsListCrude$forecastMetrics
         
         # Winkler scores for forecast metrics 
         winklerScoresInput <- winklerScoresAGGP$scores %>%
           dplyr::filter(CalibrationIndicator == 0)
  
       }
     
       ######################################
       # Function to calculate skill scores #
       ######################################
       skillScores <- skill.scores.AGGP(averageIndicator= input$avgSSOptions, 
                                        locationsFilter = input$locationsSSMain, 
                                        CrudeMetrics = CrudeMetrics,
                                        benchModel = input$BenchmarkSSMain, 
                                        compModels = input$ComparisonSSMain,
                                        winklerScoresInput = winklerScoresInput)
       
       # Saving the output to a reactive value
       skillScoresAGGP$scores <- skillScores
     
     #############################
     # Runs with an error occurs #
     #############################
     }, error = function(e){
       
       NULL
       
     }) # End of tryCatch
     
   }) # End of 'observe'
     
     
#------------------------------------------------------------------------------#
# Rendering the skill scores data ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the skill scores the to the main dashboard.      #
#------------------------------------------------------------------------------#
   
   ############################
   # Rendering the data frame #
   ############################
   output$skillScoresAGGPData <- renderDataTable({
     
     ###########################
     # Runs if no errors occur #
     ###########################
     tryCatch({
       
       # Returning the data frame
       return(datatable(skillScoresAGGP$scores))
     
     ###########################
     # Runs if an error occurs #
     ###########################
     }, error = function(e){
       
       NULL
       
     }) # End of 'tryCatch'
     
   }) # End of 'render'
   
#------------------------------------------------------------------------------#
# Downloading the skill scores as a '.csv' -------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the skill scores data as a '.csv' file to the       #
# directory of their choosing.                                                 #
#------------------------------------------------------------------------------#
   
   output$downloadSSData <- downloadHandler(
     
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
       write.csv(skillScoresAGGP$scores, file, row.names = FALSE)
       
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
       ssFigOutput <<- skill.scores.figures.AGGP(skillScores = skillScoresAGGP$scores)
       
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
# About: This section resets the current index to zero if the average metrics  #
# button is hit.                                                               #
#------------------------------------------------------------------------------#
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     # Runs if no errors occur 
     tryCatch({
       
         # Running if working with average figure
         if(length(skillScoresFigs$figList) == 1 | is.na(skillScoresFigs$figList) | is.null(skillScoresFigs$figList) | length(skillScoresFigs$figList) == 0){
           
           current_index_skillScores(1)
           
         }
       
       }, error = function(e){
       
       NULL
         
     })
     
     }) # End of observe
   
#------------------------------------------------------------------------------#
# Rendering the plot for skill scores ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the skill scores figure, either the average plot #
# or indexed crude plot.                                                       #
#------------------------------------------------------------------------------#
   
   output$SSFigureAGGP <- renderPlot({
     
     
      return(skillScoresFigs$figList[[current_index_skillScores()]])
       
     
   })
     
# Reading in additional models -------------------------------------------------
   
#------------------------------------------------------------------------------#
# Data Formatting Pop-up Message -----------------------------------------------
#------------------------------------------------------------------------------#
# 
   
   
   # observeEvent(input$my_picker, {
   # 
   #   isolate({
   #  if(input$my_picker == "Model Comparison"){
   # 
   # modalDialog(
   #   "...",
   #   title = NULL,
   #   footer = modalButton("Dismiss"),
   #   size = c("l"),
   #   easyClose = FALSE
   # )
   #    
   #  }
   # 
   #   })
   # 
   # })
   
  
#------------------------------------------------------------------------------#
# Reading in multiple files ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in multiple files unrelated to the benchmarking    #
# models. This is the background behind reading in the multiple files.         #
#------------------------------------------------------------------------------#
   
   filesOtherModels <- reactive({
     
     tryCatch({
       
       ####################################################
       # Creating an empty list to fill in with data sets #
       ####################################################
       lst <- list()
       
       ######################################
       # Looping through uploaded data sets #
       ######################################
       for (i in 1:length(input$dataset2[, 1])) {
         
         # Extract the extension of the file
         ext <- tools::file_ext(input$dataset2[i,4])
         
         ######################################################
         # Produces an error if a '.csv' file is not selected #
         ######################################################
         if (ext != "csv") {
           
           # Produced error
           showModal(modalDialog(
             title = "Error",
             paste("File", input$dataset2[i,1], "is not a '.csv' file. Please upload only '.csv' files."),
             easyClose = TRUE
           ))
           
           # Return null so the user has to re-upload
           return(NULL)
           
         }

         #######################
         # Reading in the data #
         #######################
         lst[[i]] <- read.csv(input$dataset2[i,4], header = TRUE, check.names = FALSE)
         
         # Assigning a name to the list
         names(lst)[i] <- input$dataset2[i,1]
       }

       ###################################
       # Returning a list of data frames #
       ###################################
       return(lst)
       
       }, error = function(e) {
       
       NULL
       
     })
     
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
     
     tryCatch({
       
     #######################################################
     # Function to produce the individual forecast figures #
     #######################################################
     otherIndividual <- Otherforecast.figures(formatted.forecast.input = filesOtherModels(),
                                               date.type.input = dateValues$dates)
     
     # Saving the output to the reactive value list
     individualOtherPlots$figures <- otherIndividual

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
  
  # Try to run it 
  tryCatch({
  
    output$otherModelFigure <- renderPlotly({ggplotly(individualOtherPlots$figures[[current_index_otherModels()]], tooltip = "text")})
    
    # Runs if error occurs 
    }, error = function(e){
      
      output$otherModelFigure <- NULL
      
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
          if(input$extFig == ".tiff"){
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
    
    tryCatch({

      #######################################################
      # Function to produce the individual forecast figures #
      #######################################################
      OtherpanelOutput <<- other.panel.forecast.figures(formatted.forecast.input = foremattedForecasts$forecasts, # Formatted figures
                                                       date.type.input = dateValues$dates, # Date type
                                                       formatted.forecast.Other.input = filesOtherModels()) # Read in forecasts

      # Saving the output to the reactive value list
      PanelOtherPlots$figures <- OtherpanelOutput
     
      
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
# Rendering the plotly figure --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the plotly figure that correspond to the panels  #
# of figures of both read-in and ARIMA/GLM/GAM/Prophet figures.                #
#------------------------------------------------------------------------------#
  
  ###############################
  # Rendering the plotly figure #
  ###############################
  
  # Try to produce the figure 
  tryCatch({
    
    output$otherModelPanelFigure <- renderPlot({PanelOtherPlots$figures[[current_index_Panels()]]})
    
    # Runs if error occurs 
    }, error = function(e){
    
      output$otherModelPanelFigure <- NULL
      
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
          if(input$extFig == ".tiff"){
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
  
  metricsOtherFiles <- reactive({
    
    tryCatch({
      
      ####################################################
      # Creating an empty list to fill in with data sets #
      ####################################################
      lst <- list()
      
      ######################################
      # Looping through uploaded data sets #
      ######################################
      for (i in 1:length(input$metricsOther[, 1])) {
        
        # Extract the extension of the file
        ext <- tools::file_ext(input$metricsOther[i,4])
        
        ######################################################
        # Produces an error if a '.csv' file is not selected #
        ######################################################
        if (ext != "csv") {
          
          # Produced error
          showModal(modalDialog(
            title = "Error",
            paste("File", input$metricsOther[i,1], "is not a '.csv' file. Please upload only '.csv' files."),
            easyClose = TRUE
          ))
          
          # Return null so the user has to re-upload
          return(NULL)
          
        }
        
        #######################
        # Reading in the data #
        #######################
        lst[[i]] <- read.csv(input$metricsOther[i,4], header = TRUE, check.names = FALSE)
        
        # Assigning a name to the list
        names(lst)[i] <- input$metricsOther[i,1]
      }
      
      ###################################
      # Returning a list of data frames #
      ###################################
      return(lst)
     
    ###########################
    # Runs if an error occurs #
    ###########################
    }, error = function(e) {
      
      NULL
      
    })
    
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
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    #################
    # Error checker #
    #################
    tryCatch({
      
      # Crude metrics (fit) - ARIMA, GLM, GAM, Prophet
      crudeMetricsFitAGGP <- modelFitMetricsList$fitMetrics
      
      # Crude metrics (forecast) - ARIMA, GLM, GAM, Prophet
      crudeMetricsForecastAGGP <- forecastMetricsListCrude$forecastMetrics
      
      # Other metrics
      otherMetrics <- metricsOtherFiles()
      
      # Date type 
      dateType <- dateValues$dates


    ###########################
    # Runs if an error occurs #
    ###########################
    }, error = function(e){
      
      # Returning NULL
      NULL
      
    })
    
  })
  
  
  
}


shinyApp(ui = ui, server = server)
