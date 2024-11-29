#------------------------------------------------------------------------------#
#                                                                              #
#                              StatModPredict                                  #
#                                                                              #
#------------------------------------------------------------------------------#
#                                   About                                      #                                   
#------------------------------------------------------------------------------#                                                                 
# This R-script contains the main shell for the StatModPredict toolbox. The    #
# toolbox allows users to fit and forecast using four commonly employed        #
# statistical models:                                                          #
#                                                                              #
#           (1) Auto-regressive integrate moving average (ARIMA)               #
#           (2) Generalized Linear Models (GLM)                                #
#           (3) Generalized Additive Models (GAM)                              #
#           (4) Facebook's Prophet Model (Prophet)                             #
#                                                                              #
# The toolbox provides multiple model specifications, including assumed        #
# distributions and other model-specific parameters. Additionally, using the   #
# resulting Shiny interface, users can specify parameters related to           #
# forecasting and subsequently obtain model fits, forecasting results, and     #
# performance metrics. Finally, the dashboard facilitates model-to-model       #
# comparison between within-dashboard and outside models. Users can also       #
# download associated data and customized figures to their local system.       #
#                                                                              #
# At a minimum, the users must input time-series data with a column of dates   #
# and the remaining columns corresponding to the group(s) of interest.         #
# Additional details regarding data formatting can be found on the             #
# dashboard's "About" page.                                                    #
#                                                                              #
# Please refer to the dashboard documentation for additional details regarding # 
# the implementation and methods in the toolbox.                               #
#                                                                              #
# Documentation:                                                               #
#                                                                              #
#   Bleichrodt, Amanda and Phan, Amelia and Luo, Ruiyan and Kirpich, Alexander #
#   and Chowell-Puente, Gerardo, StatModPredict: A User-Friendly R-Shiny       #
#   Interface for Fitting and Forecasting with Statistical Models              #
#   (May 30, 2024). Available at SSRN: http://dx.doi.org/10.2139/ssrn.4849702  #                                              
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
  source("AverageMetricsPanel.R")
  source("panel.forecast.figures.other.R")
  source("winkler.scores.AGGP.R") 
  source("skillScoresMain.R")
  source("filteringFormattedIndivFigs.R")
  source("filteringQuantileForecasts.R")
  source("errorReturn.R")
  source("forecast.figures.other.R") 
  source("errorReturnMetrics.R")
  source("combining.metrics.R")
  source("CrudeMetricsFigure.OTHER.R")
  source("average.compare.metrics.R")
  source("AverageMetricsPanel.other.R")
  source("Winkler.Scores.Model.Comparison.R")
  source("skillScoresOther.R")
 


#------------------------------------------------------------------------------#
#                             Needed Packages                                  #
#------------------------------------------------------------------------------#
# About: This section loads needed packages to be able to successfully use the #
# R Shiny App.                                                                 #
#------------------------------------------------------------------------------#
  pacman::p_load(MASS, shiny, shinydashboard, shinyWidgets, bslib, plotly, anytime,
                 shinyalert, shinyjs, shinybusy, editData, shinyBS, DT, stringr,
                 tidyverse, forstringr, mgcv, processx, ggpubr, forecast, 
                 prophet, zip, glue, shinyjqui, patchwork, ggplot2, zoo, gridExtra,
                 viridis, qdapRegex, RColorBrewer, chron, lubridate)
  

#------------------------------------------------------------------------------#
#                            User Interface                                    #
#------------------------------------------------------------------------------#
# About: This section creates the user interface for the Shiny Dashboard. The  #
# user interface creates the user-interactive options along with formatting    #
# the structure of the dashboard. For this particular dashboard, there are     #
# multiple pages.                                                              #
#------------------------------------------------------------------------------#
  

ui <- dashboardPage(
  
               
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
    choices = c("About", "Forecasting", "Model Metrics", "Model Comparison"),
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
# About: This section creates the data specification options: (1) Reading in   #
# the data, (2) Selecting the location, and (3) Data specifications.           #
#------------------------------------------------------------------------------#
                
  #######################################
  # Menu option for data specifications #
  #######################################
  menuItem("Data Options", 
           
           # Menu ID
           tabName = "dataOptions",
           
           # Icon for sidebar drop down 
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
# About: This section creates the user options for model fitting and           #
# forecasting. Options include the forecasting horizon, calibration, and       #
# forecasting date(s) and quantile for forecasting uncertainty.                #
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
           
           ###############################################
           # Creating the picker input to show quantiles #
           ###############################################
           pickerInput("quantileSelection", # Drop-down ID
                       label = tags$span("Prediction Interval to Show:", # Input label
                                         tags$i(class = "glyphicon glyphicon-info-sign",
                                                style = "color:#FFFFFF;",
                                                title = "Indicate the prediction interval to show.")),
                       choices = c(seq(10, 90, by = 10), 95, 98), # Adding the choices
                       selected = 95, # Pre-selected choices
                       multiple = F), # Allowing for multiple locations/groups to be selected
          
           
           # Starting with the menu open
           startExpanded = F
           
  ), # End of Menu item

#------------------------------------------------------------------------------#
# Model specifications tab -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the options for model specifications. It         #
# includes model parameters, model type, and other extra available features.   #
# Additionally, it creates the "Run" button to execute the code.               #
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
                     multiple = T, # Choosing more than one model
                     options = list(`actions-box` = TRUE)), # Creating the select-all, 
         
         ############################################
         # Conditional on Model Type Chosen - ARIMA #
         ############################################
         conditionalPanel(
           
           # Condition
           condition = "input.modelType.includes('ARIMA')",
           
           # Title of options 
           h3("ARIMA Model Settings:", style = "font-size: 15px; color: white;"),
           
           # Showing seasonal output
           checkboxInput("considerSeasonality", "Consider Seasonal Patterns"), 
 
           # Showing the seasonal trend 
           uiOutput("ARIMA.seasonality"), 
           
           # Rows of parameters
           fluidRow(
             
             ########################
             # Row 1A - p parameter #
             ########################
             splitLayout(
               
               # Width of cells
               cellWidths = c("49%", "49%"),
               
               # Min P 
               uiOutput("pMin"),
               
               # Max P
               uiOutput("pMax")
             ),
             
             #################################
             # Conditional Row - P parameter #
             #################################
             conditionalPanel(
               
               # Condition 
               condition = "input.considerSeasonality",
               
               # Row two - P parameter 
               splitLayout(
                 
                 # Width of cells
                 cellWidths = c("49%", "49%"),
                 
                 # Min P 
                 uiOutput("PMin"),
                 
                 # Max P
                 uiOutput("PMax")
               )
               
              ), 
             
             ########################
             # Row 3A - q Parameter #
             ########################
             splitLayout(
               
               # Width of cells 
               cellWidths = c("49%", "49%"),
               
               # Min Q
               uiOutput("qMin"),
               
               # Max Q
               uiOutput("qMax")
               
             ),
             
             #################################
             # Conditional Row - Q parameter #
             #################################
             conditionalPanel(
               
               # Condition 
               condition = "input.considerSeasonality",
               
               # Creating the row 
               splitLayout(
                 
                 # Width of cells 
                 cellWidths = c("49%", "49%"),
                 
                 # Min Q
                 uiOutput("QMin"),
                 
                 # Max Q
                 uiOutput("QMax")
                 
               )
               
             ), # End of Fluid Row 
             
             ########################################
             # Differences parameter - Non Seasonal #
             ########################################
             uiOutput("differences"),
             
             #################################
             # Conditional Row - D, Seasonal #
             #################################
             conditionalPanel(
               
               # Condition 
               condition = "input.considerSeasonality",
               
               # Creating the output 
               uiOutput("Seasonaldifferences")
               
             )
             
           ) # End of `fluidRow` for ARIMA parameters

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
  
    # Creating the column to host the button 
    column(
      
      # Width of the column
      width = 1, 
      
      # Creating the button - Run
      shiny::actionButton("run",
                          label = "Run Forecasts",
                          icon = icon("play", class="fa-regular fa-play"),
                          width = 170),
      
      # Creating the button - Clear Results 
      shiny::actionButton("clearResults",
                          label = "Clear Results",
                          icon = icon("trash", class="fa-regular fa-trash"),
                          width = 170)
      
      ) # End of "Run" button column 

    ) # End of first sidebar menu 

  ), # End of conditional panel for forecasting 


#------------------------------------------------------------------------------#
# Page 3: Model Metrics Side Menu ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the metrics to show drop-down option on the 3rd  #
# page of the dashboard. The metrics to show allows users to choose if they    #
# want to see the model fit or forecast metrics.                               #
#------------------------------------------------------------------------------#
  conditionalPanel(
    
    # Linking the side-bar to the model metrics page 
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
                multiple = F), # Allowing only one choice
    
    ############################################
    # Uploading an updated comparison data set #
    ############################################
    fileInput("UpdatedMetricdataset", # ID of UI input
              label = tags$span("Upload a comparison time series data file", # Shown label
                                # Creating the info circle 
                                tags$i(class = "glyphicon glyphicon-info-sign",
                                       style = "color:#FFFFFF;",
                                       title = "Upload a '.csv' file of your data.")
              ),
              
              multiple = FALSE), # End of file input
    
    ####################################################################
    # Clearing the time series and reverting back to the original data #
    ####################################################################
    checkboxInput("clearUpdateDataMetrics", label = "Use the orignal data for comparison.", value = T)
    
    ), # End of conditional panel for page 2


#------------------------------------------------------------------------------#
# Page 4: Model Comparison Side Menu -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the side-bar options available as part of the    #
# Model comparison page. Users can insert both forecast files and performance  #
# metrics files related to models not included within the dashboard.           #
#------------------------------------------------------------------------------#
  conditionalPanel(
    
    # Linking the side-bar to the model comparison page
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
              
      ) # End of side-bar menu 
      
    ) # End of conditional panel for the Model Comparison page 
                  
  ), # End of sidebar menu 


#------------------------------------------------------------------------------#                    
# UI Body ----------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the formatting for the body of the dashboard.    #
# It creates all of the spaces for user options, boxes, graphs, and data along #
# with custom CSS formatting for other aspects of the dashboard. Finally, it   #
# also hosts the text for the information page.                                #
#------------------------------------------------------------------------------#
  dashboardBody(
    
    ###############################################
    # Creating the color for the dashboard header #
    ###############################################
    tags$head(
      
      # Creating the custom header color in HTML
      tags$style(HTML("
         
          .skin-blue .main-header .logo {
            background-color: #0039A6 !important;
          }
          .skin-blue .main-header .navbar {
            background-color: #0039A6 !important;
          }
                      
                      ")
                 )
      ),
  
    #####################
    # Hiding all errors #
    #####################
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }",
               ".shiny-output-error-message { display: none; }"  # Optional: hides error messages too
    ),
    
    ###########################
    # Adding a loading circle #
    ###########################
    add_busy_spinner(spin = "fading-circle"),
             

          
#------------------------------------------------------------------------------#
# Creating Page 1: About -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides information about data set up, included models, #
# and forecast performance metrics.                                            #
#------------------------------------------------------------------------------#
 conditionalPanel(
    
   #####################################################################
   # Condition indicating the need for the 'About' page to be selected #
   #####################################################################
   condition = "input.my_picker == 'About'", 
   
#------------------------------------------------------------------------------#
# Creating Page 1A: Suggestion Citation ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section includes the suggested citation for the toolbox and the  #
# link to the associated GITHUB page.                                          #
#------------------------------------------------------------------------------#
fluidRow(
  
  # Width of row
  width = 12, 
  
  ##################################
  # Box that contains the citation #
  ##################################
  box(
    
    # Title of Box 
    title = strong("Suggested Citation: "),
    
    # Width of Box 
    width = 12,
    
    # Style for the text size
    tags$style(HTML("
      .citation-text {
        font-size: 17.5px; 
      }
    ")),
    
    # Citation text 
    div(
      class = "citation-text",
      "Bleichrodt, Amanda and Phan, Amelia and Luo, Ruiyan and Kirpich,
       Alexander and Chowell-Puente, Gerardo, StatModPredict: 
       A User-Friendly R-Shiny Interface for Fitting and Forecasting with 
       Statistical Models (May 30, 2024). 
       Available at SSRN: https://ssrn.com/abstract=4849702 or
       http://dx.doi.org/10.2139/ssrn.4849702."
      )
 
     ) # Box for suggested citation
  
  ), # End of fluidRow

#------------------------------------------------------------------------------#
# Creating Page 1B: How-Tos ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the how-tos for varying topics regarding data    #
# preparation and other options available as part of the dashboard.            #
#------------------------------------------------------------------------------#
   fluidRow(
     
     ###############################
     # Box - Select a how-to topic #
     ###############################
     box(
       
       # Title of Box 
       title = strong("Choose a topic to learn more about: "),
       
       # Width of Box 
       width = 12,
       
       # Picker Input - Topics
       pickerInput("topicsSelect",
                   choices = c("Preparing the timeseries data.",
                               "Preparing the outside forecast files.",
                               "Preparing the outside performance metrics files.",
                               "Selecting a calibration period length.",
                               "Selecting forecast date(s).",
                               "Model Specifications.",
                               "Evaluation Criteria."),
                   multiple = F
                   
       ) # End of picker input
       
     ) # End of box to select a how-to 
     
   ), # End of row with selector 
   
   ##########################################################
   # Panel if the setting up time-series option is selected #
   ##########################################################
   conditionalPanel(
     
     # Condition to run 
     condition = "input.topicsSelect == 'Preparing the timeseries data.'",
     
     #######################
     # Box for information #
     #######################
     box(
       
       # Width of box 
       width = 12, 
       
       # Box color
       style = "background-color: #f8f8f8;", 
     
       # Page containing information   
       fluidPage(

         tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                  
           "The four models in the StatModPredict toolbox require time-series data, 
            structured with one column of dates or time indexes, and subsequent 
            columns of counts. The dashboard allows users to utilize any incident data,
            provided it follows the correct formatting guidelines and is a *.csv file. 
            As multiple *.csv types are available depending on the operating system used, 
            please ensure that your files are saved as 'CSV (Comma delimited) (*.csv).'",
           
           tags$br(),
           tags$br(),
           
           "The data must be structured in a \`wide\` format, where the first column 
            corresponds to the time series dates (i.e., daily, weekly, yearly, or time
            index). The remaining columns include the counts of the event(s) of interest 
            for each location or group at each time point. Therefore, multiple locations
            or groups can be included in one file. All columns must have headers without
            dashes or quotations in their name. However, there is no restriction on 
            the file's name.",
           
           tags$br(),
           tags$br(),
           
           "When working with daily or weekly data, years must be formatted with four 
            digits (YYYY); any conventional format can be used for the month and day. 
            However, if working with yearly data, only a four-digit year can be used 
            (YYYY); for time indexes, the first row of data corresponds to a time 
            index of 1. Thus, records over time should go from the top to the bottom
            of the dataset. Once loaded, the dashboard will automatically determine 
            if the input data is daily, weekly, yearly, or time index data.", 
           
           tags$br(),
           tags$br(),
           
           ##################
           # Link for paper #
           ##################
           div(style = "text-align: center; width: 100%; margin: 0 auto;",
               
               a("For further information, click here to learn more.",
                 href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                 style = "color: black; font-weight: 600;",
                 class = "button")
               
           ) # End of row for button link
           
         ) # End of Style 
         
       ) # Page containing information 
       
     ) # Box containing information
     
   ), # End of 'conditionalPanel' for formatting the timeseries data
   
   ########################################################################
   # Panel if the preparing the outside forecast files option is selected #
   ########################################################################
   conditionalPanel(
     
     # Condition to run 
     condition = "input.topicsSelect == 'Preparing the outside forecast files.'",
     
     #######################
     # Box for information #
     #######################
     box(
       
       # Width of box 
       width = 12, 
       
       # Box color
       style = "background-color: #f8f8f8;", 
       
       # Page containing information   
       fluidPage(
         
         # Font size 
         tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                  
           "The dashboard facilitates model-to-model comparison by allowing users 
            to input multiple forecasts created by outside models. However, the 
            inputted files must follow the format described below.", 
           
            tags$br(),
            tags$br(),
           
           "Each forecast file must include a '.csv' extension and use the 
            following naming scheme: <Model Framework>-<Model>-horizon-<Horizon 
            Number>-calibration-<Calibration Size>-<Location>-<Forecast Date>. 
            Each entry in \"<>\" indicates file name elements specific to the 
            forecast of interest.  <Model Framework> refers to the overall
            framework or general model structure used, and <Model> refers to the 
            specific model indicator or name. No restrictions exist on the name
            used for <Model Framework> or <Model>.  <Horizon Number> is the length 
            of the forecasting horizon, and <Calibration Size> refers to the 
            length of the data used to calibrate the model. Regarding <Location>, 
            it must match one of the locations, including capitalization and any 
            additional symbols used in the data employed throughout the rest of 
            the dashboard. Finally, <Forecast Date> is the last data date used to 
            calibrate the model.", 
           
            tags$br(),
            tags$br(),
           
           "If working with yearly data, the <Forecast Date> needs to be a 
            four-digit year (\"YYYY\"). If working with weekly or daily data,
            <Forecast Date> needs to be in \"MM-DD-YYYY\" format. Regarding the 
            data format, the column names need to be: \"Date,\" \"data,\" \"median,\" 
            \"LB,\" and \"UB,\" and follow the order presented here.",   
           
            tags$br(),
            tags$br(),
           
           ##################
           # Link for paper #
           ##################
           div(style = "text-align: center; width: 100%; margin: 0 auto;",
               
               a("For further information, click here to learn more.",
                 href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                 style = "color: black; font-weight: 600;",
                 class = "button")
               
           ) # End of row for button link
         
         ) # End of Size style 
         
       ) # Page containing information 
       
     ) # Box containing information
     
   ), # end of conditionalPanel for formatting forecast data
   
   ###################################################################################
   # Panel if the preparing the outside performance metrics files option is selected #
   ###################################################################################
   conditionalPanel(
     
     # Condition to run 
     condition = "input.topicsSelect == 'Preparing the outside performance metrics files.'",
     
     #######################
     # Box for information #
     #######################
     box(
       
       # Width of box 
       width = 12, 
       
       # Box color
       style = "background-color: #f8f8f8;", 
       
       # Page containing information   
       fluidPage(
         
         # Font size 
         tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                  
           "The dashboard facilitates model-to-model comparison by allowing users 
            to input multiple files related to the performance metrics for model
            fits and forecasts created by outside models. However, the inputted 
            files must follow the format described below.", 
           
            tags$br(),
            tags$br(),
           
           "The file must include a .csv extension and use the following naming
            scheme: Performance-<Type>-horizon-<Horizon Number>-calibration-
            <Calibration Size>-<Location>. Each \"<>\" entry 
            indicates file name elements specific to the location, horizon, 
            and calibration length of interest. <Type> refers to the type of
            performance metrics shown in the file; either \"Fit\" for model fit 
            metrics or \"Forecast\" for forecast performance metrics. 
            <Horizon Number> is the length of the forecasting horizon, and     
            <Calibration Size> refers to the length of the data used to calibrate the model. 
            Regarding <Location>, it must match one of the locations, including 
            capitalization and any additional symbols used in the data employed 
            throughout the rest of the dashboard.",
           
            tags$br(),
            tags$br(),
           
           "Regarding the data format, the first three columns of the data must
            be labeled: \"Location\", \"Model\", and \"Date\", with each row 
            corresponding to the metrics for the given model. \"Date\" refers to 
            the associated forecast period date. When working with daily or 
            weekly data, years must be formatted with four digits (YYYY); any conventional format can be 
            used for the month and day. However, if working with yearly data, 
            only a four-digit year can be used (YYYY); for time indexes, the 
            first row of data corresponds to a time index of 1. Thus, records
            over time should go from the top to the bottom of the dataset.
            Each of the remaining columns should include the performance metrics 
            of interest. The cell should be left blank if some metrics are 
            unavailable for a given model." ,
           
           tags$br(),
           tags$br(),
           
           ##################
           # Link for paper #
           ##################
           div(style = "text-align: center; width: 100%; margin: 0 auto;",
               
               a("For further information, click here to learn more.",
                 href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                 style = "color: black; font-weight: 600;",
                 class = "button")
               
           ) # End of row for button link
         
         ) # End of style for font size 
         
       ) # Page containing information 
       
     ) # Box containing information
     
   ), # end of "conditionalPanel" for formatting the performance metrics
   
   #########################################################################
   # Panel if the selecting a calibration period length option is selected #
   #########################################################################
   conditionalPanel(
     
     # Condition to run 
     condition = "input.topicsSelect == 'Selecting a calibration period length.'",
     
     #######################
     # Box for information #
     #######################
     box(
       
       # Width of box 
       width = 12, 
       
       # Box color
       style = "background-color: #f8f8f8;", 
       
       # Page containing information   
       fluidPage(
         
         # Font size
         tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
         
           "The calibration period length corresponds to the number of time points 
           fed into the model or used to \"calibrate\" the model, ending with the 
           indexed forecast date. As the forecast date is the last date of data 
           used to calibrate the model, the length of the calibration period 
           depends on the number of time points available prior to the selected 
           forecast date. For example, let's say we have data ranging from 2008 
           through 2023. If the user chooses a forecast date of 2008, there is no
           data available before that date. Therefore, the only possible 
           calibration period length is one (i.e., the forecast date). However, 
           if the user selects 2022, the available calibration period lengths 
           range from one (i.e., the forecast date) through 16 (i.e., the start of 
           the data). If the user selects multiple forecast dates, the available
           calibration period lengths depend on the earliest forecast date chosen.
           Additionally, the start of the calibration period is not fixed to the
           first date available; rather, it depends on the length of the selected 
           calibration period. For example, if the user chooses a forecast date of 
           2022 and a calibration period length of 2, and the first date of data 
           available is 2008, the calibration period would range from 2021 through 
           2022.", 
           
           tags$br(),
           tags$br(),
           
           ##################
           # Link for paper #
           ##################
           div(style = "text-align: center; width: 100%; margin: 0 auto;",
               
               a("For further information, click here to learn more.",
                 href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                 style = "color: black; font-weight: 600;",
                 class = "button")
               
           ) # End of row for button link
           
         ) # End of style for font size 
           
       ) # Page containing information 
       
     ) # Box containing information
     
   ), # end of "conditionalPanel" for selecting a calibration period length 
   
   ################################################################
   # Panel if the selecting a forecast date(s) option is selected #
   ################################################################
   conditionalPanel(
     
     # Condition to run 
     condition = "input.topicsSelect == 'Selecting forecast date(s).'",
     
     #######################
     # Box for information #
     #######################
     box(
       
       # Width of box 
       width = 12, 
       
       # Box color
       style = "background-color: #f8f8f8;", 
       
       # Page containing information   
       fluidPage(
         
         # Font size 
         tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
         
           "The forecast date refers to the last time point of data used to 
            calibrate the model; thus, it is the date on which the forecast is 
           \"conducted.\" When completing a retrospective analysis, this is the 
           date the forecast would have been run if conducted in real-time.
           Let's say we are working with time series data that ranges from 2008 
           through 2022. If we selected a forecast date of 2022, the model would
           be calibrated with data through 2022. The forecast dates available to
           users correspond to the date column included in the original input
           data. Therefore, the forecast dates that would be available range from 
           2008 through 2022. Additionally, users can select more than one 
           forecast date at a time.", 
           
           tags$br(),
           tags$br(),
           
           ##################
           # Link for paper #
           ##################
           div(style = "text-align: center; width: 100%; margin: 0 auto;",
               
               a("For further information, click here to learn more.",
                 href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                 style = "color: black; font-weight: 600;",
                 class = "button")
               
           ) # End of row for button link
           
         ) # End of style for font size
         
       ) # Page containing information 
       
     ) # Box containing information
     
   ), # end of "conditionalPanel" for selecting a forecast date 
   
#------------------------------------------------------------------------------#
# Creating Page 1C: Model Specifications ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section, specifically, includes details related to the different #
# models available as part of the dashboard, included the math behind each     #
# model and the software associated with each. This only shows up if the       #
# user selects to learn more information about the model specifications.       #
#------------------------------------------------------------------------------#

  #################################################
  # Panel if the "Models" is selected by the user #
  #################################################
  conditionalPanel(
    
    # Condition to run 
    condition = "input.topicsSelect == 'Model Specifications.'",
    
    # Creating the row with model details
    fluidRow(
      
      #######################################
      # Box - Choosing a modeling framework #
      #######################################
      box(
        
        # Title of Box 
        title = strong("Choose a Modeling Framework to Learn More About:"),
        
        # Width of Box 
        width = 12,
        
        # Picker Input - Models 
        pickerInput("re_model",
                    choices = c("Auto-regressive integrated moving average (ARIMA)" = "re_arima", 
                                "Generalized Linear Models (GLM)" = "re_glm", 
                                "Generalized Additive Models (GAM)" = "re_gam",
                                "Facebook's Prophet Model (Prophet)" = "re_prophet"),
                    multiple = FALSE)
                
        
      ) # End of box to select a model
      
    ), # End of row with selector 
  
    #####################
    # ARIMA Information #
    #####################
    conditionalPanel(
      
      # Condition to show the panel
      condition = "input.re_model == 're_arima'",
      
      ###############
      # Row for Box #
      ###############
      fluidRow(
      
      #######################
      # Box for information #
      #######################
      box(
        
        # Width of box
        width = 12,
        
        # Box color
        style = "background-color: #f8f8f8;", 
        
        # Page containing information
        fluidPage(
          
          # Font size 
          tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
          
          # Allowing math equations to be shown in the panel 
          withMathJax(), 
          
          # Page width 
          width = 12, 
          
          ################
          # Text to show #
          ################
          
          "Autoregressive integrated moving average (ARIMA) models are a commonly employed
           approach to time series forecasting [1-12], primarily focusing on describing the
           autocorrelations with the data [13]. ARIMA models consist of three parts: (1) auto regressive models,
           (2) moving average models, and (3) integration, which aids in increasing data stability. 
           Additional details regarding the ARIMA model and its associated components can be found in [13].",
          
           tags$br(),
           tags$br(),
  
           tags$b("Non-seasonal ARIMA models (ARIMA)"),
  
           tags$br(),
           tags$br(),
  
           "Auto-regressive models of order \\(p\\) (\\(AR(p)\\)) involve using a linear-type regression of the
            most recent values of the time series against themselves. The models can capture
            various time series patterns and thus are robust to different processes of interest. The
            \\(AR(p)\\)  model is given by [13]:",
  
           tags$br(),
           tags$br(),
            
           "\\[
           y_t = c + \\phi_1 y_{t-1} + \\phi_2 y_{t-2} + \\cdots + \\phi_p y_{t-p} + \\varepsilon_t \\tag{1}
           \\]",
           
           tags$br(),
           tags$br(),
           
           "where \\(\\varepsilon_t\\) is white noise, and \\(c\\) is a constant. The parameters \\(\\phi_p\\) capture the data
            patterns, and \\(y_t\\) are the lagged values of the time series with length \\(t\\). Moving average models of order \\(q\\) (\\(MA(q)\\)) are identical to (Eq.1). However, they
            include past forecast errors within the model rather than the auto-correlated data [13]:",
           
           tags$br(),
           tags$br(),
           
           "\\[
            y_t = c + \\varepsilon_t + \\theta_1 \\varepsilon_{t-1} + \\theta_2 \\varepsilon_{t-2} + \\cdots + \\theta_q \\varepsilon_{t-q} \\tag{2}
           \\]",
           
           tags$br(),
           tags$br(),
           
           "Similar to Eq. 1, \\(\\varepsilon_t\\) is a white noise process with mean 0 and variance \\(\\sigma^2\\), \\(c\\) is a constant, \\(\\theta_q\\) controls the time series pattern,
            and \\(\\varepsilon_{t-q}\\) are the lagged error terms. When working with stationary stochastic process whose joint probability distribution does not change with
            time, both the \\(AR(p)\\) and \\(MA(q)\\) models can be consolidated into an ARMA model [13]:",
           
           tags$br(),
           tags$br(),
           
           "\\[
            y_t - \\phi_1 y_{t-1} - \\phi_2 y_{t-2} - \\cdots - \\phi_p y_{t-p} = c + \\varepsilon_t + \\theta_1 \\varepsilon_{t-1} + \\theta_2 \\varepsilon_{t-2} + \\cdots + \\theta_q \\varepsilon_{t-q} \\tag{3}
            \\]",
           
           tags$br(),
           tags$br(),
           
           "where the notation follows that described for (Eq. 1) and (Eq. 2). However, when the
            data shows evidence of non-stationarity and the mean changes over time (i.e., trend),
            ARIMA \\((p,q,d)\\) models can be employed as they first involve a differencing \\((d)\\) step
            which removes any non-stationary trends. To difference the data, the difference
            between consecutive observations is computed, such as \\(y_t - y_{t-1}\\), and then the ARMA
            model can be applied to the data after differencing. Thus, the ARIMA model is given by
            [13]:",
           
           tags$br(),
           tags$br(),
           
           "\\[
            (1 - \\phi_1 B - \\cdots - \\phi_p B^p)(1 - B)^d y_t = c + (1 + \\theta_1 B + \\cdots + \\theta_q B^q) \\varepsilon_t \\tag{4}
            \\]",
           
           tags$br(),
           tags$br(),
           
           "where \\((1 - \\phi_1 B - \\cdots - \\phi_p B^p) y_t\\) corresponds to the \\(AR(p)\\) component, \\(B\\) represents the
            backshift operator, \\((1 - B)^d y_t\\) corresponds to differencing the time series data \\(d\\) times,
            and the \\(MA(q)\\) component is given by \\(c + (1 + \\theta_1 B + \\cdots + \\theta_q B^q) \\varepsilon_t\\). Finally, as in (Eq. 1)
            -(Eq. 3), \\(\\varepsilon_t\\) is the white noise process in the model.",
           
           tags$br(),
           tags$br(),
           
           tags$b("Seasonal ARIMA models (SARIMA)"),
           
           tags$br(),
           tags$br(),
           
           "The ARIMA model can also capture various seasonal data patterns as well, with only a
            slight adjustment to (Eq. 4) and the process discussed above. To form a seasonal
            ARIMA model we include additional parameters, \\((P,D,Q)_m\\) where \\(m\\) represents the
            seasonal period [13]. Therefore, (Eq. 4) now becomes [13]:",
           
           tags$br(),
           tags$br(),
           
           "\\[
            (1 - \\phi_1 B - \\cdots - \\phi_p B^p)(1 - \\Phi_1 B^m - \\cdots - \\Phi_P B^{mP})(1 - B)^d (1 - B^m) y_t = c + (1 + \\theta_1 B + \\cdots + \\theta_q B^q)(1 + \\Theta_1 B^m + \\cdots + \\Theta_Q B^{mQ}) \\varepsilon_t \\tag{5}
            \\]",
           
           tags$br(),
           tags$br(),
           
           "The \\(AR(p)\\), \\(MA(q)\\), and differencing terms are interpreted identical to (Eq. 4). The \\(m\\)
            parameter indicates the seasonal period, \\(1 - \\Phi_1 B^m - \\cdots - \\Phi_P B^m\\) corresponds to the
            seasonal \\(AR(P)\\) model, \\((1 - B^m) y_t\\) corresponds to the seasonal differencing and the
            seasonal \\(MA(Q)\\) model is given by \\(1 + \\Theta_1 B^m + \\cdots + \\Theta_Q B^m\\) [13]. \\(\\varepsilon_t\\) is the white noise
            process in the model [13].", 
           
           tags$br(),
           tags$br(),
           
           tags$b("Software Specifications"), 
           
           tags$br(),
           tags$br(),
           
           "We employ the", tags$i("auto.arima"), "function from the \"forecast\" package to automatically select
           the order values of the ARIMA model based on the specified data and parameter
           ranges. Additional details regarding the function can be found in [14]. To provide
           flexibility in model specifications, users can choose values for each of the parameters
           for the non-seasonal (p, q, d) and seasonal (P, Q, D, m) ARIMA models. However, as the", tags$i("auto.arima"),
           "function takes data as a timeseries object, when the seasonality parameter is set to one the model then assumes 
           no seasonal patterns in the data. Subsequently, it ignores any seasonal parameters specified. 
           Finally, the", tags$i("forecast"), "function from the \"forecast\" package is used to produce all forecasts and assumes
           the default settings as discussed in [15].",  
          
          tags$br(),
          tags$br(),
          
          tags$b("References"), 
          
          tags$br(),
          tags$br(),
          
          tags$ol(
            tags$li("Furtado P. Epidemiology SIR with regression, Arima, and Prophet in forecasting COVID-19. Eng. Proc 2021;5(1):52. https://doi.org/10.3390/engproc2021005052."),
            tags$li("Benvenuto D, Giovanetti M, Vassallo L, Angeletti S, Ciccozzi M. Application of the ARIMA model on the COVID-2019 epidemic dataset. Data Brief 2020;29:105340. https://doi.org/10.1016/j.dib.2020.105340."),
            tags$li("Abolmaali S, Shirzaei S. A comparative study of SIR Model, Linear Regression, Logistic Function, and ARIMA Model for forecasting COVID-19 cases. AIMS Public Health 2021;8(4):598. https://doi.org/10.3934/publichealth.2021048."),
            tags$li("Sah S, Surendiran B, Dhanalakshmi R, Mohanty SN, Alenezi F, Polat K. Forecasting COVID-19 pandemic using Prophet, ARIMA, and hybrid stacked LSTM-GRU models in India. Comput Math Methods Med 2022. https://doi.org/10.1155/2022/1556025."),
            tags$li("Alghamdi T, Elgazzar K, Bayoumi M, Sharaf T, Shah S, editors. Forecasting traffic congestion using ARIMA modeling. IEEE 2019; 1227-1232. https://doi.org/10.1109/IWCMC.2019.8766698."),
            tags$li("Ariyo AA, Adewumi AO, Ayo CK. Stock price prediction using the ARIMA model. IEEE 2014; 106-112. https://doi.org/10.1109/UKSim.2014.67."),
            tags$li("Tektaş M. Weather forecasting using ANFIS and ARIMA models. EREM 2010;51(1):5-10. http://dx.doi.org/10.5755/j01.erem.51.1.58."),
            tags$li("Yenidoğan I, Çayir A, Kozan O, Dağ T, Arslan Ç. Bitcoin forecasting using ARIMA and PROPHET. IEEE 2018. https://doi.org/10.1109/UBMK.2018.8566476."),
            tags$li("Samal KKR, Babu KS, Das SK, Acharaya A. Time series-based air pollution forecasting using SARIMA and prophet model. Proceedings of the 2019 international conference on information technology and computer communications 2019. http://dx.doi.org/10.1145/3355402.3355417."),
            tags$li("Bleichrodt A, Dahal S, Maloney K, Casanova L, Luo R, Chowell G. Real-time forecasting the trajectory of monkeypox outbreaks at the national and global levels, July–October 2022. BMC Med 2023;21(1):19. https://doi.org/10.1186/s12916-022-02725-2."),
            tags$li("Chowell G, Dahal S, Tariq A, Roosa K, Hyman JM, Luo R. An ensemble n-sub-epidemic modeling framework for short-term forecasting epidemic trajectories: Application to the COVID-19 pandemic in the USA. PLoS Comput Biol 2022;18(10):e1010602. https://doi.org/10.1371/journal.pcbi.1010602."),
            tags$li("Bleichrodt A, Luo R, Kirpich A, Chowell G. Retrospective evaluation of short-term forecast performance of ensemble sub-epidemic frameworks and other time-series models: The 2022-2023 mpox outbreak across multiple geographical scales, July 14th, 2022, through February 26th, 2023. medRxiv [Preprint] 2023. https://doi.org/10.1101/2023.05.15.23289989."),
            tags$li("Hyndman RJ, Athanasopoulos G. Forecasting: principles and practice. 3rd ed. Australia: OTexts; 2018. https://otexts.com/fpp3/."),
            tags$li("Hyndman RJ. auto.arima: Fit best ARIMA model to univariate time series (Version 8.22.0). RDocumentation 2024. https://www.rdocumentation.org/packages/forecast/versions/8.22.0."),
            tags$li("Hyndman RJ. forecast (Version 8.22.0). RDocumentation 2024. https://rdocumentation.org/packages/forecast/versions/8.16/topics/forecast.")
          ),
          
          tags$br(),
  
           ##################
           # Link for paper #
           ##################
           div(style = "text-align: center; width: 100%; margin: 0 auto;",
               
               a("For further information, click here to learn more.",
                 href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                 style = "color: black; font-weight: 600;",
                 class = "button")
               
           ) # End of row for button link
           
         ) # End of style for fluid page
         
        ) # Page containing information
  
       ) # Box containing information
      
      ) # End of Row containing the box 
  
     ), # end of "conditionalPanel" for ARIMA Model
  
    ########################
    # Information for GLMs #
    ########################
    conditionalPanel(
      
      # Condition to run
      condition = "input.re_model == 're_glm'",
      
      # Fluid Row
      fluidRow(
      
      #######################
      # Box for information #
      #######################
      box(
        
        # Width of box
        width = 12,
        
        # Box color
        style = "background-color: #f8f8f8;", 
        
        # Page containing information
        fluidPage(
          
          # Font size 
          tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
  
          # Allowing math equations to be shown in the panel 
          withMathJax(), 
          
          # Page width
          width = 12, 
          
          ################
          # Text to show #
          ################
          
          "Generalized Linear Models (GLMs) extend linear regression models to 
           allow for multiple types of assumed distributions (i.e., Poisson,
           Negative Binomial, Gaussian/Normal). The model contains three components: 
           (1) random or stochastic component, (2) systematic component, and (3)
           link function [1]. As applied in this toolbox, a GLM is structured as follows [1]:",
          
          tags$br(),
          tags$br(),
          
          "\\[
            g(y_t) = \\alpha + t. \\tag{1}
           \\]",
          
          tags$br(),
          tags$br(),
          
          "The random component identifies the response variable, \\(y_t\\), and
           its associated probability distribution [1]. As applied in the toolbox, 
           \\(y_t\\) corresponds to the time series data values and can be continuous 
           or discrete counts. Together, the single predictor, \\(t\\), and the intercept 
           \\(\\alpha\\) compose the systematic component of the GLM [1]. Within the 
           toolbox, we assume a simple GLM model with only one predictor \\(t\\), the 
           time components of the original data (i.e., dates). Finally, the link component
           (\\(g(\\cdot)\\)) is responsible for the robust nature of the GLM framework to other 
           error structure types [1]. Specifically, the link function designates a function 
           connecting \\(E(y_t)\\), where \\(E(y_t) = \\mu_t\\), and the linear predictor, \\(t\\) [1].",
          
          tags$br(),
          tags$br(),
          
          tags$b("Distributions"),
          
          tags$br(),
          tags$br(),
          
          "As applied in this toolbox, we utilize three possible distributions: Normal, Poisson, and Negative Binomial.",
          
          tags$br(),
          tags$br(),
          
          tags$b("Normal"),
          
          tags$br(),
          tags$br(),
          
          "The link function for the normal distribution is the identity function. 
           Therefore, when the user selects the normal distribution for the GLM, the 
           given model is the equivalent of simple linear regression (SLR) [1].",
          
          tags$br(),
          tags$br(),
          
          tags$b("Poisson"),
          
          tags$br(),
          tags$br(),
          
          "Poisson regression is a form of GLM that assumes a Poisson distribution 
           which is used to model counts and has the property of \\(\\sigma^2 = \\mu\\). 
           Further details regarding the distribution can be found in [2]. As applied in
           the toolbox, a GLM assuming a Poisson-distributed outcome applies a logarithm link; 
           thus, it is often referred to as a 'log-linear'. Using (Eq. 1), the application of the Poisson 
           link is as follows [3]:",
          
          tags$br(),
          tags$br(),
          
          "\\[
           \\ln(\\mu_t) = \\alpha + t. \\tag{2}
           \\]",
          
          tags$br(),
          tags$br(),
          
          "The general function, \\(g(\\cdot)\\), introduced in (Eq. 1) is now given as
           \\(\\ln\\) to show the specific function used within Poisson regression.",
          
          tags$br(),
          tags$br(),
          
          tags$b("Negative Binomial"),
          
          tags$br(),
          tags$br(),
          
          "Similar to Poisson regression, Negative Binomial Regression requires count 
           data. However, unlike Poisson regression, Negative Binomial models are more
           robust to overdispersed data as they allow the variance to exceed the mean [4]. 
           For example, the quadratic mean-variance relationship is given by \\(\\sigma^2 = \\mu 
           + \\frac{\\mu^2}{\\theta}\\) [4]. Additional details regarding the estimation of 
           \\(\\theta\\) can be found in [4]. The link function is identical to that of 
           the Poisson distribution.",
          
          tags$br(),
          tags$br(),
          
          tags$b("Software Specifications"),
          
          tags$br(),
          tags$br(),
          
          "We employ the `glm` function from the 'forecast' package to conduct simple 
           linear and Poisson regression [5]. We use the `glm.nb` function provided 
           by the 'MASS' package when assuming a negative binomial distribution [6].
           We utilized the `predict` function from the 'stats' package to obtain model 
           forecasts and fits [7]. Regarding model customization, users can select
           the underlying distribution assumption (i.e., Normal, Poisson, or Negative
           Binomial). If either Poisson or Negative Binomial is selected, we exponentiate
           the resulting model fits and forecasts to return results in the original
           scale of the data. We use the default settings for both functions provided
           in their respective documentation.",
          
          tags$br(),
          tags$br(),
          
          tags$b("References"), 
          
          tags$br(),
          tags$br(),
          
          tags$ol(
            tags$li("Agresti A. Introduction to Categorical Data Analysis. 2nd ed. New Jersey: John Wiley & Sons, Inc.; 2007."),
            tags$li("Roback P, Legler J. Beyond Multiple Linear Regression: Applied Generalized Linear Models and Multilevel Models in R. Florida: CRC Press; 2021. https://bookdown.org/roback/bookdown-BeyondMLR/."),
            tags$li("Kida Y. Generalized linear models: Introduction to advanced statistical modeling. Towards Data Science 2019. https://towardsdatascience.com/generalized-linear-models-9cbf848bb8ab."),
            tags$li("Venables WN, Ripley BD. Modern Applied Statistics with S. 4th ed. New York: Springer; 2002."),
            tags$li("R-core. glm: Fitting Generalized Linear Models (Version 3.6.2). RDocumentation 1969. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm."),
            tags$li("Ripley B. glm.nb: Fit a Negative Binomial Generalized Linear Model (Version 7.3-60.0.1). RDocumentation 2024. https://www.rdocumentation.org/packages/MASS/versions/7.3-60.0.1/topics/glm.nb."),
            tags$li("R-core. predict: Model Predictions (Version 3.6.2). RDocumentation 1969. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/predict.")
          ), 
          
          tags$br(),
  
          ##################
          # Link for paper #
          ##################
          div(style = "text-align: center;",
              a("For further information, click here to learn more.",
                href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                style = "color: black; font-weight: 600;",
                class = "button")
              
          ) # End of row for button link
          
          ) # End of style for box
           
         ) # Page containing information
         
       ) # Box containing information
      
      ) # End of fluid row 
       
     ), # End of conditional panel 
  
    ######################################
    # Generalized Additive Models (GAMs) #
    ######################################
    conditionalPanel(
      
      # Condition to show the panel
      condition = "input.re_model == 're_gam'",
      
      # Fluid row
      fluidRow(
      
      #######################
      # Box for information #
      #######################
      box(
        
        # Width of box
        width = 12,
        
        # Box color
        style = "background-color: #f8f8f8;", 
        
        # Page containing information
        fluidPage(
          
          # Font size 
          tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
          
          # Allowing math equations to be shown in the panel 
          withMathJax(), 
          
          # Page width
          width = 12,
          
          ################
          # Text to show #
          ################
          
          "Generalized Additive Models (GAMs) extend GLMs to include non-linear 
          data patterns while maintaining similar levels of explainability and 
          simplicity [1]. Unlike GLM models, GAM models include a sum of unknown 
          smooth functions of some covariates [2]. Specific to our dashboard, 
          with time as the only covariate, the general structure of the GAM, 
          assuming normality, is as follows [1]:",
          
          tags$br(),
          tags$br(),
          
          "\\[
          y_t = \\beta_0 + z(t) + \\epsilon_t \\tag{1}
          \\]",
          
          tags$br(),
          tags$br(),
          
          "where \\(z(\\cdot)\\) is an unknown smooth function of time. However, 
          \\(\\epsilon_t\\) is included only in the model assuming normality and 
          follows \\(\\epsilon_t \\sim N(0, \\sigma^2)\\) [3]. The Poisson and 
          Negative Binomial distributions follow that discussed for the included GLM,
          models and both distributions employ a log-link resulting in:",
          
          tags$br(),
          tags$br(),
          
          "\\[
          \\ln(y_t) = \\beta_0 + z(t). \\tag{2}
          \\]",
          
          tags$br(),
          tags$br(),
          
          "Regardless of the distribution selected by the user, the smooth function 
          \\(z(\\cdot)\\) is represented using basis functions (i.e., building blocks
          for complex functions). The default setting employed within the dashboard 
          uses basis splines, or piecewise polynomial functions [4]. Specifically,",
          
          tags$br(),
          tags$br(),
          
          "\\[
          z(t) = \\sum_{k=1}^{K} \\beta_k b_k(t), \\tag{3}
          \\]",
          
          tags$br(),
          tags$br(),
          
          "where \\(\\{b_k(\\cdot)\\}\\) represent the basis functions, 
          \\(\\{\\beta_k\\}\\) are the expansion coefficients to be estimated, 
          and \\(k\\) is the number of basis functions [1]. Here, the number of
          basis functions varies depending on the length of calibration data
          available for a given forecasting period. Users can select the specific 
          basis function from the following [5]; though we set the default to a
          discrete penalty on the basis coefficients, and then fit the model by 
          solving a penalized least square problem (i.e., ps in [5]). 
          Additionally, users can also customize the number of basis functions 
          considered. For any GAM employed within the toolbox, the generalized 
          cross-validation (GCV) criterion selects the smoothness tuning parameter [4].",
          
          tags$br(),
          tags$br(),
          
          tags$b("Software Specifications"),
          
          tags$br(),
          tags$br(),
          
          "We employ the `gam` function from the 'mcgv' package for model fitting
          [4]. Regarding model customization, users can select the underlying 
          distribution assumption (i.e., Normal, Poisson, or Negative Binomial). 
          Further details related to the packages used can be found on the GLM 
          page. If either Poisson or Negative Binomial is selected, we exponentiate the
          resulting model fits and forecasts to return results in the original 
          scale of the data. Additionally, while the number of bases functions 
          is auto populated based upon the length of the calibration period, 
          users can specify the number of basis functions (k) they wish to employ 
          in the calculation of the smoothing function. Finally users can 
          also specify a different smoothing term than the default ps option, 
          selecting one of the options provided in [5]. Once the model has 
          been fit, we utilize the `predict` function from the 'stats' package to 
          obtain model forecasts and fits [6]. Except for the available 
          specifications discussed here, we use the default settings for both
          functions (`gam` and `predict`) provided in their respective documentation.",
          
          tags$br(),
          tags$br(),
          
          tags$b("References"), 
          
          tags$br(),
          tags$br(),
          
          tags$ol(
            tags$li("Shafi A. What are Generalized Additive Models? Towards Data Science 2021. https://towardsdatascience.com/generalised-additive-models-6dfbedf1350a."),
            tags$li("Wood SN. Generalized Additive Models: An Introduction with R. 2nd ed. New York: Chapman and Hall/CRC; 2017."),
            tags$li("Roback P, Legler J. Beyond Multiple Linear Regression: Applied Generalized Linear Models and Multilevel Models in R. Florida: CRC Press; 2021. https://bookdown.org/roback/bookdown-BeyondMLR/."),
            tags$li("Wood SN. gam: Generalized additive models with integrated smoothness estimation (Version 1.9-1). RDocumentation 2023. https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/gam."),
            tags$li("Wood SN. smooth.terms: Smooth terms in GAM (Version 1.9-1). RDocumentation 2023. https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/smooth.terms."),
            tags$li("R-core. predict: Model Predictions (Version 3.6.2). RDocumentation 1969. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/predict.")
          ),
          
          tags$br(),
  
          ##################
          # Link for paper #
          ##################
          div(style = "text-align: center;",
              a("For further information, click here to learn more.",
                href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                style = "color: black; font-weight: 600;",
                class = "button")
              
            ) # End of row for button link
          
          ) # End of box style 
          
        ) # Page containing information
        
      ) # Box containing information
      
     ) # End of fluidRow
      
    ), # end of "conditionalPanel" for GAM Model
  
    ######################################
    # Facebook's Prophet Model (Prophet) #
    ######################################
    conditionalPanel(
      
      # Condition to show the panel
      condition = "input.re_model == 're_prophet'",
      
      # Fluid Row 
      fluidRow(
      
      #######################
      # Box for information #
      #######################
      box(
        
        # Width of box
        width = 12,
        
        # Box color
        style = "background-color: #f8f8f8;", 
        
        # Page containing information
        fluidPage(
          
          # Font size 
          tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                   
                   # Allowing math equations to be shown in the panel 
                   withMathJax(), 
                   
                   # Page width
                   width = 12,
                   
                   ################
                   # Text to Show #
                   ################
                   
                   "Facebook's Prophet model has been increasingly employed across 
                   multiple fields to forecast different processes of interest [1-10].
                   The model's specification is similar to that of the GAM and is 
                   decomposable into three primary components: (1) trend, (2) seasonality, 
                   and (3) holidays and an error term [11]. Thus, the model has the 
                   following form with time as the only regressor [11]:",
                   
                   tags$br(),
                   tags$br(),
                   
                    "\\[
                    y(t) = g(t) + s(t) + h(t) + \\epsilon_t. \\tag{1}
                    \\]",
                                     
                   tags$br(),
                   tags$br(),
                   
                   "Here, \\(g(t)\\) represents the non-periodic component for 
                   modeling the time series trend. Periodic trends (e.g., seasonal 
                   changes) are captured by \\(s(t)\\), and \\(h(t)\\) is an 
                   irregular events component used for modeling irregular changes 
                   (e.g., holidays or similar events). The component \\(\\epsilon_t\\) 
                   represents the normally distributed model errors at time \\(t\\) 
                   [11]. Additional details regarding the model can be found in [11-12].",
                   
                   tags$br(),
                   tags$br(),
                   
                   tags$b("Non-periodic Component, \\(g(t)\\)"),
                   
                   tags$br(),
                   tags$br(),
                   
                   "Within the dashboard, users can select from linear or uniform 
                   growth trends. When a user chooses the linear growth option, 
                   \\(g(t)\\) becomes a set of piecewise linear equations where 
                   the growth rate varies between change points [11]. Thus, the 
                   non-periodic component is given by [11]:",
                   
                   tags$br(),
                   tags$br(),
                   
                    "\\[
                    g(t) = (k + a(t)^T \\delta)t + (m + a(t)^T \\gamma) \\tag{2}
                    \\]",
                   
                   tags$br(),
                   tags$br(),
                   
                   "where \\(k\\) is the growth rate, and \\(m\\) is an offset
                   parameter. The Prophet model allows the growth rate to change 
                   at various time points or changepoints. Therefore, there are 
                   \\(S\\) automatically selected changepoints at time \\(s_j\\),
                   where \\(j = 1, \\ldots, S\\) [11]. \\(\\delta\\) represents 
                   the vector of rate adjustments, where \\(\\delta_j\\) is the 
                   rate change at \\(s_j\\) [11]. The rate at time \\(t\\) is 
                   represented by \\(k + \\mathbf{a}(t)^\\top \\delta\\), where [11]:",
                   
                   tags$br(),
                   tags$br(),
                   
                   "\\[
                    a_j(t) = 
                    \\begin{cases}
                    1, & \\text{if } t \\geq s_j \\\\
                    0, & \\text{Otherwise}.
                    \\end{cases} \\tag{3}
                    \\]",
                   
                   tags$br(),
                   tags$br(),
                   
                   "\\(m\\), the offset parameter, is also adjusted with each 
                   change to \\(k\\). Finally, to ensure a continuous function, 
                   the adjustment at changepoint \\(j\\) is given by 
                   \\(-s_j\\delta_j\\) [11]. As applied in the dashboard, the 
                   changepoints are automatically selected by setting a sparse
                   prior on \\(\\delta\\) [11]. Additional details regarding the 
                   changepoints can be found in [11]. If the user selects the 
                   'flat' growth option, \\(g(t)\\) becomes a constant value [11].",
                   
                   tags$br(),
                   tags$br(),
                   
                   tags$b("Periodic Component, \\(s(t)\\)"),
                   
                   tags$br(),
                   tags$br(),
                   
                   "The seasonal component of the Prophet model, \\(s(t)\\), 
                   relies heavily on a Fourier series. Specifically, the model 
                   approximates seasonal effects with the following standard 
                   Fourier series [11]:",
                   
                   tags$br(),
                   tags$br(),
                   
                   "\\[
                    s(t) = \\sum_{n=1}^{N} (a_n \\cos (\\frac{2 \\pi n t}{P}) + b_n \\sin (\\frac{2 \\pi n t}{P})) \\beta \\tag{4}
                    \\]",
                   
                   tags$br(),
                   tags$br(),
                   
                   "where \\(P\\) is the time series’ period, \\(N\\) represents 
                   the number of sine and cosine pairs included in the Fourier 
                   series, and \\(a_n\\) and \\(b_n\\) are the coefficients of the
                   Fourier series. \\(\\beta\\) is a smoothing prior imposed on 
                   the seasonality, where \\(\\beta \\sim \\text{Normal}(0, \\sigma^2)\\) 
                   [11]. If a user selects", tags$em("Daily"), "seasonality, \\(N = 4\\). 
                   If", tags$em("Weekly"), "seasonality is selected, \\(N = 3\\), and 
                   \\(N = 10\\) when", tags$em("Yearly"), "seasonality is selected [13]. 
                   If the user selects", tags$em("Auto"), "for seasonality, then \\(s(t)\\) 
                   is selected based on the Akaike Information Criterion (AIC) [11]. 
                   If", tags$em("None"), "is selected, the \\(s(t)\\) term disappears 
                   from the model [11].",
                   
                   tags$br(),
                   tags$br(),
                   
                   tags$b("Irregular Events Component, \\(h(t)\\)"),
                   
                   tags$br(),
                   tags$br(),
                   
                   "The final component of the Prophet model, \\(h(t)\\), allows 
                   the user to consider the effect of major holidays in the model 
                   fitting and forecasting process. Specifically, users can specify 
                   dates to be treated as holidays, where the effect of each 
                   selected date is independent [11]. Therefore, the irregular 
                   events component is given by [11]:",  
                   
                   tags$br(),
                   tags$br(),
                   
                   "\\[
                    h(t) = [1(t \\in D_1), ..., 1(t \\in D_i)] \\kappa \\tag{5}
                    \\]",
                   
                   tags$br(),
                   tags$br(),
                   
                   "where \\(D_i\\) represents the past and future dates for a 
                   given holiday, \\(i\\), and \\(1\\) is an indicator representing 
                   if time \\(t\\) falls during holiday \\(i\\) [11]. Finally, \\(\\kappa\\) 
                   represents the change in the forecast, where 
                   \\(\\kappa \\sim \\text{Normal}(0, v^2)\\) [11].",
                   
                   tags$br(),
                   tags$br(),
                   
                   tags$b("Software Specifications"),
                   
                   tags$br(),
                   tags$br(),
                   
                   "We employ the prophet function from the 'prophet' package for 
                   model fitting [12]. As discussed above, users can select either 
                   a", tags$em("linear"), "or", tags$em("flat"), "growth trend 
                   assumption as part of the model fitting process. Additionally, 
                   users have multiple seasonality assumption options with the 
                   default set to", tags$em("Auto"), "[12]. Finally, if there are 
                   holidays which should be considered as part of the model fitting
                   and forecasting process, users can select multiple holidays
                   (i.e., dates) via the calendar in the sidebar panel. However, the
                   dashboard defaults to considering no holidays or special dates. 
                   We utilize the", tags$em("predict"), "function from the 'stats'
                   package to obtain model forecasts and fitted values [14].",  
                   
                   tags$br(),
                   tags$br(),
  
                   tags$b("References"), 
                   
                   tags$br(),
                   tags$br(),
                   
                   tags$ol(
                     tags$li("Furtado P. Epidemiology SIR with regression, Arima, and Prophet in forecasting COVID-19. Eng. Proc 2021;5(1):52. https://doi.org/10.3390/engproc2021005052."),
                     tags$li("Sah S, Surendiran B, Dhanalakshmi R, Mohanty SN, Alenezi F, Polat K. Forecasting COVID-19 pandemic using Prophet, ARIMA, and hybrid stacked LSTM-GRU models in India. Comput Math Methods Med 2022. https://doi.org/10.1155/2022/1556025."),
                     tags$li("Yenidoğan I, Çayir A, Kozan O, Dağ T, Arslan Ç. Bitcoin forecasting using ARIMA and PROPHET. IEEE 2018. https://doi.org/10.1109/UBMK.2018.8566476."),
                     tags$li("Samal KKR, Babu KS, Das SK, Acharaya A. Time series based air pollution forecasting using SARIMA and prophet model. Proceedings of the 2019 international conference on information technology and computer communications 2019. http://dx.doi.org/10.1145/3355402.3355417."),
                     tags$li("Bleichrodt A, Luo R, Kirpich A, Chowell G. Retrospective evaluation of short-term forecast performance of ensemble sub-epidemic frameworks and other time-series models: The 2022-2023 mpox outbreak across multiple geographical scales, July 14th, 2022, through February 26th, 2023. medRxiv [Preprint] 2023. https://doi.org/10.1101/2023.05.15.23289989."),
                     tags$li("Xie C, Wen H, Yang W, Cai J, Zhang P, Wu R, et al. Trend analysis and forecast of daily reported incidence of hand, foot and mouth disease in Hubei, China by Prophet model. Sci Rep 2021;11(1):1445. https://doi.org/10.1038/s41598-021-81100-2."),
                     tags$li("Navratil M, Kolkova A. Decomposition and forecasting time series in the business economy using prophet forecasting model. Cent. Eur. Bus. Rev 2019;8(4):26. http://dx.doi.org/10.18267/j.cebr.221."),
                     tags$li("Jha BK, Pande S, editors. Time series forecasting model for supermarket sales using FB-prophet. IEEE 2021. http://dx.doi.org/10.1109/ICCMC51019.2021.9418033."),
                     tags$li("Kirpich A, Shishkin A, Weppelmann TA, Tchernov AP, Skums P, Gankin Y. Excess mortality in Belarus during the COVID-19 pandemic as the case study of a country with limited non-pharmaceutical interventions and limited reporting. Sci Rep 2022;12(1):5475. https://doi.org/10.1038/s41598-022-09345-z."),
                     tags$li("Shishkin A, Lhewa P, Yang C, Gankin Y, Chowell G, Norris M, et al. Excess mortality in Ukraine during the course of COVID-19 pandemic in 2020–2021. Sci Rep 2023;13(1):6917. https://doi.org/10.1038/s41598-023-33113-2."),
                     tags$li("Taylor SJ, Letham B. Forecasting at scale. The American Statistician 2018;72(1):37-45. https://doi.org/10.1080/00031305.2017.1380080."),
                     tags$li("Taylor SJ. prophet: Prophet forecaster (Version 1.0). RDocumentation 2021. https://www.rdocumentation.org/packages/prophet/versions/1.0/topics/prophet."),
                     tags$li("Rafferty G. Forecasting Time Series Data with Facebook Prophet: Build, improve, and optimize time series forecasting models using the advanced forecasting tool. United Kingdom: Packt Publishing; 2021."),
                     tags$li("R-core. predict: Model Predictions (Version 3.6.2). RDocumentation 1969. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/predict.")
                   ),
                   
                   tags$br(),
                   
                   ##################
                   # Link for paper #
                   ##################
                   div(style = "text-align: center;",
                       a("For further information, click here to learn more.",
                         href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                         style = "color: black; font-weight: 600;",
                         class = "button")
                       
                   ) # End of row for button link
                   
            ) # End of box style 
          
          ) # Page containing information
        
        ) # Box containing information
      
      ) # End of fluidRow 
      
    ) # End of "conditionalPanel" for Prophet Model
    
  ), # End of "conditionalPanel" for seeing model specifications

#------------------------------------------------------------------------------#
# Creating Page 1D: Evaluation Criteria ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section, specifically, includes details related to the different #
# models available as part of the dashboard, included the math behind each     #
# model and the software associated with each. This only shows up if the       #
# user selects to learn more information about the model specifications.       #
#------------------------------------------------------------------------------#

  #####################################################################
  # Panel if the "Evaluation Criteria" option is selected by the user #
  #####################################################################
  conditionalPanel(
    
    # Condition to run 
    condition = "input.topicsSelect == 'Evaluation Criteria.'",
    
    # Creating the row with criteria details
    fluidRow(
      
      ###########################
      # Box - Choosing a metric #
      ###########################
      box(
        
        # Title of Box 
        title = strong("Choose a Evaluation Criteria to Learn More About:"),
        
        # Width of Box 
        width = 12,
        
        # Picker Input - Metrics
        pickerInput("re_metric",
                    choices = c("Mean Squared Error (MSE)" = "re_mse", 
                                "Mean Absolute Error (MAE)" = "re_mae", 
                                "Prediction Interval Coverage (PI coverage)" = "re_PI",
                                "Weighted Interval Scores (WIS)" = "re_WIS",
                                "Akaike information criterion (AIC)" = "re_AIC",
                                "Corrected Akaike information criterion (AICc)" = "re_AICc",
                                "Bayesian information criterion (BIC)" = "re_BIC", 
                                "Skill Scores" = "re_SS",
                                "Winkler Scores" = "re_winkler"),
                    multiple = FALSE)
        
      ) # End of box to select a metric
      
    ), # End of row with selector 
  
    ###################
    # MSE Information #
    ###################
    conditionalPanel(
      
      # Condition to show the panel
      condition = "input.re_metric == 're_mse'",
      
      # Fluid row 
      fluidRow(
      
      #######################
      # Box for information #
      #######################
      box(
        
        # Width of box
        width = 12,
        
        # Box color
        style = "background-color: #f8f8f8;", 
        
        # Page containing information
        fluidPage(
          
          # Font size 
          tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                   
                   # Allowing math equations to be shown in the panel 
                   withMathJax(), 
                   
                   # Page width 
                   width = 12, 
                   
                   ################
                   # Text to show #
                   ################
                   
                   "Mean Squared Error (MSE) assess deviations in the mean model
                    fit from the original time series data [1]. Specifically, 
                    MSE is given by [1]:",
                   
                   tags$br(),
                   tags$br(),
                   
                    "\\[
                    \\text{MSE} = \\frac{1}{N} \\sum_{t=1}^{N} (\\hat{y}_t - y_t)^2 \\tag{1}
                    \\]",
                    
                    tags$br(),
                  
                    "In the above equation, \\(y_t\\) is the original data of the 
                    process of interest at time point \\(t\\), and \\(\\hat{y}_t\\)
                    is the model fit or forecasted value at time \\(t\\). \\(N\\)
                    represents the total length of time under evaluation (i.e., 
                    calibration or forecast period).", 
                   
                    tags$br(),
                    tags$br(),
                     
                    tags$b("References"), 
                     
                    tags$br(),
                    tags$br(),
                   
                    tags$ol(
                      
                      tags$li("Kuhn M, Johnson K. Applied predictive modeling. 2013th 1127 ed. New York: Springer; 2013. https://doi.org/10.1007/978-1-4614-6849-3.")
                       
                       ),
                   
                     tags$br(),
                     
                     ##################
                     # Link for paper #
                     ##################
                     div(style = "text-align: center; width: 100%; margin: 0 auto;",
                         
                         a("For further information, click here to learn more.",
                           href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                           style = "color: black; font-weight: 600;",
                           class = "button")
                         
                     ) # End of row for button link
                     
            ) # End of style for fluid page
            
          ) # Page containing information
          
        ) # Box containing information
      
       ) # End of fluidRow
        
      ), # End of "conditionalPanel" for MSE
    
      ###################
      # MAE Information #
      ###################
      conditionalPanel(
        
        # Condition to show the panel
        condition = "input.re_metric == 're_mae'",
        
        # Fluid Row
        fluidRow(
        
        #######################
        # Box for information #
        #######################
        box(
          
          # Width of box
          width = 12,
          
          # Box color
          style = "background-color: #f8f8f8;", 
          
          # Page containing information
          fluidPage(
            
            # Font size 
            tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                     
                     # Allowing math equations to be shown in the panel 
                     withMathJax(), 
                     
                     # Page width 
                     width = 12, 
                     
                     ################
                     # Text to show #
                     ################
                     
                     "Mean Absolute Error (MAE) assess deviations in the mean model
                      fit from the original time series data [1]. Specifically, 
                      MAE is given by [1]:",
                     
                     tags$br(),
                     tags$br(),
                     
                     "\\[
                      \\text{MAE} = \\frac{1}{N} \\sum_{h=1}^{N} |\\hat{y}_t - y_t| \\tag{1}
                      \\]",
                     
                     tags$br(),
                     
                     "In the above equation, \\(y_t\\) is the original data of the 
                      process of interest at time point \\(t\\), and \\(\\hat{y}_t\\)
                      is the model fit or forecasted value at time \\(t\\). \\(N\\)
                      represents the total length of time under evaluation (i.e., 
                      calibration or forecast period).", 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$b("References"), 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$ol(
                       
                       tags$li("Kuhn M, Johnson K. Applied predictive modeling. 2013th 1127 ed. New York: Springer; 2013. https://doi.org/10.1007/978-1-4614-6849-3.")
                       
                     ),
                     
                     tags$br(),
                     
                     ##################
                     # Link for paper #
                     ##################
                     div(style = "text-align: center; width: 100%; margin: 0 auto;",
                         
                         a("For further information, click here to learn more.",
                           href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                           style = "color: black; font-weight: 600;",
                           class = "button")
                         
                     ) # End of row for button link
                     
            ) # End of style for fluid page
            
          ) # Page containing information
          
        ) # Box containing information
        
       ) # End of fluidRow 
        
      ), # End of "conditionalPanel" for MAE
    
      ###########################
      # PI Coverage Information #
      ###########################
      conditionalPanel(
        
        # Condition to show the panel
        condition = "input.re_metric == 're_PI'",
        
        # Fluid Row
        fluidRow(
        
        #######################
        # Box for information #
        #######################
        box(
          
          # Width of box
          width = 12,
          
          # Box color
          style = "background-color: #f8f8f8;", 
          
          # Page containing information
          fluidPage(
            
            # Font size 
            tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                     
                     # Allowing math equations to be shown in the panel 
                     withMathJax(), 
                     
                     # Page width 
                     width = 12, 
                     
                     ################
                     # Text to show #
                     ################
                     
                     "The dashboard provides the prediction interval coverage as measure of model fit and 
                     forecast uncertainty. Thus, the PI coverage represents the
                     proportion of observed data the falls within the selected PI level, as 
                     indicated below:",
                     
                     tags$br(),
                     tags$br(),
                     
                     "\\[
                      (1 - \\alpha) \\times 100\\% \\text{ PI} \\tag{1}
                      \\]",
                     
                     tags$br(),
                     
                     "where \\(\\alpha\\) is the select significance level.",
                     
                     tags$br(),
                     tags$br(),
                     
                     ##################
                     # Link for paper #
                     ##################
                     div(style = "text-align: center; width: 100%; margin: 0 auto;",
                         
                         a("For further information, click here to learn more.",
                           href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                           style = "color: black; font-weight: 600;",
                           class = "button")
                         
                     ) # End of row for button link
                     
           ) # End of style for fluid page
            
          ) # Page containing information
          
         ) # Box containing information
        
        ) # End of fluidRow
        
      ), # End of "conditionalPanel" for 95% PI Coverage
    
      ###################
      # WIS Information #
      ###################
      conditionalPanel(
        
        # Condition to show the panel
        condition = "input.re_metric == 're_WIS'",
        
        # Fluid row
        fluidRow(
        
        #######################
        # Box for information #
        #######################
        box(
          
          # Width of box
          width = 12,
          
          # Box color
          style = "background-color: #f8f8f8;", 
          
          # Page containing information
          fluidPage(
            
            # Font size 
            tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                     
                     # Allowing math equations to be shown in the panel 
                     withMathJax(), 
                     
                     # Page width 
                     width = 12, 
                     
                     ################
                     # Text to show #
                     ################
                     
                     "The dashboard provides WIS as measure of model fit and 
                     forecast uncertainty. The WIS, described in [1], provides 
                     quantiles of the predictive forecast distribution by
                     combining a set of interval scores (IS) from probabilistic 
                     forecasts [2]. Only a central (1 − \\(\\alpha\\)) * 100% 
                     PI is needed to calculate an IS [2] as indicated below:",
                     
                     tags$br(),
                     tags$br(),
                     
                     "\\[
                         IS_{\\alpha}(F, y) = (u - l) + \\frac{2}{\\alpha} 
                         \\cdot (l - y) \\cdot 1(y < l) + \\frac{2}{\\alpha} 
                         \\cdot (y - u) \\cdot 1(y > u) \\tag{1}
                      \\]", 
                     
                     tags$br(),
                     
                     tags$b("1"), "is an indicator function, where", tags$b(1), 
                     "\\((y < l) = 1 \\) if \\( y < l \\) and 0 otherwise. The 
                     \\( \\frac{\\alpha}{2} \\) and \\( 1 - \\frac{\\alpha}{2} 
                     \\) quantiles of the forecast", tags$em("F"), "are represented by", 
                     tags$em("l"), "and", tags$em("u"),". The IS consists of three 
                     distinct quantities [2]:", 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$ol(
                       tags$li("The sharpness of", tags$em("F"), " given by the width 
                               \\( u - l \\) of the central \\((1 - \\alpha) 
                               \\times 100\\%\\) PI."),
                       tags$li("A penalty term \\( \\frac{2}{\\alpha} \\cdot 
                               (l - y) \\cdot 1(y < l) \\) for the observations 
                               that fall below the lower end point", tags$em("l"),
                               "of the \\((1 - \\alpha) \\times 100\\%\\) PI. 
                               This penalty term is directly proportional to the
                               distance between", tags$em("y"), "and the lower
                               end", tags$em("l")," of the PI. The strength of the 
                               penalty depends on the level."),
                       tags$li("An analogous penalty term \\( \\frac{2}{\\alpha} 
                               \\cdot (y - u) \\cdot 1(y > u) \\) for the 
                               observations falling above the upper 
                               limit", tags$em("u"), " of the PI.")
                     ),
                     
                     tags$br(),
                     
                     "As indicated in the ", tags$em("Quantile Forecast"), 
                     " box on the Forecasting page, the dashboard provides 
                     multiple central PIs at different levels: \\((1 - \\alpha_1)
                     < (1 - \\alpha_2) < \\cdots < (1 - \\alpha_k)\\). Additionally,", tags$em("m"),  
                     ", the central prediction interval at level \\(1 - \\alpha_0 \\to 0\\), 
                     is given by the first column in the ", tags$em("Quantile Forecast"), " box. From 
                     this, the ", tags$em("WIS"), " is calculated by [1, 3-4]:",
                     
                     tags$br(),
                     tags$br(),
                     
                     "\\[
                     WIS_{\\alpha_0:K}(F, y) = \\frac{1}{K + \\frac{1}{2}} \\cdot 
                     \\left( w_0 \\cdot |y - m| + \\sum_{k=1}^{K} w_k \\cdot 
                     IS_{\\alpha_k}(F, y) \\right) \\tag{2}
                     \\]",
                     
                     tags$br(),
                     
                     "where, \\( w_k = \\frac{\\alpha_k}{2} \\) for \\( k = 1, 2, 
                     \\ldots, K \\) and \\( w_0 = \\frac{1}{2} \\).",
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$b("References"), 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$ol(
                       
                       tags$li("Chowell G, Dahal S, Bleichrodt A, Tariq A, Hyman JM, Luo R. SubEpiPredict: A tutorial-based primer and toolbox for fitting and forecasting growth trajectories using the ensemble n-sub-epidemic modeling framework. Infectious Disease Modelling 2024. https://doi.org/10.1016/j.idm.2024.02.001."),
                       tags$li("Gneiting T, Raftery AE. Strictly Proper Scoring Rules, Prediction, and Estimation. J Am Stat Assoc 2007;102(477):359-78. https://doi.org/10.1198/016214506000001437."),
                       tags$li("Bracher J, Ray EL, Gneiting T, Reich NG. Evaluating epidemic forecasts in an interval format. PLoS Comput Biol 2021;17(2):e1008618. https://doi.org/10.1371/journal.pcbi.1008618."),
                       tags$li("Cramer EY, Ray EL, Lopez VK, Bracher J, Brennen A, Castro Rivadeneira AJ, et al. Evaluation of individual and ensemble probabilistic forecasts of COVID-19 mortality in the United States. Proc Natl Acad Sci U S A 2022;119(15):e2113561119. https://doi.org/10.1073/pnas.2113561119.")
                     
                       ),
                     
                     tags$br(),
                     
                     ##################
                     # Link for paper #
                     ##################
                     div(style = "text-align: center; width: 100%; margin: 0 auto;",
                         
                         a("For further information, click here to learn more.",
                           href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                           style = "color: black; font-weight: 600;",
                           class = "button")
                         
                     ) # End of row for button link
                     
            ) # End of style for fluid page
            
          ) # Page containing information
          
         ) # Box containing information
        
        ) # End of fluidRow 
        
      ), # End of "conditionalPanel" for WIS
    
    ###################
    # AIC Information #
    ###################
    conditionalPanel(
      
      # Condition to show the panel
      condition = "input.re_metric == 're_AIC'",
      
      # Fluid Row
      fluidRow(
        
        #######################
        # Box for information #
        #######################
        box(
          
          # Width of box
          width = 12,
          
          # Box color
          style = "background-color: #f8f8f8;", 
          
          # Page containing information
          fluidPage(
            
            # Font size 
            tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                     
                     # Allowing math equations to be shown in the panel 
                     withMathJax(), 
                     
                     # Page width 
                     width = 12, 
                     
                     ################
                     # Text to show #
                     ################
                     "The Alkaike Information Criterion (AIC) is frequently used 
                     to select, evaluate, and compare model fits [1-2]. It is as follows [1]:",
                     
                     tags$br(),
                     tags$br(),
                     
                     "\\[
                      AIC = -2*LL + (2*k) \\tag{1}
                      \\]",
                     
                     tags$br(),

                     "where", tags$em("LL"), " is the log-likelihood,", tags$em("k"),
                     " represents the number of parameters included in the model 
                     and the calibration period length is given by", tags$em("n"), ".", 
                     "The AIC for the model fit is directly available as part of
                     the", tags$em("auto.arima()"), " function [3]. However, for the 
                     GLM and GAM models, the AIC must be manually calculated 
                     using (1). The dashboard obtains the log-likelihood
                     directly from the GLM and GAM fits, utilizing the", tags$em("logLik()"), 
                     "function [4]. The AIC only applies to frequent 
                     methods, so the dashboard will not supply the value for 
                     the Prophet model, a Bayesian framework.", 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$b("References"), 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$ol(
                       
                       tags$li("Spiess AN, Neumeyer N. An evaluation of R2 as an inadequate measure for nonlinear models in pharmacological and biochemical research: a Monte Carlo approach. BMC Pharmacol. 2010 Jun 7;10:6. doi: 10.1186/1471-2210-10-6. PMID: 20529254; PMCID: PMC2892436."),
                       tags$li("Akaike H. Information theory and an extension of the maximum likelihood principle. InSelected papers of hirotugu akaike 1998 (pp. 199-213). New York, NY: Springer New York."),
                       tags$li("Hyndman RJ. auto.arima: Fit best ARIMA model to univariate time series (Version 8.22.0). RDocumentation 2024. https://www.rdocumentation.org/packages/forecast/versions/8.22.0."),
                       tags$li("R-Core. logLik: Extract Log-Likelihood (Version 3.6.2). RDocumentation 2024. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/logLik"),
                       
                     ),
                     
                     tags$br(),
                     
                     ##################
                     # Link for paper #
                     ##################
                     div(style = "text-align: center; width: 100%; margin: 0 auto;",
                         
                         a("For further information, click here to learn more.",
                           href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                           style = "color: black; font-weight: 600;",
                           class = "button")
                         
                     ) # End of row for button link
                     
            ) # End of style for fluid page
            
          ) # Page containing information
          
        ) # Box containing information
        
      ) # End of fluidRow
      
    ), # End of "conditionalPanel" for AIC
    
    ####################
    # AICc Information #
    ####################
    conditionalPanel(
      
      # Condition to show the panel
      condition = "input.re_metric == 're_AICc'",
      
      # Fluid Row
      fluidRow(
        
        #######################
        # Box for information #
        #######################
        box(
          
          # Width of box
          width = 12,
          
          # Box color
          style = "background-color: #f8f8f8;", 
          
          # Page containing information
          fluidPage(
            
            # Font size 
            tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                     
                     # Allowing math equations to be shown in the panel 
                     withMathJax(), 
                     
                     # Page width 
                     width = 12, 
                     
                     ################
                     # Text to show #
                     ################
                     "The bias-corrected Alkaike Information Criterion (AICc) 
                     is a variation of the Alkaike Information Criterion (AIC) 
                     that adjusts for small sample sizes [1]. It is as follows:", 
                     
                     tags$br(),
                     tags$br(),
                     
                     "\\[
                      AICc = -2*LL + (2*k) + \\frac{2*k*(k+1)}{n-k-1} \\tag{1}
                      \\]",
                     
                     tags$br(),
                     
                     "where", tags$em("LL"), " is the log-likelihood,", tags$em("k"),
                     " represents the number of parameters included in the model 
                     and the calibration period length is given by", tags$em("n"), ".", 
                     "The AICc for the model fit is directly available as part of
                     the", tags$em("auto.arima()"), " function [2]. However, for the 
                     GLM and GAM models, the AICc must be manually calculated 
                     using (1). The dashboard obtains the log-likelihood
                     directly from the GLM and GAM fits, utilizing the", tags$em("logLik()"), 
                     "function [3]. The AICc only applies to frequent 
                     methods, so the dashboard will not supply the value for 
                     the Prophet model, a Bayesian framework.", 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$b("References"), 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$ol(
                       
                       tags$li("Spiess AN, Neumeyer N. An evaluation of R2 as an inadequate measure for nonlinear models in pharmacological and biochemical research: a Monte Carlo approach. BMC Pharmacol. 2010 Jun 7;10:6. doi: 10.1186/1471-2210-10-6. PMID: 20529254; PMCID: PMC2892436."),
                       tags$li("Hyndman RJ. auto.arima: Fit best ARIMA model to univariate time series (Version 8.22.0). RDocumentation 2024. https://www.rdocumentation.org/packages/forecast/versions/8.22.0."),
                       tags$li("R-Core. logLik: Extract Log-Likelihood (Version 3.6.2). RDocumentation 2024. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/logLik"),
                       
                     ),
                     
                     tags$br(),
                     
                     ##################
                     # Link for paper #
                     ##################
                     div(style = "text-align: center; width: 100%; margin: 0 auto;",
                         
                         a("For further information, click here to learn more.",
                           href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                           style = "color: black; font-weight: 600;",
                           class = "button")
                         
                     ) # End of row for button link
                     
            ) # End of style for fluid page
            
          ) # Page containing information
          
        ) # Box containing information
        
      ) # End of fluidRow
      
    ), # End of "conditionalPanel" for AICc
    
    ###################
    # BIC Information #
    ###################
    conditionalPanel(
      
      # Condition to show the panel
      condition = "input.re_metric == 're_BIC'",
      
      # Fluid Row
      fluidRow(
        
        #######################
        # Box for information #
        #######################
        box(
          
          # Width of box
          width = 12,
          
          # Box color
          style = "background-color: #f8f8f8;", 
          
          # Page containing information
          fluidPage(
            
            # Font size 
            tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                     
                     # Allowing math equations to be shown in the panel 
                     withMathJax(), 
                     
                     # Page width 
                     width = 12, 
                     
                     ################
                     # Text to show #
                     ################
                     "The Bayesian information criterion (BIC), also known as 
                     Schwarz's Bayesian criterion (SBC), is frequently applied 
                     in the model selection process [1]. As with the Alkakie
                     Information Criterion (AIC), the BIC is available for any
                     model where the log-likelihood can be calculated [1]. It is 
                     given by the following [2]:",  

                     tags$br(),
                     tags$br(),
                     
                     "\\[
                      BIC = -2*LL + k*log(n) \\tag{1}
                      \\]",
                     
                     tags$br(),
                     
                     "where", tags$em("LL"), " is the log-likelihood,", tags$em("k"),
                     " represents the number of parameters included in the model 
                     and the calibration period length is given by", tags$em("n"), ".", 
                     "The BIC for the model fit is directly available as part of
                     the", tags$em("auto.arima()"), " function [3]. However, for the 
                     GLM and GAM models, the BIC must be manually calculated 
                     using (1). The dashboard obtains the log-likelihood
                     directly from the GLM and GAM fits, utilizing the", tags$em("logLik()"), 
                     "function [4]. The BIC only applies to frequent 
                     methods, so the dashboard will not supply the value for 
                     the Prophet model, a Bayesian framework.", 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$b("References"), 
                     
                     tags$br(),
                     tags$br(),
                     
                     tags$ol(
                       
                       tags$li("Doug. BIC: Bayesian Information Criterion (Version 0.999375-37). RDocumentation 2024. https://www.rdocumentation.org/packages/lme4/versions/0.999375-37/topics/BIC"),
                       tags$li("Spiess AN, Neumeyer N. An evaluation of R2 as an inadequate measure for nonlinear models in pharmacological and biochemical research: a Monte Carlo approach. BMC Pharmacol. 2010 Jun 7;10:6. doi: 10.1186/1471-2210-10-6. PMID: 20529254; PMCID: PMC2892436."),
                       tags$li("Hyndman RJ. auto.arima: Fit best ARIMA model to univariate time series (Version 8.22.0). RDocumentation 2024. https://www.rdocumentation.org/packages/forecast/versions/8.22.0."),
                       tags$li("R-Core. logLik: Extract Log-Likelihood (Version 3.6.2). RDocumentation 2024. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/logLik"),
                       
                     ),
                     
                     tags$br(),
                     
                     ##################
                     # Link for paper #
                     ##################
                     div(style = "text-align: center; width: 100%; margin: 0 auto;",
                         
                         a("For further information, click here to learn more.",
                           href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                           style = "color: black; font-weight: 600;",
                           class = "button")
                         
                     ) # End of row for button link
                     
            ) # End of style for fluid page
            
          ) # Page containing information
          
        ) # Box containing information
        
      ) # End of fluidRow
      
    ), # End of "conditionalPanel" for BIC
    
    ############################
    # Skill Scores Information #
    ############################
    conditionalPanel(
      
      # Condition to show the panel
      condition = "input.re_metric == 're_SS'",
      
      # Fluid Row
      fluidRow(
      
      #######################
      # Box for information #
      #######################
      box(
        
        # Width of box
        width = 12,
        
        # Box color
        style = "background-color: #f8f8f8;", 
        
        # Page containing information
        fluidPage(
          
          # Font size 
          tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                   
                   # Allowing math equations to be shown in the panel 
                   withMathJax(), 
                   
                   # Page width 
                   width = 12, 
                   
                   ################
                   # Text to show #
                   ################
                   
                   "Skills scores [1] provide one method of comparing the 
                   proportion of improvement of one model over another model
                   based upon MSE, MAE, WIS, and Winkler Scores (95% PI Coverage).
                   The formula is as follows [1]:", 
                   
                   tags$br(),
                   tags$br(),
                   
                   "\\[
                     \\frac{\\text{Baseline Model} -
                     \\text{Comparison Model}}{\\text{Baseline Model}} * 100 
                     \\tag{1}
                    \\]", 
                  
                   tags$br(),
                   
                   "The baseline model refers to the metric of the model which 
                   a user may want to compare the same metric for another model 
                   (i.e., comparison model) against.", 
                   
                   tags$br(),
                   tags$br(),
                   
                   tags$b("References"), 
                   
                   tags$br(),
                   tags$br(),
                   
                   tags$ol(
                     
                     tags$li("Hyndman RJ, Athanasopoulos G. Forecasting: principles and practice. 3rd ed. Australia: OTexts; 2018. https://otexts.com/fpp3/."),
                   
                     ),
                   
                   tags$br(),
                   
                   ##################
                   # Link for paper #
                   ##################
                   div(style = "text-align: center; width: 100%; margin: 0 auto;",
                       
                       a("For further information, click here to learn more.",
                         href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                         style = "color: black; font-weight: 600;",
                         class = "button")
                       
                   ) # End of row for button link
                   
          ) # End of style for fluid page
          
        ) # Page containing information
        
       ) # Box containing information
      
      ) # End of Fluid Row 
      
    ), # End of "conditionalPanel" for Skill Scores
    
    ##############################
    # Winkler Scores Information #
    ##############################
    conditionalPanel(
      
      # Condition to show the panel
      condition = "input.re_metric == 're_winkler'",
      
      # Fluid Row
      fluidRow(
      
      #######################
      # Box for information #
      #######################
      box(
        
        # Width of box
        width = 12,
        
        # Box color
        style = "background-color: #f8f8f8;", 
        
        # Page containing information
        fluidPage(
          
          # Font size 
          tags$div(style = "font-size: 17.5px; background-color: #f8f8f8;",
                   
                   # Allowing math equations to be shown in the panel 
                   withMathJax(), 
                   
                   # Page width 
                   width = 12, 
                   
                   ################
                   # Text to show #
                   ################
                   
                   "In place of 95% PI Coverage, the dashboard employs Winkler 
                    Scores in the skill scores calculations. Winkler scores are 
                    calculated as follows [1]:", 
                   
                   tags$br(),
                   tags$br(),
                   
                   "\\[
                     W_{\\alpha, t} =
                       \\begin{cases} 
                     (u_{\\alpha, t} - l_{\\alpha, t}) + \\frac{2}{\\alpha}(l_{\\alpha, t} - y_t), & \\text{if } y_t < l_{\\alpha, t} \\\\
                     (u_{\\alpha, t} - l_{\\alpha, t}), & \\text{if } l_{\\alpha, t} \\leq y_t \\leq u_{\\alpha, t} \\\\
                     (u_{\\alpha, t} - l_{\\alpha, t}) + \\frac{2}{\\alpha}(y_t - u_{\\alpha, t}), & \\text{if } y_t > u_{\\alpha, t}
                     \\end{cases}
                     \\]", 
                   
                   tags$br(),
                   
                   "where ", tags$em("u"), " and ", tags$em("l"), " refer to the 
                   upper and lower bounds of the 95% prediction interval 
                   (\\( \\alpha = 0.05 \\)) at time ", tags$em("t"), ".
                   The parameter, \\( y_t \\) represents the observed 
                   data at time ", tags$em("t"), ".", 
                   
                   tags$br(),
                   tags$br(),
                   
                   tags$b("References"), 
                   
                   tags$br(),
                   tags$br(),
                   
                   tags$ol(
                     
                     tags$li("Hyndman RJ, Athanasopoulos G. Forecasting: principles and practice. 3rd ed. Australia: OTexts; 2018. https://otexts.com/fpp3/."),
                     
                   ),
                   
                   tags$br(),
                   
                   ##################
                   # Link for paper #
                   ##################
                   div(style = "text-align: center; width: 100%; margin: 0 auto;",
                       
                       a("For further information, click here to learn more.",
                         href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4849702",
                         style = "color: black; font-weight: 600;",
                         class = "button")
                       
                   ) # End of row for button link
                   
            ) # End of style for fluid page
          
          ) # Page containing information
        
        ) # Box containing information
      
      ) # End of fluidRow
      
    ) # End of "conditionalPanel" for Winkler Scores 
    
  ) # End of conditionalPanel for Metrics

), # End of the "About" Page 


#------------------------------------------------------------------------------#
# Creating Page 2: Forecasting -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates all of the information shown on page one of the  #
# dashboard. Additionally, it creates the UI inputs located within each box    #
# of page 1. This is the primary page of the dashboard. It is where users      #
# select all of the forecasting options, and receive output related to the     #
# formatted forecasts, quantile forecasts, and time-series figures and data.   #
#------------------------------------------------------------------------------#
conditionalPanel(
  
  # Condition indicating the need for the 'forecasting' page to be selected 
  condition = "input.my_picker == 'Forecasting'", 

#------------------------------------------------------------------------------#
# Row 1: Creating the formatted forecast data sets and figures -----------------
#------------------------------------------------------------------------------#
# About: This section creates the UI side allowing users to obtain forematted  #
# forecast data sets and associated figures (individual and panels).           #
#------------------------------------------------------------------------------#

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
                 
                 # Plotting the forecast plot 
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
                       div(style = "margin-right: 10px;", actionButton("filterFormattedForecasts", "Data Options")),
                       
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
                       
                     ) # End of conditional panel for the "See Data" 
                     
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
# Second Row: Quantile Forecasts -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates creates the UI for obtaining the quantile        #
# version of the forecasts, including the point median forecast. The results   #
# can be downloaded by the user.                                               #
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
             
             ###################################################
             # Row 1: Title customized based on model selected #
             ###################################################
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
          
            ########################################################################
            # Row 3: Creating the options - Download button, locations, and arrows #
            ########################################################################
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
# Row 3: Time-series Plot ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the UI for obtaining the time-series plot and    #
# associated data. Users can customize the time-series figures, and download   #
# the data to their personal computer.                                         #
#------------------------------------------------------------------------------#

  ######################################################
  # Creating the row for the time-series plot and data #
  ######################################################
  fluidRow(
    
    # Width of row 
    width = 12,
    
    # Alignment Column
    column(
      
      # Width of column 
      width = 12,
    
      ##################################################################
      # Box 1: Creating the box which will show the time-series figure #
      ##################################################################
      box(
        
        # Width of the box 
        width = 12, 
        
        # Title of bo 
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
          
        ), # End of row containing the time-series plot figure  
        
        ############################################
        # Row for Buttons: Download and Check-Boxs #
        ############################################
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
      
        ############################################################
        # Box 2: Showing the data rather than the time series plot #
        ############################################################
        conditionalPanel(
          
          # Condition
          condition = "input.timeseriesCheckBox",
            
          ##########################################
          # Row that contains the time-series data #
          ##########################################
          fluidRow(
            
            # Width of row
            width = "100%",
            
            ####################
            # Alignment column #
            ####################
            column(
              
              # Width
              width = 12, 
              
              # Showing the time-series data  
              dataTableOutput("timeseries") , 
                
              # Style for the download time-series button 
              div(style = "display: flex; justify-content: flex-start; align-items: center;",
                  uiOutput("downloadTimeseries")))
            
            ) # End of row showing time-series data 
        
         ) # End of conditional panel for showing the time-series data
      
      ) # End of box containing time-series data 
    
    ) # End of alignment column
  
  ) # End of row containing time-series data
      
), # End of conditional panel creating page 1: "Forecasting"


#------------------------------------------------------------------------------#
# Creating Page 3: Forecasting and Model Fit Metrics ---------------------------
#------------------------------------------------------------------------------#
# About: This section creates the user-interface for the third page of the     #
# dashboard. This includes all of the performance metrics related to the model #
# fits and subsequent forecasts. The top portion of the page contains the      #
# metrics averaged across the forecast periods, the middle contains the crude  #
# metrics (i.e., for each forecast date), and the last box contains the        #
# Winkler and Skill Scores.                                                    #
#------------------------------------------------------------------------------#

conditionalPanel(
  
    # Condition 
    condition = "input.my_picker == 'Model Metrics'", 
    
    #########################################
    # Setting a fluidRow for the whole page #
    #########################################
    fluidRow(
    
    ######################################################
    # Setting a standard column width for the whole page #
    ######################################################
    column(
      
      # Column width 
      width = 12,
      
#------------------------------------------------------------------------------#
# Row 3A: The average metrics --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the box and all options associated with the      #
# top-box, which contains the average performance metrics for the selected     #
# models. It housing the data, figures, and all options and buttons (arrows)   #
# to navigate through figures when necessary.                                  #
#------------------------------------------------------------------------------#
  
    ####################################
    # Creating the top-row of the page #
    ####################################
    fluidRow(
      
      ##################################################
      # Creating the box-shell for the average metrics #
      ##################################################
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
              textOutput("AvgMetricsFigureTitle")
              
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
                
                ########################################################
                # Conditional Panel: Download data & Filtering Options #
                ########################################################
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
                      
                  ) # End of style for conditional panel 
                  
                ), # End of condition
                
                ###########################################################
                # Conditional Panel: Download Figure & Figure edit button #
                ###########################################################
                conditionalPanel(
                  
                  # Condition
                  condition = "input.AvgFigure", 
                  
                  # Aligning buttons
                  div(style = "display:flex; vertical-aline: top",
                      
                      #Download Button
                      div(style = "margin-right: 10px",
                          actionButton("download_AvgmetricsFig", "Download Average Metrics Figure", icon = icon("download"))),
                      
                      # Edit figures button
                      div(style = "margin-right: 10px", actionButton("editAvgMetrics", "Figure Options")))
                  
                ),
                
                #########################
                # Show figure check-box #
                #########################
                div(checkboxInput("AvgFigure", "Show Figure"))
                
            ), # End of main style of the contents of the box 
            
          ), # End of column for left-hand buttons
          
          ##########################################
          # Condition Panel: Figure Metrics Arrows #
          ##########################################
          conditionalPanel(
            
            # Condition
            condition = "input.AvgFigure",
            
            # Left and right arrows
            column(2,
                   div(style = "display: flex; justify-content: flex-end; align-items: center;",
                       actionButton(inputId = "PreviousAMetricFigure", label = icon("arrow-left")),
                       actionButton(inputId = "NextAMetricFigure", label = icon("arrow-right"))))
            
          ) # End of conditional panel
          
        ) # End of 'fluidRow' for buttons 
        
      ) # End of 'column' for average metrics box 
      
    ), # End of fluid row for first row of page
  
  
#------------------------------------------------------------------------------#
# Row 3B: The crude metrics ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the box and all options associated with the      #
# middle-box, which contains the crude performance metrics for the selected    #
# models. It housing the data, figures, and all options and buttons (arrows)   #
# to navigate through figures when necessary.                                  #
#------------------------------------------------------------------------------#
  
    #################################################
    # Creating the top-row of the crude metrics box #
    #################################################
    fluidRow(
      
      ####################################################
      # Creating a box-shell for crude model fit metrics #
      ####################################################
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
                
                #############################################################
                # Condition: Show Download Button for Data & Filter Options #
                #############################################################
                conditionalPanel(
                  
                  # Condition
                  condition = "!input.crudeFigure",
                  
                  # Style 
                  div(style = "display:flex; vertical-aline: top",
                      
                      # Download Button
                      div(style = "margin-right: 10px",
                          downloadButton("download_metrics", "Download Crude Metrics")),
                      
                      # Filtering data 
                      div(style = "margin-right: 10px;", actionButton("filterCrudeMetrics", "Filtering Options"))    
                      
                  )
                  
                ), # End of condition
                
                ###############################################################
                # Condition: Show Download Button for Figure & Figure Options #
                ###############################################################
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
# Row 3C: Winkler Scores and Skill Scores --------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the tabbed last box, which contains both the     #
# Winkler Scores and the Skill Scores. Each is included within their own tabs  #
# and contain their own options. Users can easily switch between tabs to see   #
# both metrics, filter the data, calculate the average metrics, and download   #
# the data as `.csv` files.                                                    #
#------------------------------------------------------------------------------#
  
    ###########################################
    # Creating the row to host the tabbed box #
    ###########################################
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
                   
                 ) # End of Row 2 creating user options 
                 
        ), # End of Winkler data tab 
        
        #########################################
        # Creating the box for Skill Score Data #
        #########################################
        tabPanel(id = "SkillScoresData",
                 
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
                 
        ) # End of the skill scores data tab 
        
      ) # End of the tabbed box containing skill scores and Winkler scores 
      
    ) # End of row hosting the third box
  
  ) # End of alignment column 

 ) # End of 'fluidRow'
  
), # End of 'conditionalPanel' for Page 3, Model Metrics 


#------------------------------------------------------------------------------#
# Creating Page 4: Model Comparison --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the user-interface for the fourth page of the    #
# dashboard, which allows users to read in outside models and metrics and      #
# compare them against the models included in the dashboard. The top row of    #
# the dashboard shows the forecast figures, the middle includes the crude      #
# and average metrics, and the last row shows the Winkler and Skill Scores.    #
# There are a few side-bar options on this page as well, where users can read  #
# in the outside models and metrics.                                           #
#------------------------------------------------------------------------------#                   
                
conditionalPanel(
  
    # Condition to show the last page 
    condition = "input.my_picker == 'Model Comparison'", 
    
  
#------------------------------------------------------------------------------#
# Row 4A: Individual and Panel forecast figures --------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the individual and panel forecast figures for    #
# the other model forecast files and the dashboard forecasts (panel only).     #
# Additionally, this section host all of the available button and options,     #
# such as the download buttons, figure editing, filtering, and arrows to       #
# navigate through images.                                                     #
#------------------------------------------------------------------------------#

    # Fluid Row to host box
    fluidRow(

    ###############################################
    # Creating the box-shell for forecast figures #
    ###############################################
    box(
      
      # Box title 
      title = NULL, 
      
      # Width of the box 
      width = 12, 
      
      #############################################################
      # Row 1: Rendering the individual or panel forecast figures #
      #############################################################
      fluidRow(
        
        ####################
        # Alignment column #
        ####################
        column(
          
          width = 12, 
          
          ##############################################
          # Conditional Panel: Show Individual Figures #
          ##############################################
          
          # Plot title
          textOutput("OtherForecastTitle"), 
          
          # Rendering the data frame
          plotOutput("otherModelFigure")
          
        ) # End of alignment column 
        
      ), # End of row one alignment column 
      
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
                  div(style = "margin-right: 10px", uiOutput("otherFigureOptions")), 
                  
                  # Showing the panel figures check box
                  div(checkboxInput("showPanelPlotOther", "Show the Panel Figures", value = F))
                  
              ), # End of style for buttons
              
              #######################
              # Creating the arrows #
              #######################
              div(
                
                style = "display: flex; justify-content: flex-end; align-items: center;",
                actionButton(inputId = "otherFigsPrevious", label = icon("arrow-left")),
                actionButton(inputId = "otherFigstNext", label = icon("arrow-right"))
                
              ) # End of style for buttons 
              
            ) # End of overall row style 
            
          ) # End of alignment column
          
        ) # End of Row 2 - User Options
        
      ) # End of box for forecast figures 
    
    ), # End of Row


#------------------------------------------------------------------------------#
# Row 4B: Average Metrics ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the box that contains the average metrics data   #
# and the associated figures based upon the user-entered performance metrics   #
# and metrics obtained from the dashboard. It is a tabbed box, where the first #
# box contains the combined data set with all metrics and the second tab       #
# contains the associated figures. This section also creates the buttons and   #
# options available to the user, such as download options, filtering data,     #
# and editing figures.                                                         #
#------------------------------------------------------------------------------#

    # Row
    fluidRow(
  
    #################################
    # Creating the tabbed box shell #
    #################################
    tabBox(
      
        # Box title 
        title = NULL, 
        
        # ID of the box 
        id = "box2",
        
        # Width of the box 
        width = 12, 
      
        #########################################
        # Creating the average metrics Data Tab #
        #########################################
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
                     
                   ) # End of alignment column 
                   
                 ), # End of row for data table 
                 
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
                         
                         # Download button for the combined average metrics 
                         div(downloadButton("downloadAverageMetrics", "Download Avg. Metrics", style = "margin-right: 10px")),
                         
                         # Filtering for the average metrics 
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
                     
                   ) # End of alignment column 
                   
                 ), # End of row creating the figure 
                 
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
                        
                           # Download button for the combined average metrics figure
                           div(actionButton("downloadOtherCombinedMetricsFigAvg", "Download Figures", style = "margin-right: 10px")),
                           
                           # Filtering for the average metrics 
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
                 
      ), # End of 'tabPanel' for the average metrics figure 
      
    ) # End of 'tabbox' for the average metrics 
    
  ), # End of fluidRow 
    

#------------------------------------------------------------------------------#
# Row 4C: Crude Metrics --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the box that contains the crude metrics data     #
# and the associated figures based upon the user-entered performance metrics   #
# and metrics obtained from the dashboard. It is a tabbed box, where the first #
# box contains the combined data set with all metrics and the second tab       #
# contains the associated figures. This section also creates the buttons and   #
# options available to the user, such as download options, filtering data,     #
# and editing figures.                                                         #
#------------------------------------------------------------------------------#

  # Fluid Row
  fluidRow(

  ###################################################
  # Creating the tabbed-box shell for crude metrics #
  ###################################################
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
                   
                 ) # End of alignment column 
                 
                 ), # End of row with data 
               
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
                   
                 ) # End of alignment column 
                 
               ), # End of row with for crude metrics figures 
               
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
                         
                     ), # End of style for download and filter buttons 
                   
                     #############################################
                     # Creating the forward and backwards arrows #
                     #############################################
                     div(
                       
                       style = "display: flex; justify-content: flex-end; align-items: center;",
                       actionButton(inputId = "otherCrudeMetricPanelsPrevious", label = icon("arrow-left")),
                       actionButton(inputId = "otherCrudeMetricPanelsNext", label = icon("arrow-right"))
                       
                     ) # End of style for buttons 
                     
                   ) # End of style for row 
                   
                 ) # End of alignment column 
                 
               ) # End of row with figure options 
               
      ) # End of 'tabPanel' for the crude metrics figure 
               
    ) # End of 'tabbox' for the crude metrics 
  
  ), # End of fluidRow
  

#------------------------------------------------------------------------------#
# Row 4D: Winkler Scores and Skill Scores --------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the tabbed last box, which contains both the     #
# Winkler Scores and the Skill Scores. Each is included within their own tabs  #
# and contain their own options. Users can easily switch between tabs to see   #
# both metrics, filter the data, calculate the average metrics, and download   #
# the data as `.csv` files.                                                    #
#------------------------------------------------------------------------------#

  # Fluid row
  fluidRow(

  #########################################################
  # Creating the table shell for winkler and skill scores #
  #########################################################
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
                 
               ) # End of alignment column 
               
             ), # End of row for Winkler Scores rendering 
             
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
                     
                     # Download button for the combined Winkler Scores 
                     div(style = "margin-right: 10px", downloadButton("downloadWinklerMetrics", "Download Winkler Scores")),
                     
                     # Filtering for the combined Winkler Scores 
                     div(style = "margin-right: 10px", actionButton("filterWinklerOtherMetrics", "Filtering Options")), 
                     
                     # Check-mark for average Winkler scores
                     div(checkboxInput("winklerOtherAvg", "See Avg. Winkler Scores"))
                     
                 ) # End of overall style for row 
                 
               ) # End of alignment column 
               
             ) # End of row with filtering options 
             
    ), # End of 'tabPanel' for the combined Winkler Scores 
    
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
                 
               ) # End of alignment column 
               
             ), # End of row for data table
             
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
                 
          ) # End of 'tabPanel' for the skill scores data 
          
        ) # End of 'tabbox' for the skill scores
  
      ) # End of fluidRow 
                       
    ) # End of other page 
        
  ) # End of dashboard body
  
) # End of user interface 

#------------------------------------------------------------------------------#
# SERVER -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# The remainder of this code creates the back-bone to the features of the      #
# toolbox. It calls most of the functions included in the GITHUB and provides  #
# the extensive customizations provided within the toolbox.                    #
#------------------------------------------------------------------------------#
server <- function(input, output, session) {
  

#------------------------------------------------------------------------------#
# Creating the start up message ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This creates the start-up message when users first open the app. It   #
# welcomes users to the app, provides an overview of features, and points      #
# users to the about page for set-up.                                          #
#------------------------------------------------------------------------------#
  
  ######################################
  # Show the alert when the app starts #
  ######################################
  shinyalert(
    
    # Welcome message
    title = "Welcome!",
    
    # Enabling html rendering
    html = TRUE,
    
    # Size of modal
    size = "m", 
    
    # Creating the text message
    text = HTML(
      
      "Welcome to StatModPredict, a user-friendly R-Shiny interface for fitting,
       forecasting, evaluating, and comparing the results from ARIMA, GLM, GAM, 
       and Facebook's Prophet models. Primary functions of the toolbox include:
       
       <br><br>
       
       <ul>
         <li>Customization of forecasting and modeling parameters</li>
         <li>Obtainment of model fits and forecasts</li>
         <li>Model evaluation criteria</li>
         <li>Visualizations</li>
         <li>Comparison against 'outside' models</li>
       </ul>
       
       <br>
       
       The toolbox requires only time series data, and additional details about 
       the available features and set-up can be found on our 'About Page'. "
    ),
    
    type = "info"
    
  )
  
#------------------------------------------------------------------------------#
# Events to clear output -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the reactive value with triggers the clearing of #
# output and underlying code on the main dashboard page. The two events are    #
# the changing of data and when the clear button is clicked.                   #
#------------------------------------------------------------------------------#
  
  clearingOut <- reactive({return(list(file(), input$clearResults, input$locations,
                                       input$modelType, input$forecastHorizon,
                                       input$forecast.period))})
  
#------------------------------------------------------------------------------#
# Resetting some side-bar values -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section resets the model picker when the calibration period      #
# lengths is changes. This occurs to reset the basis function associated with  #
# the GAM model.                                                               #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Observing the change in calibration period #
  ##############################################
  observeEvent(input$calibrationPeriod, ignoreInit = TRUE, {

    # Isolating the event to the button click 
    isolate({
      
      # Updating the pickerInput 
      updatePickerInput(session, "modelType", selected = character(0), choices = c("ARIMA", "GLM", "GAM", "Prophet"))  # Reset to no selection
      
      })
    
    }) # End of 'observeEvent' 
  
  
#------------------------------------------------------------------------------#
# Producing error if no model is not selected ----------------------------------
#------------------------------------------------------------------------------#
# About: This section returns an error if the user has hit run but no model is #
# selected.                                                                    #
#------------------------------------------------------------------------------#
  
  ######################################
  # Observing the run button being hit #
  ######################################
  observeEvent(input$run, {
    
    # Requiring a location to be selected 
    req(input$locations)
    
    # Checking the length of the model input
    if(length(input$modelType) == 0){

      # Alert to show
      shinyalert("Please select at least one model prior to hitting the `Run`
      button.", type = "error")

    }

  })
  
#------------------------------------------------------------------------------#
# Error to appear when using smoothing and Prophet model -----------------------
#------------------------------------------------------------------------------#
# About: This section returns a message if smoothing data and have the model   #
# Prophet selected. Due to the structure of the needed model input, smoothing  #
# is not available.                                                            #
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

        # Error to show
        shinyalert("The data smoothing will not be applied to the calibration period
                   for the Prophet model. It will apply for any other selected model.",
                   type = "warning")

        # Returning null if no error occurs
        }else{

        NULL

      } # End of if-else for error

    ###########################
    # Returns if error occurs #
    ###########################
    }, error = function(e){

      NULL

    }) # End of tryCatch

  }) # End of "observe"
  
#------------------------------------------------------------------------------#
# Error to appear when using smoothing  ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns a message if smoothing data is selected. The     #
# message lets the user know that the model fits for the selected models will  #
# not be available, including the MSE, MAE, PI, and WIS metrics.               #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    ###############################
    # Running if not error occurs #
    ###############################
    tryCatch({
      
      # Returning error if smoothing 
      if(input$smoothingInput > 1 & !is.null(input$modelType)){
        
        # Models for error
        modelsError <- input$modelType[input$modelType != "Prophet"]
        
        # Error to show
        shinyalert("Model fits and some fit metrics (MSE, MAE, WIS, PI) are not available for the 
                   following models due to the smoothing transformation: ", c(modelsError), 
                   type = "warning")
        
      # Returning null if no error occurs
      }else{
        
        NULL
        
      } # End of if-else for error
      
    ###########################
    # Returns if error occurs #
    ###########################
    }, error = function(e){
      
      NULL
      
    }) # End of tryCatch
    
  }) # End of "observe"
  
   
#------------------------------------------------------------------------------#
# Reading in the data frame ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section reads in the user-selected data frame from the  #
# working directory. It then saves the data under the reactive element 'file'. #   
# The file is selected using the 'fileInput' picker.                           #
#------------------------------------------------------------------------------#
  
  ###############################
  # Reactive value to load data #
  ###############################
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
      
      # Runs if no error occurs
      }else{
      
      NULL
      
    } # End of if-else checking for error 
    
    #######################
    # Reading in the data #
    #######################
    return(read.csv(file1$datapath, header = T, check.names=FALSE))
    
    ###########################
    # Runs if an error occurs #
    ###########################
    }, error = function(e){
      
      NULL
      
    }) # End of "try-catch"
    
  }) # End of "observe"
  
#------------------------------------------------------------------------------#
# Checking location names ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the file column headers to ensure they do not     #
# include the "-" character, as it causes issues later in the code.            #
#------------------------------------------------------------------------------#
  

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the file
    req(file())
    
    #########################################
    # Pulling the column names and checking #
    #########################################
    
    # Pulling the data
    dataFile <- file()
    
    dataTest <- dataFile
    
    # Pulling the names
    groupNames <- c(colnames(dataTest)[-1])
    
    # Checking for "-"
    has_dash <- any(grepl("-", groupNames))
    
    ################
    # Error return #
    ################
    if(has_dash){
      
      # Error to show
      shinyalert("At least one of the column names in the selected data contain a special character (i.e., "-"). Please remove all special characters from the column names and reload your data.", 
                 type = "warning")
      
    ###################
    # No error return #
    ###################
    }else{
      
      NULL
      
    }
    
  })
  
  
#------------------------------------------------------------------------------#
# Adding locations filter to the top of the page -------------------------------
#------------------------------------------------------------------------------#
# This section uses the data from above to determine the possible locations to #
# include in the drop down filter. Users can select more than one location,    #
# and the respective boxes will include information only for those locations.  #
#------------------------------------------------------------------------------#
  
  output$location.selection <- renderUI({
    
    # Requiring the file
    req(file())
    
    ##########################
    # Creating the drop down #
    ##########################
    return(pickerInput("locations", # ID for calling picker 
                       label = "Location/Group", # Label for picker 
                       choices = c(colnames(file())[-1]), # Location choices 
                       selected = NULL, 
                       multiple = T, # Allowing more than one choice 
                       options = list(`actions-box` = TRUE))) # Creating the select-all
    
    
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

    # Requiring the input file
    req(file())

    # Data file
    data <- file()

    # Sub-setting dates
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

    } # End of if-else checking for date error

  }) # End of 'observe' statement

#------------------------------------------------------------------------------#
# Creating the download button: Time series Figure -----------------------------
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

  ##############################################
  # Reactive value to save time series data in #
  ##############################################
  timeseriesData <- reactiveValues()

  #############################################
  # Making changes when the run button is hit #
  #############################################
  observeEvent(input$run, {

    # Requiring the file 
    req(file())

    ################################
    # Reading in the original data #
    ################################
    data <- file()

    # Reading in the selected locations #
    locationsTest <- input$locations

    # Creating the time-series data 
    isolate({data.for.table <- data %>%
                  dplyr::select(names(data)[1], all_of(locationsTest))})

    ############################################
    # Saving the results to the reactive value #
    ############################################
    isolate({timeseriesData$dataList <- data.for.table})

  }) # End of 'observeEvent'


#------------------------------------------------------------------------------#
# Clearing the time series data   ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the time series data if the original data set is  #
# changed, locations are changed, or any other settings are shifted.           #
#------------------------------------------------------------------------------#

  ########################################
  # Observing a change in the file input #
  ########################################
  observeEvent(clearingOut(), {

    # Resetting the time series data
    timeseriesData$dataList <- NULL

  })

  #############################################
  # Observing a change in the locations input #
  #############################################
  observeEvent(input$locations, {

    # Resetting the time series data
    timeseriesData$dataList <- NULL

  })

#------------------------------------------------------------------------------#
# Rendering the data table for the time series ---------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the data table if the 'check-mark' is hit to     #
# show the data frame.                                                         #
#------------------------------------------------------------------------------#

  ####################
  # Render statement #
  ####################
  output$timeseries <- renderDataTable({

    # Returning the data
    return(timeseriesData$dataList)

    })


#------------------------------------------------------------------------------#
# Downloading the time series data as a '.csv' ---------------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the time series data as a '.csv' file to the        #
# directory of their choosing.                                                 #
#------------------------------------------------------------------------------#

  ###########################################
  # Backbone of download time series button #
  ###########################################
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
# Creating the time-series figure customization options ------------------------
#------------------------------------------------------------------------------#
# About: This section creates the figure customization options for the time-   #
# series figure. While the time-series is created with pre-set defaults, there #
# are multiple options which the user can customize.                           #
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
  dateBreaksReactive <- reactiveValues(breaksDate = "3")

  #############################################
  # Reactive value for showing forecast lines #
  #############################################
  forecastLinesReactive <- reactiveValues(indicator = F)

  ########################################
  # Reactive value for including a title #
  ########################################
  titleTimeseriesReactive <- reactiveValues(titleMain = NULL)

  ###########################################
  # Reactive value for main title face type #
  ###########################################
  titleFaceReactive <- reactiveValues(faceTitle = "Plain")

  ######################################
  # Reactive value for main title size #
  ######################################
  titleSizeReactive <- reactiveValues(size = 10)

  #############################################
  # Reactive value for main title orientation #
  #############################################
  titleOrienReactive <- reactiveValues(location = "Center")

  ########################################
  # Reactive value for y-axis label size #
  ########################################
  yAxisLabelSizeReactive <- reactiveValues(size = 10)

  #############################################
  # Reactive value for y-axis label face type #
  #############################################
  yAxisFaceReactive <- reactiveValues(faceTitle = "Plain")

  ####################################
  # Reactive value for y-axis breaks #
  ####################################
  yAxisBreaks <- reactiveValues(breaks = 11)

  ###################################
  # Reactive value for y-axis start #
  ###################################
  yAxisStart <- reactiveValues(start = "Zero")

  #######################################
  # Reactive value for y-axis tick size #
  #######################################
  yAxisTickSize <- reactiveValues(size = 10)

  #######################################
  # Reactive value for the x-axis label #
  #######################################
  XAxisLab <- reactiveValues(lab = "")

  ############################################
  # Reactive value for the x-axis label size #
  ############################################
  xAxisLabelSizeReactive <- reactiveValues(size = 10)

  #############################################
  # Reactive value for x-axis label face type #
  #############################################
  xAxisFaceReactive <- reactiveValues(faceTitle = "Plain")

  #############################################
  # Reactive value for x-axis tick label size #
  #############################################
  xAxisTickSizeReactive <- reactiveValues(size = 10)

  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editLegendLabels, {

    # Creating the pop-up
    showModal(modalDialog(

      # Title
      title = "Figure Options",

      # Creating the tabbed menu
      tabsetPanel(

        #############
        # Title Tab #
        #############
        tabPanel("Figure Title",
                 textInput("titleTimeseries", label = "Title of Figure", value = titleTimeseriesReactive$titleMain), # Main title
                 pickerInput("titleTimeseriesFace", label = "Font Face", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = titleFaceReactive$faceTitle), # Face of main title
                 numericInput("titleTimeseriesSize", label = "Text Size", value = titleSizeReactive$size), # Main title size
                 pickerInput("titleTimeseriesOrien", label = "Title Orientation", choices = c("Left", "Center", "Right"), selected = titleOrienReactive$location), # Main title orientation
                 ),

        ##############
        # Y-Axis tab #
        ##############
        tabPanel("Y-Axis",
                 tags$h4("Primary Label Options"),
                 textInput("yaxisTimeSeries", "Label for Y-Axis", value = yAxisLab$lab), # Label for y-axis
                 numericInput('yaxisTimeseriesLabelSize', "Label Text Size", value = yAxisLabelSizeReactive$size), # Size of main y-axis label
                 pickerInput("yaxisTimeseriesLabelFace", label = "Font Face", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = yAxisFaceReactive$faceTitle), # Face of main y-axis label
                 tags$h4("Axis Tick Label Options"),
                 numericInput("yAxisTimeseriesTickSize", label = "Tick Label Size", value = yAxisTickSize$size), # Size of tick labels
                 tags$h4("Axis Options"),
                 pickerInput("yAxisTimeseriesStart", label = "Y-Axis Start", choices = c("Minimum Value in Data", "Zero"), select = yAxisStart$start), # Start point for tick labels
                 numericInput("yAxisTimeseriesBreaks", label = "Y-Axis Breaks", value = yAxisBreaks$breaks), # Number of breaks
                 pickerInput("logScale", "Scale for Y-Axis", c("Original", "Log(Base 10)"), selected = scaleY$logScale) # Scale of y-axis
        ),

        ##############
        # X-Axis tab #
        ##############
        tabPanel("X-Axis",
                 tags$h4("Primary Label Options"),
                 textInput("XaxisTimeSeries", "Label for X-Axis", value = XAxisLab$lab), # Label for x-axis
                 numericInput('XaxisTimeseriesLabelSize', "Label Text Size", value = xAxisLabelSizeReactive$size), # Size of main X-axis label
                 pickerInput("XaxisTimeseriesLabelFace", label = "Font Face", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = xAxisFaceReactive$faceTitle), # Face of main x-axis label
                 tags$h4("Axis Tick Label Options"),
                 textInput("dateBreaksTS", "Number of Date Breaks (Label)", value = dateBreaksReactive$breaksDate), # Number of date breaks
                 numericInput("axisTickSizeX", label = "Tick Label Size", value = xAxisTickSizeReactive$size) # Size of tick labels
        ),

        #############
        # Lines tab #
        #############
        tabPanel("Lines",
                 checkboxInput("forecastLines", "Show Forecast Dates", value = forecastLinesReactive$indicator) # Show forecast lines
        )

        ), # End of menu

      # Allowing for the user to click anywhere to close the menu
      easyClose = TRUE,

    )) # End of module

  }) # End of observe event


  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScale, {

    # Updating the scale
    scaleY$logScale <- input$logScale

  })

  ################################################################
  # Update the reactive value - Y-axis for the time-series label #
  ################################################################
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

  ##########################################
  # Update the reactive value - Main title #
  ##########################################
  observeEvent(input$titleTimeseries,{

    # Update the text
    titleTimeseriesReactive$titleMain <- input$titleTimeseries

  })

  ###############################################
  # Update the reactive value - Main Title Face #
  ###############################################
  observeEvent(input$titleTimeseriesFace,{

    # Update the text face
    titleFaceReactive$faceTitle <- input$titleTimeseriesFace

  })

  ###############################################
  # Update the reactive value - Main Title Size #
  ###############################################
  observeEvent(input$titleTimeseriesSize,{

    # Update the text size
    titleSizeReactive$size <- input$titleTimeseriesSize

  })

  ######################################################
  # Update the reactive value - Main Title Orientation #
  ######################################################
  observeEvent(input$titleTimeseriesOrien,{

    # Update the title orientation
    titleOrienReactive$location <- input$titleTimeseriesOrien

  })


  #################################################
  # Update the reactive value - Y-Axis Label Size #
  #################################################
  observeEvent(input$yaxisTimeseriesLabelSize,{

    # Update the size of the Y-Axis label
    yAxisLabelSizeReactive$size <- input$yaxisTimeseriesLabelSize

  })

  ############################################
  # Update the reactive values - Y-Axis Face #
  ############################################
  observeEvent(input$yaxisTimeseriesLabelFace,{

    # Update the face of the Y-Axis label
    yAxisFaceReactive$faceTitle <- input$yaxisTimeseriesLabelFace

  })

  #############################################
  # Update the reactive value - Y-Axis Breaks #
  #############################################
  observeEvent(input$yAxisTimeseriesBreaks,{

    # Update the the number of breaks in axis
    yAxisBreaks$breaks <- input$yAxisTimeseriesBreaks

  })

  ############################################
  # Update the reactive value - Y-Axis Start #
  ############################################
  observeEvent(input$yAxisTimeseriesStart,{

    # Update the start of the y-axis
    yAxisStart$start <- input$yAxisTimeseriesStart

  })

  ######################################################
  # Update the reactive value - Y-Axis Tick Label Size #
  ######################################################
  observeEvent(input$yAxisTimeseriesTickSize,{

    # Update the size of the tick labels
    yAxisTickSize$size <- input$yAxisTimeseriesTickSize

  })

  ################################################################
  # Update the reactive value - X-axis for the time-series label #
  ################################################################
  observeEvent(input$XaxisTimeSeries, {

    # Updating the x-axis label
    XAxisLab$lab <- input$XaxisTimeSeries

  })

  #####################################################################
  # Update the reactive value - X-axis for the time-series label size #
  #####################################################################
  observeEvent(input$XaxisTimeseriesLabelSize, {

    # Updating the x-axis label size
    xAxisLabelSizeReactive$size <- input$XaxisTimeseriesLabelSize

  })

  #####################################################################
  # Update the reactive value - X-Axis for the time-series label face #
  #####################################################################
  observeEvent(input$XaxisTimeseriesLabelFace, {

    # Updating the x-axis label face
    xAxisFaceReactive$faceTitle <- input$XaxisTimeseriesLabelFace

  })

  ######################################################
  # Update the reactive value - X-Axis tick label size #
  ######################################################
  observeEvent(input$axisTickSizeX, {

    # Updating the x-axis tick size
    xAxisTickSizeReactive$size <- input$axisTickSizeX

  })

#------------------------------------------------------------------------------#
# Built-in location error ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the location error that occurs when the 'Run'    #
# button is clicked an no location is selected.                                #
#------------------------------------------------------------------------------#

  #################################################
  # Returning an error if no location is selected #
  #################################################
  observeEvent(input$run, {

    # Requiring the data tot be loaded
    req(file())

    # Checking length of locations
    lengthLocations <- length(input$locations)

    #################################
    # Returning the error if needed #
    #################################
    if(lengthLocations == 0 || is.null(input$locations)){

      # Producing the error
      isolate({shinyalert("No location has been selected. Please select at least one location prior to running forecasts.",
                          type = "error")})

      # Nulling out the time series figure
      isolate(timeseriesFigureList$figureInteractive <- NULL)

    }else{

      NULL

    }

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

      ##############################################
      # Function to produce the time-series figure #
      ##############################################

      # Running if times series data exist
      if(!is.null(timeseriesData$dataList)){

        # Requiring the locations
        req(input$locations)
        
        # Requiring a model to be selected
        req(input$modelType)

        # Function
        timeseriesFigure <- timeseries.figure.function(timeseries.input = timeseriesData$dataList, # Time-series data
                                                       location.input = c(input$locations), # Locations
                                                       dateType.input = dateValues$dates, # Type of data
                                                       forecastLineShow = forecastLinesReactive$indicator, # Show forecast lines
                                                       forecastDatesStart = input$forecast.period[1], # Start of slider
                                                       forecastDatesEnd = input$forecast.period[2], # End of slider
                                                       scaleYAxis = scaleY$logScale, # Scale for y-axis
                                                       yAxisLabel = yAxisLab$lab, # Y-axis label
                                                       dateBreaks = dateBreaksReactive$breaksDate, # Number of date breaks
                                                       mainTitle = titleTimeseriesReactive$titleMain, # Main title
                                                       mainTitleFace = titleFaceReactive$faceTitle, # Main title face
                                                       mainTitleSize = titleSizeReactive$size, # Main title size
                                                       mainTitleOrien = titleOrienReactive$location, # Main title orientation
                                                       yAxisTitleSize = yAxisLabelSizeReactive$size, # Y-axis label size
                                                       yAxisTitleFace = yAxisFaceReactive$faceTitle, # Y-Axis label face
                                                       yAxisBreaks = yAxisBreaks$breaks, # Y-Axis breaks
                                                       yAxisStartVal = yAxisStart$start, # Y-Axis start value
                                                       yAxisTickSize = yAxisTickSize$size, # Y-Axis tick label size
                                                       xAxisLabel = XAxisLab$lab, # X-axis label
                                                       xAxisLabelSize = xAxisLabelSizeReactive$size, # X-Axis label size
                                                       xAxisLabelFace = xAxisFaceReactive$faceTitle, # X-Axis label face
                                                       xAxisTickSize = xAxisTickSizeReactive$size # X-Axis tick label size
                                                      )

      }

      if(!is.null(timeseriesFigure)){

      # Saving figure list to reactive value variable - Plotly figure
      timeseriesFigureList$figureInteractive <- timeseriesFigure[[1]]

      # Saving the non-reactive ggplot figure to the second list
      timeseriesFigureList$figureStatic <- timeseriesFigure[[2]]

      }

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
# About: This section clears the time series image if the original data set is #
# changed.                                                                     #
#------------------------------------------------------------------------------#

  observeEvent(clearingOut(), {

    # Resetting the interactive form of the image
    timeseriesFigureList$figureInteractive <- NULL

    # Resetting the static form of the image
    timeseriesFigureList$figureStatic <- NULL

  })

#------------------------------------------------------------------------------#
# Rendering the time series image ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the time series figure to the main dashboard.    #
#------------------------------------------------------------------------------#
  
  output$timeseriesPlot <- renderPlotly({

    ##################
    # Plot to return #
    ##################
    timeFig <- ggplotly(timeseriesFigureList$figureInteractive, tooltip = "text")

    ########################
    # Returning the figure #
    ########################
    return(timeFig)

    })
  
#------------------------------------------------------------------------------#
# Downloading the time series figure -------------------------------------------
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

      # Static version of plot
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
# in the sidebar menu for users. This setting will not apply if the Prophet    #
# model is selected.                                                           #
#------------------------------------------------------------------------------#

  ##########################
  # Creating the UI object #
  ##########################
  output$smoothing <- renderUI({

    # Requiring the input data 
    req(file())
    
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
                          value = 1,
                          min = 1))
      
      ###############################################
      # Returns NULL if not working with daily data #
      ###############################################
      }else{
        
        # Return NULL 
        return(NULL)
        
        }
    
    
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
    
    req(file())
    
    ############################################################
    # Determining the number that is the sequence for the date #
    ############################################################
    dateSeq <- switch(as.character(dateValues$dates), # Calling the data type
                      "week" = 7, # Sequence forecast periods by seven if weekly data
                      "day" = 1, # Sequence forecast periods by seven if daily data
                      1) # Sequence forecast periods by one if daily, yearly, or time index
    
    #########################################################################
    # Determining the min, max, and creating a date vector - Year and Index #
    #########################################################################
    if(dateValues$dates %in% c("year", "index")){
      
      # Earliest possible date
      data_min_date <- as.numeric(min(na.omit(as.numeric(file()[,1]))))
      
      # Latest possible date
      data_max_date <- as.numeric(max(na.omit(as.numeric(file()[,1]))))
      
      ###############################################
      # Create the `sliderInput` for the UI Sidebar #
      ###############################################
      return(sliderInput("forecast.period",
                         label = tags$span("Forecasting Date(s) ", # Input label
                                           tags$i(class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF;",
                                                  title = "The forecasting date corresponds the last week of data included in the calibration period (i.e., the week the forecast is conducted).")
                         ),
                         min = data_min_date, # Min value
                         max = data_max_date, # Max value
                         value = c(data_min_date, data_max_date), # Values to show
                         step =  dateSeq, # Interval to show breaks by 
                         sep = "")) 
      
      #######################################################################
      # Determining the min, max, and creating a date vector - Day and Week #
      #######################################################################
      }else{
      
        # Earliest possible date
        data_min_date <- min(na.omit(anytime::anydate(file()[, 1])))
        
        # Latest possible date
        data_max_date <- max(na.omit(anytime::anydate(file()[, 1])))
        
        ###############################################
        # Create the `sliderInput` for the UI Sidebar #
        ###############################################
        return(sliderInput("forecast.period",
                           label = tags$span("Forecasting Date(s) ", # Input label
                                             tags$i(class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF;",
                                                    title = "The forecasting date corresponds the last week of data included in the calibration period (i.e., the week the forecast is conducted).")
                           ),
                           min = data_min_date, # Min value
                           max = data_max_date, # Max value
                           value = c(data_min_date, data_max_date), # Values to show
                           step =  dateSeq)) # Interval to show breaks by
        
        } # End of if-else 

    
    }) # End of 'renderUI' statement 

  

#------------------------------------------------------------------------------#
# UI Input for the Calibration Period ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines the possible lengths of the calibration       #
# period lengths for a user to use. It takes in an input of the user selected  #
# data fame, and outputs the information for the calibration period picker.    #
#------------------------------------------------------------------------------#
  
  output$calibration.period <- renderUI({
    
    # Requiring forecast period 
    req(input$forecast.period)
    
    #######################
    # Reading in the data #
    #######################
    data <- file()
    
    #############################################
    # Sub-setting the dates from the crude data #
    #############################################
    if(dateValues$dates %in% c("day", "week")){
      
      # Daily or weekly dates 
      crude.dates <- anytime::anydate(data[, 1])
      
    }else{
      
      # Yearly or time-index data 
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
                choices = c(seq(1,last.calibration.period, 1)), # Initial starting period 
                selected = last.calibration.period, # Selecting the max value
                multiple = T, # Allowing for multiple selections 
                options = list(`actions-box` = TRUE, # Creating the select-all
                               `live-search`= TRUE)) # Allowing typing 
    

  }) # End of 'renderUI' statement 
  
  
    
#------------------------------------------------------------------------------#
# Preparing the calibration periods --------------------------------------------
#------------------------------------------------------------------------------#
# About: Below creates the forecasting periods used throughout the remainder   #
# of the dashboard program. The forecasting, or calibration periods is a list  #
# of the data used to calibrated the different models at each forecasting      #
# period. Therefore, this section returns a list of data frames.               #
#------------------------------------------------------------------------------#

  #########################################################################
  # Initialize `reactiveValues` to store the calibration period data sets #
  #########################################################################
  calibration.period.list <- reactiveValues(calibrations = NULL)

  ###################################
  # Observing differences in inputs #
  ###################################
  observe({
    
    #######################################
    # Runs if the 'run' button is clicked #
    #######################################
    observeEvent(input$run, {
      
      # Requiring the input data 
      req(input$forecast.period)
      
      # Requiring the calibration period
      req(input$calibrationPeriod)

      ##################################
      # Loading the user selected data #
      ##################################
      data <- file()
      
      ######################################################
      # Extracting the needed information for the function #
      ######################################################
      
      # Extracting the date column
      dateHeader <- names(data)[1]
      
      # Determining the locations to keep
      locationsList <- input$locations
      
      # Sub-setting data to keep date column and locations of interest
      data <- data %>%
        dplyr::select(dateHeader, all_of(locationsList)) # Selecting needed information
      
      #############################################
      # Calibration period input selected by user #
      #############################################
      caliPeriod <- c(input$calibrationPeriod)
      
      ############################################################
      # Determining the number that is the sequence for the date #
      ############################################################
      dateSeq <- switch(as.character(dateValues$dates), # Calling the data type
                        "week" = 7, # Sequence forecast periods by seven if weekly data
                        "day" = 7, # Sequence forecast period by seven if daily data 
                        1) # Sequence forecast periods by one if daily, yearly, or time index
      
      #################################################
      # Forecast period range - Yearly and Index data #
      #################################################
      if(dateSeq == 1){
        
        # Working with yearly or time index data
        forecastPeriodRange <- c(seq(input$forecast.period[1],  input$forecast.period[2], by = 1))
        
      ################################################
      # Forecast period range - Daily or weekly data #
      ################################################
      }else{
        
        # List of forecast period dates
        forecastPeriodRange <- c(seq.Date(anytime::anydate(input$forecast.period[1]),  anytime::anydate(input$forecast.period[2]), by = 7)) # If working with daily or weekly data
        
      }
      
      #######################################################
      # Function that returns a list of calibration periods #
      #######################################################
      isolate({
        
        # Function of calibration/forecasting periods
        calibrationPeriod.return <- calibration.period.function(crude.data.input = data, # Observed data 
                                                                calibration.period.input = c(caliPeriod), # Range of calibration periods 
                                                                forecast.period.input = c(forecastPeriodRange), # Forecast period dates 
                                                                date.input = dateValues$dates) # Date type 
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
    
  }) # End of 'observe' statement
  
  
#------------------------------------------------------------------------------#
# Clearing the calibration periods ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the exisiting calibration period data frames if   #
# the user changes the underlying data set or clicks clear.                    #
#------------------------------------------------------------------------------#
  
  observeEvent(clearingOut(), {

    # Resetting the the list containing calibration period data
    calibration.period.list$calibrations <- NULL

  })


#------------------------------------------------------------------------------#
# Setting up ARIMA options  ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: The following section is centered on the available features with the  #
# included auto-regressive integrated moving average models. First, it creates #
# the ARIMA specific UI outputs, including, parameter specification,           #
# indication of seasonality, and other available features. It then runs the    #
# model and outputs quantile forecasts which are used throughout the rest of   #
# the shiny app, and also outputted to the main screen.                        #
#------------------------------------------------------------------------------#

  ##############################################################
  # Conditional Output: Creating the UI output for seasonality #
  ##############################################################
  output$ARIMA.seasonality <- renderUI({
    
    # Showing output if seasonality is indicated 
    if(input$considerSeasonality){
      
      ##############################################
      # Creating the text input - Seasonal Pattern #
      ##############################################
      textInput("seasonality", "Seasonal Pattern", value = 7)
    
    # Showing no output if seasonality is not indicated  
    }else{NULL}
    
  }) # End of 'renderUI' statement
  
  ####################################################
  # Conditional Output: Creating UI output for P Min #
  ####################################################
  output$PMin <- renderUI({
    
    # Showing output if seasonality is indicated 
    if(input$considerSeasonality){

    #####################################
    # Creating the numeric input - pMin #
    #####################################
    numericInput("PMin", label = "P Min", value = 0)
      
    # Showing no output if seasonality is not indicated  
    }else{NULL}

  }) # End of 'renderUI' statment
  
  ########################################################
  # Conditional Output: Creating the UI output for P Max #
  ########################################################
  output$PMax <- renderUI({
    
    # Showing output if seasonality is indicated 
    if(input$considerSeasonality){

    ######################################
    # Creating the numeric input - P Max #
    ######################################
    numericInput("PMax", label = "P Max", value = 3)
      
    # Showing no output if seasonality is not indicated  
    }else{NULL}
    
  }) # End of 'renderUI' statement
  
  ########################################################
  # Conditional Output: Creating the UI output for Q Min #
  ########################################################
  output$QMin <- renderUI({
    
    # Showing output if seasonality is indicated 
    if(input$considerSeasonality){

    ######################################
    # Creating the numeric input - Q Min #
    ######################################
    numericInput("QMin", label = "Q Min", value = 1)
      
    # Showing no output if seasonality is not indicated  
    }else{NULL}

  }) # End of 'renderUI' statement

  ########################################################
  # Conditional Output: Creating the UI output for Q Max #
  ########################################################
  output$QMax <- renderUI({
    
    # Showing output if seasonality is indicated 
    if(input$considerSeasonality){
      
    ######################################
    # Creating the numeric input - Q Max #
    ######################################
    numericInput("QMax", label = "Q Max", value = 3)
      
    # Showing no output if seasonality is not indicated  
    }else{NULL}

  }) # End of 'renderUI' statement
  
  ############################################################
  # Creating the UI parameter outputs - Seasonal Differences #
  ############################################################
  output$Seasonaldifferences <- renderUI({
    
    # Showing output if seasonality is indicated 
    if(input$considerSeasonality){
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("Seasonaldifferences", label = "Seasonal Differences", value = 2)
      
    # Showing no output if seasonality is not indicated  
    }else{NULL}
    
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
  # Creating the UI parameter outputs - p Max #
  #############################################
  output$pMax <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("pMax", label = "p Max", value = 10)
    
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
  # Creating the UI parameter outputs - q Max #
  #############################################
  output$qMax <- renderUI({
    
    ##############################
    # Creating the numeric input #
    ##############################
    numericInput("qMax", label = "q Max", value = 5)
    
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

#------------------------------------------------------------------------------#
# Producing the ARIMA Forecasts ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes in ARIMA inputs and runs the ARIMA model based on  #
# the user selected inputs. It then outputs a list with both the quantile      #
# based forecasts and the best-fit model information. The forecasts are saved  #
# in a reactive values and then shown on the main page.                        #
#------------------------------------------------------------------------------#
   
  ############################################################################
  # Initialize 'reactiveValues' to store the information about the ARIMA model #
  ############################################################################
  ARIMAInfo <- reactiveValues()

  ##########################################################################
  # Initialize 'reactiveValues' to store information about ARIMA model fit #
  ##########################################################################
  ARIMAInfoMetrics <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    ########################
    # Runs if 'run' is hit #
    ########################
    observeEvent(input$run, {
      
      # Requiring the input data
      req(file())
      
      # Requiring a model input
      req(input$modelType)
      
      # Requiring location selection
      req(input$locations)
      
      #############################################################
      # Isolating the behavior to only when the run button is hit #
      #############################################################
      isolate({
        
        # List of selected models 
        model <- c(input$modelType)
        
        ######################################
        # Runs if an ARIMA model is selected #
        ######################################
        if(any(model %in% c("ARIMA"))){
          
          # Creating the list of parameters based on seasonality choice - Seasonality 
          if(input$considerSeasonality){
            
            paramsList <- c(input$pMin, input$pMax, input$qMin, input$qMax, 
                            input$differences, input$PMin, input$PMax, input$QMin, input$QMax, 
                            input$Seasonaldifferences)
            
            freqValue <- input$seasonality
            
          # Creating the list of parameters based on seasonality choice - No Seasonality    
          }else{
            
            paramsList <- c(input$pMin, input$pMax, input$qMin, input$qMax, 
                            input$differences, 0, 3, 1, 3, 
                            2)
            
            freqValue <- 1
            
          }
          
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
                               parameter.input = c(paramsList), # ARIMA parameters 
                               seasonality.input = freqValue) # ARIMA seasonality 
            
            #################################################
            # Sub-setting the fits and model fit statistics #
            #################################################
            
            # Fit and forecasts 
            fitForecast <- ARIMAList[1]$Forecasts
            
            # Fit statistics 
            fitStatistics <- ARIMAList[2]$ModelFit
            
            ##############################################
            # Determining if an error should be returned #
            ##############################################
            
            # Pulling the names of files with issues 
            namesErrorARIMA <- c(names(fitForecast[is.na(fitForecast)]))
            
            #####################################################
            # Saving the list of forecasts that should be shown #
            #####################################################
            
            # Pulling only the forecasts that are not NA in the list
            ARIMAInfo$arima <- fitForecast[!is.na(fitForecast)]
            
            # Pulling the list of best-fit metrics
            ARIMAInfoMetrics$metrics <- fitStatistics[-1,] 
            
            #####################################################
            # Error if there are any forecasts that did not run #
            #####################################################
            if(length(namesErrorARIMA) > 0){
              
              # Error to return
              shinyalert("Unable to run the following ARIMA forecasts: ", 
                         paste(namesErrorARIMA, collapse = "\n"),
                         "Please check your data and the length of the specified calibration
                         period(s).",  paste(namesErrorARIMA, collapse = "\n"), type = "error")
              
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
# GLM forecasts and associated fit metrics (AIC, AICc, and BIC) which can be   #
# used throughout the remainder of the code.                                   #
#------------------------------------------------------------------------------#
   
   ####################################################################
   # Initialize 'reactiveValues' to store the a list of GLM forecasts #
   ####################################################################
   GLMList <- reactiveValues()
  
   ######################################################################
   # Initialize 'reactiveValues' to store the a list of GLM fit metrics #
   ######################################################################
   GLMInfoMetrics <- reactiveValues()
  
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     #############################################
     # Runs only if the "Run" button is selected #
     #############################################
     observeEvent(input$run,{

       # Requiring original data
       req(file())
       
       # Requiring location selection
       req(input$locations)
       
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
             
             #################################################
             # Sub-setting the fits and model fit statistics #
             #################################################
             
             # Fit and forecasts
             fitForecast <- GLMListQuantile[1]$Forecasts
             
             # Fit statistics
             fitStatistics <- GLMListQuantile[2]$ModelFit
             
             ##############################################
             # Determining if an error should be returned #
             ##############################################
             
             # Pulling the names of files with issues
             namesErrorGLM <- c(names(fitForecast[is.na(fitForecast)]))
             
             #####################################################
             # Saving the list of forecasts that should be shown #
             #####################################################
             
             # Pulling only the forecasts that are not NA in the list
             GLMList$GLM <- fitForecast[!is.na(fitForecast)]
             
             # Pulling the list of best-fit metrics
             GLMInfoMetrics$metrics <- fitStatistics[-1,]
             
             #####################################################
             # Error if there are any forecasts that did not run #
             #####################################################
             if(length(namesErrorGLM) > 0){
               
               # Error to return
               shinyalert("Unable to run the following GLM forecasts: ", 
                          paste(namesErrorGLM, collapse = "\n"),
                          "Please check your data and the length of the specified calibration
                         period(s).",  paste(namesErrorGLM, collapse = "\n"), type = "error")
               
             }
             
           }) # End of inner 'isolate' statement
           
         ############################################
         # End of code that runs for an 'GLM' model #
         ############################################
         }else{
           
           # Null
           NULL
           
         } # End of 'else' if GLM is not selected
         
       }) # Outer 'isolate' statement
       
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
     
     # Requiring the calibration period
     req(input$calibrationPeriod)
     
     # Creating the calibration period vector
     KVector <- ""

     # Filling the vector
     for(i in 1:length(input$calibrationPeriod)){

       # Indexing the calibration period
       lengthCal <- as.numeric(input$calibrationPeriod[i])
       
       # Calculating the K
       k <- floor(as.numeric(as.numeric(lengthCal)/2) + 5)
       
       # Adding it to the vector
       if (KVector == "") {
         
         KVector <- k  # Initialize with the first value
         
       } else {
         
         KVector <- paste(KVector, k, sep = ", ")  # Append the next values
         
       }

     } # End of the loop 
     
     # Creating the output 
     textInput("numberBasis", 
               "Number of Basis Functions:",
               value = KVector) 
     
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
# GAM forecasts and associated fit metrics which can be used throughout the    #
# remainder of the code.                                                       #
#------------------------------------------------------------------------------#
   
   ####################################################################
   # Initialize 'reactiveValues' to store the a list of GAM forecasts #
   ####################################################################
   GAMList <- reactiveValues()
  
   ######################################################################
   # Initialize 'reactiveValues' to store the a list of GAM fit metrics #
   ######################################################################
   GAMInfoMetrics <- reactiveValues()

   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     #############################################
     # Runs only if the "Run" button is selected #
     #############################################
     observeEvent(input$run,{
       
       # Requiring a model input
       req(input$modelType)
       
       # Requiring original data
       req(file())
       
       # Requiring location selection
       req(input$locations)
      
       # Requiring the calibration period
       req(input$calibrationPeriod)
       
       #######################
       # Isolating behaviors #
       #######################
       isolate({
         
         # Model list 
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
                                       k.input = (input$numberBasis), # Number of basis functions 
                                       smoothingTerm.input = input$smoothingTerm, # Smoothing term for GAM
                                       uniqueCalibrations = c(input$calibrationPeriod)) 
                
                ############################
                # Handling the basis error #
                ############################
                if(any(GAMListQuantile == "KError")){

                  # Error to run
                  shinyalert("Please check your basis function specification. The number of basis functions specified
                             does not match the number of calibration periods selected.", type = "error")

                  # Clearing out the data
                  GAMListQuantile <- NULL

                }else{
                  
                #################################################
                # Sub-setting the fits and model fit statistics #
                #################################################
                
                # Fit and forecasts
                fitForecast <- GAMListQuantile[1]$Forecasts
                
                # Fit statistics
                fitStatistics <- GAMListQuantile[2]$ModelFit
                
                ##############################################
                # Determining if an error should be returned #
                ##############################################
                
                # Pulling the names of files with issues
                namesErrorGAM <- c(names(fitForecast[is.na(fitForecast)]))
                
                #####################################################
                # Saving the list of forecasts that should be shown #
                #####################################################
                
                # Pulling only the forecasts that are not NA in the list
                GAMList$GAM <- fitForecast[!is.na(fitForecast)]
                
                # Pulling the list of best-fit metrics
                GAMInfoMetrics$metrics <- fitStatistics[-1,]
                
                #####################################################
                # Error if there are any forecasts that did not run #
                #####################################################
                if(length(namesErrorGAM) > 0){
                  
                  # Error to return
                  shinyalert("Unable to run the following GAM forecasts: ", 
                             paste(namesErrorGAM, collapse = "\n"),
                             "Please check your data and the length of the specified calibration
                         period(s).",  paste(namesErrorGAM, collapse = "\n"), type = "error")
                  
                }
                
                } # End of 'else' for error 
                
           }) # End of inner 'isolate' statement
           
         ############################################
         # End of code that runs for an 'GAM' model #
         ############################################
         }else{
           
             # Null
             NULL
           
         } # End of 'else' if GAM is not selected
         
       }) # Outer 'isolate' statement
       
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
   
  ########################################################################
  # Initialize 'reactiveValues' to store the a list of Prophet forecasts #
  ########################################################################
  ProphetList <- reactiveValues()
  
  ##########################################################################
  # Initialize 'reactiveValues' to store the a list of Prophet fit metrics #
  ##########################################################################
  ProphetInfoMetrics <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    #############################################
    # Runs only if the "Run" button is selected #
    #############################################
    observeEvent(input$run,{
      
      # Requiring a model input
      req(input$modelType)
      
      # Requiring original data
      req(file())
      
      # Requiring location selection
      req(input$locations)
      
      #######################
      # Isolating behaviors #
      #######################
      isolate({
        
        # Model list 
        model <- c(input$modelType)
        
        #######################################
        # Running only if Prophet is selected #
        #######################################
        if(any(model %in% c("Prophet"))){
          
          ####################################
          # Inner 'isolate' for GAM function #
          ####################################
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
               
               #################################################
               # Sub-setting the fits and model fit statistics #
               #################################################
               
               # Fit and forecasts
               fitForecast <- prophetList[1]$Forecasts
               
               ##############################################
               # Determining if an error should be returned #
               ##############################################
               
               # Pulling the names of files with issues
               namesErrorProphet <- c(names(fitForecast[is.na(fitForecast)]))
               
               #####################################################
               # Saving the list of forecasts that should be shown #
               #####################################################
               
               # Pulling only the forecasts that are not NA in the list
               ProphetList$prophet <- fitForecast[!is.na(fitForecast)]

               ##############################################
               # Determining if an error should be returned #
               ##############################################
               
               # Pulling the names of files with issues
               namesErrorProphet <- c(names(fitForecast[is.na(fitForecast)]))
               
               #####################################################
               # Saving the list of forecasts that should be shown #
               #####################################################
               
               # Pulling only the forecasts that are not NA in the list
               ProphetList$prophet <- fitForecast[!is.na(fitForecast)]
               
               #####################################################
               # Error if there are any forecasts that did not run #
               #####################################################
               if(length(namesErrorProphet) > 0){
                 
                 # Error to return
                 shinyalert("Unable to run the following Prophet forecasts: ", 
                            paste(namesErrorProphet, collapse = "\n"),
                            "Please check your data and the length of the specified calibration
                         period(s).",  paste(namesErrorProphet, collapse = "\n"), type = "error")
                 
               }
               
          }) # End of inner 'isolate' statement
          
        ############################################
        # End of code that runs for an 'GAM' model #
        ############################################
        }else{
          
          # Null
          NULL
          
        } # End of 'else' if GAM is not selected
        
      }) # Outer 'isolate' statement
      
    }) # End of 'observeEvent'
    
  }) # End of 'observe'
               

#------------------------------------------------------------------------------#
# Creating the filtering options for the quantile forecasts --------------------
#------------------------------------------------------------------------------#
# About: This section creates the quantile forecast data filter for the main   #
# page of the dashboard. Filtering options are available for the calibration   #
# period length, the model, and the location.                                  #
#------------------------------------------------------------------------------#
   
   ######################################
   # Creating the needed reactive value #
   ######################################
   
   # To store the filtered quantile forecasts 
   finalQuantileCombined <- reactiveValues()
   
   # Indicator 
   QuantileForecastMAINIndicator <- reactiveVal(0)
   
   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({
     
     # Creating the combination of quantile forecasts 
     quantile.forecast <- c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet)
     
     # Running only if an error is not returned above
     if(all(!is.null(quantile.forecast))){
       
       #####################################
       # Creating the pop-up for filtering #
       #####################################
       observeEvent(input$filterQuantileForecasts, ignoreInit = T,{
         
         # Changing the indicator to one (i.e., button has been clicked)
         isolate({QuantileForecastMAINIndicator(1)})
         
         # Requiring the calibration period
         req(input$calibrationPeriod)
         
         # Isolating button click behavior 
         isolate({
           
           #######################
           # Creating the button #
           #######################
           showModal(modalDialog(
             
             title = "Filtering Options",
             pickerInput("modelQuantile", "Model:", c(input$modelType), selected = c(input$modelType), multiple = T), # Model filtering
             pickerInput("locationsQuantile", "Location:", c(input$locations), selected = c(input$locations), multiple = T), # Location Type
             pickerInput("calibrationQuantile", "Calibration Period Length(s):", c(input$calibrationPeriod), selected = c(input$calibrationPeriod), multiple = T), # Calibration period lengths
             
           )) # End of the modal creation 
           
         }) # End of inner isolation 
         
       }) # End of 'observeEvent'
       
       ##################################################
       # Function to filter the formatted forecast data #
       ##################################################
       filteredForecasts <- filteringQuantileForecasts(QuantileForecast.input = quantile.forecast, # Quantile forecast
                                                       modelFilterQ.input = input$modelQuantile, # Model filter
                                                       locationFilterQ.input = input$locationsQuantile, # Location filter
                                                       indicator.input = QuantileForecastMAINIndicator(), # Indicator
                                                       calibrationQ.input = input$calibrationQuantile) # Calibration filter
       
       # Saving the results to the reactive value
       finalQuantileCombined$data <- rev(filteredForecasts)
    
     ########################################
     # Returning nothing if an error occurs #
     ########################################
     }else{
       
       finalQuantileCombined$data <- NULL
       
     } # End of 'else'
     
   }) # End of 'observeEvent'


#------------------------------------------------------------------------------#
# Resetting the quantile forecast data -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the quantile forecast data when the data is       #
# changed.                                                                     #
#------------------------------------------------------------------------------#
   
   ###############################
   # Looking for the data change #
   ###############################
   observeEvent(clearingOut(), {
     
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
     
     # Resetting the ARIMA metrics
     ARIMAInfoMetrics$metrics <- NULL
     
     # Resetting the GLM metrics
     GLMInfoMetrics$metrics <- NULL
     
     # Resetting the GAM metrics
     GAMInfoMetrics$metrics <- NULL
     
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
   observeEvent(input$modelQuantile, {
     
     # Isolate the behavior to when the button is clicked
     isolate({
       
       current_index(1)
       
     }) # End of isolate
     
   }) # End of 'observeEvent'
   
   #########################################################
   # Fixes the index when the data is filtered - Locations #
   #########################################################
   observeEvent(input$locationQuantile, {
     
     # Isolate the behavior to when the button is clicked
     isolate({
       
       current_index(1)
       
     }) # End of isolate
     
   }) # End of 'observeEvent'
   
   
   ############################################################
   # Fixes the index when the data is filtered - Calibrations #
   ############################################################
   observeEvent(input$calibrationQuantile, {
     
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
# quantile forecasts. If only one forecast is conducted, then a '.csv' file is #
# available for download. If multiple forecasts are conducted, a '.zip' file   #
# with the resulting '.csv's are available for download.                       #
#------------------------------------------------------------------------------#
   
   ##########################################################
   # Observing changes in the reactive value with forecasts #
   ##########################################################
   observe({
     
     ###############################################
     # Downloading a '.csv' if one forecast is run #
     ###############################################
     if(length(finalQuantileCombined$data) == 1){
       
       ######################################################
       # Backbone of download button for quantile forecasts #
       ######################################################
       output$download_quantile_forecasts <- downloadHandler(
         
         ####################################
         # Function to create the file-name #
         ####################################
         filename = function() {
           
           # File name 
           paste("quantile-calibration-", input$calibrationPeriod, "-location-", input$locations, '-model-', input$modelType, "-", input$dataset, sep = "")
           
         },
         
         #############################
         # Function to save the file #
         #############################
         content = function(file) {
           
           # Saving the file
           write.csv(finalQuantileCombined$data[[1]], file, row.names = FALSE)
           
         }
         
       ) # End of download button  
     
     ######################################################
     # Downloading a '.zip' if multiple forecasts are run #
     ######################################################
     }else{
       
       ######################################################
       # Backbone of download button for quantile forecasts #
       ######################################################
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
           
           # Saving the '.csv' files
           for (fileName in names(finalQuantileCombined$data)) {
             
             # Forecast file 
             file_obj <- data.frame(finalQuantileCombined$data[[fileName]])
             
             # If forecast file is found 
             if (!is.null(file_obj)) {
               
               # File name 
               file_name <- glue("{fileName}.csv")
               
               # Saving the '.csv'
               write_csv(file_obj, file.path(temp_directory, file_name))
               
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
       
     } # End of 'else' for files
     
   }) # ENd of 'observe' 
   
#------------------------------------------------------------------------------#
# Reading in the updated time series '.csv' ------------------------------------
#------------------------------------------------------------------------------#
# About; This section reads in the updated time series for plotting reasons.   #
# If loaded in, the forecast period section of the data will update with the   #
# most recent time series information.                                         #
#------------------------------------------------------------------------------#
   
   ######################################
   # Reactive value to save data within #
   ######################################
   updatedDataValue <- reactiveValues()

   ###########################
   # Reading in the new data #
   ###########################
   updatedFile <- reactive({

     # Runs if no error occurs
     tryCatch({

       ############################################
       # Name of file from the 'fileInput' picker #
       ############################################
       file1 <- input$obsNewData

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

       ###########################
       # Runs if no error occurs #
       ###########################
       }else{

         NULL

       } # End of if-else checking for error

       #######################
       # Reading in the data #
       #######################
       return(read.csv(file1$datapath, header = T, check.names=FALSE))

     # Runs if an error occurs
     }, error = function(e){

       NULL

     }) # End of 'tryCatch'

   }) # End of "observe"


#------------------------------------------------------------------------------#
# Producing list of formatted forecasts ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the quantile forecasts from the previous section   #
# and forms the formatted forecast files from the quantiles produced earlier.  #
# The formatted forecasts files consist of the date, the data, the median,     #
# and the lower and upper prediction interval. The user can determine the      #
# prediction interval to show on the forecast file. The forecast files are     #
# then filtered in a later step and shown on the main dashboard page.          #
#------------------------------------------------------------------------------#

   ################################################################
   # Initialize `reactiveValues` to store the formatted forecasts #
   ################################################################
   foremattedForecasts <- reactiveValues()

   #####################################################
   # Observing changes in inputs/other reactive values #
   #####################################################
   observe({

     # Requiring original data
     req(file())

     # Saving the data under the reactive value
     updatedDataValue$data <- updatedFile()

     ########################################################
     # Checking the uploaded data against the original data #
     ########################################################

     # Running if an updated file is loaded
     if(!is.null(updatedFile())){

       # Checking the length
       if(nrow(updatedFile()) <= nrow(file())){

         # Returning an error
         shinyalert("Please check the updated time series. It should include
                    dates past that included in the orignal time series.",
                    type = "error")

         # Clearing the updated data
         updatedDataValue$data <- NULL

       }
       
       # Updated data
       dataUpdated <- updatedFile()
       
       # Sub-setting dates 
       datesUpdated <- dataUpdated[,1]
       
       # Determining the date type
       updatedDateType <- date.type.function(dates.input = datesUpdated)
       
       # Checking the date
       if(updatedDateType != dateValues$dates){
         
         # Returning an error
         shinyalert("Please check the updated time series. The time make-up of the 
                    updated data does not match that included in the orignal time series.",
                    type = "error")
         
         # Clearing the updated data
         updatedDataValue$data <- NULL
         
       }
       
       # Checking the location names
       if(any(c(colnames(updatedFile())) %!in% c(colnames(file())))){

         # Returning an error
         shinyalert("Please check the updated time series. The included groups or
                    locations do not match that included in the orignal time series.",
                    type = "error")

         # Clearing the updated data
         updatedDataValue$data <- NULL

       }

     } # End of "if-else" checking for NULL results

     ###############################################################
     # Creating a list of events to trigger the formatted forecast #
     ###############################################################
     eventsTriggerFFData <- reactive({

       # List of events
       events <- list(input$run, updatedDataValue$data)

       # Returning the list of events
       return(events)

     })

     ##################################################################
     # Reverting back to the original data when the button is clicked #
     ##################################################################
     observeEvent(input$clearObsNewData, {

       # Clearing the updated data
       updatedDataValue$data <- NULL

     })


     ###############################################
     # Observe event for hitting the action button #
     ###############################################
     observeEvent(eventsTriggerFFData(),{

       # Creating the list of quantiles
       quantileList <- c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet)
       
       ################################################################
       # Function to create list of formatted forecasts - if Non Null #
       ################################################################
       if(all(!is.null(quantileList))){

         # 'Isolate' statement
         isolate({
  
           # Running the forecasts
           formattedForecastList <- formatted.forecast.function(quantile.input = c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet), # List of quantiles
                                                                data.input = file(), # Original data
                                                                dateType.input = dateValues$dates, # Type of date data
                                                                model.input = input$modelType, # Selected model
                                                                quantile.selected.input = input$quantileSelection, # Selected quantile
                                                                horizon.input = input$forecastHorizon, # Select a forecasting horizon
                                                                smoothing.input = input$smoothingInput, # Data smoothing
                                                                data.update = updatedDataValue$data, # Updated observed data
                                                                quantileSelected = input$quantileSelection) # Quantile
  
           # Saving formatted forecast to reactive value
           foremattedForecasts$forecasts <- formattedForecastList
           
         }) # End of inner 'isolate' statement
         
         } # End of 'if'

     }) # End of 'observeEvent' statement

   }) # End of 'observe' statement


#------------------------------------------------------------------------------#
# Clearing the original data ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the above created formatted forecasts if the file #
# containing the original data is changed.                                     #
#------------------------------------------------------------------------------#

  observeEvent(clearingOut(), {

    # Clearing the formatted forecast data
    foremattedForecasts$forecasts <- NULL

    # Clearing the updated data
    updatedDataValue$data <- NULL

  })

#------------------------------------------------------------------------------#
# Creating the filtering options for the formatted forecast --------------------
#------------------------------------------------------------------------------#
# About: This section creates the formatted forecast data filter for the main  #
# page of the dashboard. It filters the data by model, location, and           #
# calibration period length.                                                   #
#------------------------------------------------------------------------------#

  #######################################
  # Creating the needed reactive values #
  #######################################

  # To store the filtered formatted forecast
  finalForecastCombined <- reactiveValues()

  # Indicator
  formattedForecastMAINIndicator <- reactiveVal(0)

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    # Running only if an error is not returned above
    if(all(!is.null(foremattedForecasts$forecasts))){

      #######################
      # Creating the pop-up #
      #######################
      observeEvent(input$filterFormattedForecasts, ignoreInit = T,{

        # Requiring the model type
        req(input$modelType)

        # Requiring the location
        req(input$locations)
        
        # Requiring the calibration period
        req(input$calibrationPeriod)

        # Changing the indicator to one (i.e., button has been clicked)
        isolate({formattedForecastMAINIndicator(1)})

        # Isolating button click behavior
        isolate({

          #######################
          # Creating the button #
          #######################
          showModal(modalDialog(

            # Title of the box
            title = "Data Options",

            # Filter for model
            pickerInput("modelsForecastFigs", "Model:", c(input$modelType), selected = c(input$modelType), multiple = T), # Model filtering

            # Filter for location
            pickerInput("locationsForecastFigs", "Location:", c(input$locations), selected = c(input$locations), multiple = T), # Location Type

            # Filter for calibration period
            pickerInput("calibrationForecastFigs", "Calibration Period Length:", c(input$calibrationPeriod), selected = c(input$calibrationPeriod), multiple = T), # Calibration period length

            # Check to show the observed data during the forecast period
            checkboxInput("updateUnderlyingData", "Update the underlying data (Forecast Period)", value = F), # Determining if underlying data should be updated

            # Conditional panel: Show the associated settings
            conditionalPanel(

              # Condition
              condition = "input.updateUnderlyingData == true",

              # Load the updated time series.
              fileInput("obsNewData", # ID of UI input
                        label = tags$span("Upload the updated time-series data file", # Shown label
                                          # Creating the info circle
                                          tags$i(class = "glyphicon glyphicon-info-sign",
                                                 style = "color:#FFFFFF;",
                                                 title = "Upload a '.csv' file of your data.")
                        ),

                        multiple = FALSE), # End of file input

              # Revert back to the original data
              actionButton("clearObsNewData", "Revert back to original data")

            ) # End of conditional panel

          )) # End of 'showModal'

        }) # End of inner isolate

      }) # End of 'observeEvent' for button

      ##################################################
      # Function to filter the formatted forecast data #
      ##################################################
      filteredForecasts <- filteringFormattedForecasts(formattedForecast.input = foremattedForecasts$forecasts, # Formatted forecast
                                                        modelFilterFF.input = input$modelsForecastFigs, # Model filter
                                                        locationFilterFF.input = input$locationsForecastFigs, # Location filter
                                                        calibrationFilterFF.input = input$calibrationForecastFigs, # Calibration period length filter
                                                        indicator.input = formattedForecastMAINIndicator()) # Indicator to filter

      # Saving the results to the reactive value
      finalForecastCombined$data <- rev(filteredForecasts)

    } # End of 'else'

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
  observeEvent(clearingOut(), {

    # Clearing the output
    finalForecastCombined$data <- NULL

    # Resetting the indicator
    formattedForecastMAINIndicator(0)

  })

#------------------------------------------------------------------------------#
# Creating the arrows for the formatted forecasts and figures ------------------
#------------------------------------------------------------------------------#
# About: This section creates the foward and backwards arrows that allow for   #
# navigation between the figures and associated data sets.                     #
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

       #################################
       # If working with panel figures #
       #################################
       if(input$panelModelsForecasts){

         # Isolating the action to only when the button is clicked
         isolate({

           # Run if the current index is less than the length of the list
           if (current_index_formatted() < length(finalFiguresDashboard$data)) {

             # Changing the index of the reactive value
             current_index_formatted(min(current_index_formatted() + 1))

           }

         }) # End of 'isolate' statement

       ######################################
       # If working with individual figures #
       ######################################
       }else{

         # Isolating the action to only when the button is clicked
         isolate({

           # Run if the current index is less than the length of the list
           if (current_index_formatted() < length(finalForecastCombined$data)) {

             # Changing the index of the reactive value
             current_index_formatted(min(current_index_formatted() + 1))

           }

         }) # End of 'isolate' statement

       } # End of 'if-else'

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

    ###################################################################
    # Fixes the index when the data is filtered - Calibration periods #
    ###################################################################
    observeEvent(input$calibrationForecastFigs, {

      # Isolate the behavior to when the button is clicked
      isolate({

        current_index_formatted(1)

      }) # End of isolate

    }) # End of 'observeEvent'

#------------------------------------------------------------------------------#
# Rendering the title and formatted forecast data ------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the title and formatted forecast data based on   #
# the user selected filters. If no filters are selected, but the button is     #
# has been hit, no data will be shown.                                         #
#------------------------------------------------------------------------------#

   ######################################################
   # Rendering the title for the formatted forecast box #
   ######################################################
   output$Formatted.ForecastTitle <- renderText({

     # Producing nothing if no locations or models are chosen
     if(length(input$locationsForecastFigs) == 0 || length(input$modelsForecastFigs) == 0 || is.null(finalForecastCombined$data) || input$calibrationForecastFigs == 0){

       # Returning NULL
       return(NULL)

     # Runs if at least one location or model is selected
     }else{

       # Rendering the title of the formatted forecast box
       return(names(finalForecastCombined$data[current_index_formatted()]))

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
# formatted forecasts to create a 'zip' or 'csv' file. If there was only one   #
# forecast produced or selected, a single '.csv' file is outputted. If         #
# multiple files are outputted, they are included within a '.zip' folder.      #
#------------------------------------------------------------------------------#

  ###########################################
  # Observing changes in the reactive value #
  ###########################################
  observe({

    ###########################
    # Producing a '.csv' file #
    ###########################
    if(length(finalForecastCombined$data) == 1){

      ###############################################################
      # Backbone of download button for the formatted forecast data #
      ###############################################################
      output$download_FormatttedForecasts <- downloadHandler(

        ####################################
        # Function to create the file-name #
        ####################################
        filename = function() {

          # File name
          paste("formatted-forecast-calibration-", input$calibrationPeriod, "-location-", input$locations, '-model-', input$modelType, "-", input$dataset, sep = "")

        },

        #############################
        # Function to save the file #
        #############################
        content = function(file) {

          # Saving the file
          write.csv(finalForecastCombined$data[[1]], file, row.names = FALSE)

        })

    #############################
    # Producing a '.zip' folder #
    #############################
    }else{

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

          # Saving the files
          for(fileName in names(finalForecastCombined$data)) {

            # Individual file
            file_obj <- finalForecastCombined$data[[fileName]]

            # If plot is found
            if (!is.null(file_obj)) {

              # File name
              file_name <- glue("{fileName}.csv")

              # Saving the csv
              write_csv(file_obj, file.path(temp_directory, file_name))

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

    } # End of 'if-else' for number of files to download

  })

#------------------------------------------------------------------------------#
# Creating the "Edit Figures" pop-up -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up containing all of the figure editing  #
# options. Each of the customization selected by the user applies both to the  #
# individual figures, and to the panel figures.                                #
#------------------------------------------------------------------------------#

  ##########################
  # Line type - Median fit #
  ##########################
  lineTypeFFPanel <- reactiveValues(type = "Solid")

  ###########################
  # Line color - Median fit #
  ###########################
  lineColorFFPanel <- reactiveValues(color = "Red")

  ###########################
  # Line width - Median fit #
  ###########################
  lineWidthFFPanel <- reactiveValues(width = 0.9)

  ######################
  # Line type - Bounds #
  ######################
  lineTypeBoundsFFPanel <- reactiveValues(type = "Dashed")

  #######################
  # Line color - Bounds #
  #######################
  lineColorBoundsFFPanel <- reactiveValues(color = "Black")

  #######################
  # Line Width - Bounds #
  #######################
  lineWidthBoundsFFPanel <- reactiveValues(width = 0.65)

  #########################
  # Ribbon color - Bounds #
  #########################
  ribbonColorFFPanel <- reactiveValues(color = "Light Grey")

  ###############################
  # Reactive value for dot size #
  ###############################
  dotSizeReactiveFFPanel <- reactiveValues(sizeVal = 2)

  #############################
  # Dot color - Observed data #
  #############################
  dotColorReactiveFFPanel <- reactiveValues(color = "Black")

  #######################################
  # Reactive value for the y-axis label #
  #######################################
  yAxisLabFFPanel <- reactiveValues(lab = "Count")

  ########################################
  # Reactive value for Y-Axis Label Size #
  ########################################
  yAxisLabSizeFFPanel <- reactiveValues(size = 10)

  ########################################
  # Reactive value for Y-Axis label Face #
  ########################################
  yAxisLabFaceFFPanel <- reactiveValues(face = "Plain")

  #######################################
  # Reactive value for y-axis tick size #
  #######################################
  yAxisTickLabelSizeFFPanel <- reactiveValues(size = 10)

  #############################################
  # Reactive value for starting at the y-axis #
  #############################################
  startYReactiveFFPanel <- reactiveValues(check = "0")

  #####################################################
  # Reactive value for the number of breaks in y-axis #
  #####################################################
  yAxisBreaksFFPanel <- reactiveValues(value = 11)

  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYFFPanel <- reactiveValues(logScale = "Original")

  #######################################
  # Reactive value for the x-axis label #
  #######################################
  xAxisLabelFFPanel <- reactiveValues(title = "")

  ############################################
  # Reactive value for the x-axis label size #
  ############################################
  xAxisLabelSizeFFPanel <- reactiveValues(size = 10)

  ########################################
  # Reactive value for X-Axis label Face #
  ########################################
  xAxisLabFaceFFPanel <- reactiveValues(face = "Plain")

  #######################################
  # Reactive value for X-Axis tick size #
  #######################################
  xAxisTickSizeFF <- reactiveValues(size = 10)

  ################################################
  # Reactive value for the number of date breaks #
  ################################################
  dateBreaksReactiveFFPanel <- reactiveValues(breaksDate = "2")

  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editForecastFigures, {

    showModal(modalDialog(

      # Title
      title = "Figure Options",

      # Creating the tabbed menu
      tabsetPanel(

        ################
        # Y-Axis Table #
        ################
        tabPanel("Y-Axis",
                 tags$h4("Primary Label Options"), # Labels title
                 textInput("yaxisLabelFPanel", "Label for Y-Axis:", value = yAxisLabFFPanel$lab), # Label for y-axis
                 numericInput("yaxisLabelSizeFPanel", "Label Text Size:", value = yAxisLabSizeFFPanel$size), # Size for y-axis label
                 pickerInput("yaxisLabelFaceFPanel", label = "Font Face", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = yAxisLabFaceFFPanel$face), # Face of main y-axis label
                 tags$h4("Axis Options"),
                 numericInput("yAxisTickSizeFPanel", "Tick Label Size:", value = yAxisTickLabelSizeFFPanel$size), # Tick label size
                 pickerInput("zeroStartFFPanel", "Y-Axis Origin:", c("0", "Minimum value in data"), selected = startYReactiveFFPanel$check), # Starting of the y-axis
                 numericInput("yAxisBreaksFPanel", "Y-Axis Breaks:", value = yAxisBreaksFFPanel$value), # Y-Axis breaks
                 pickerInput("logScaleFFPanel", "Scale for Y-Axis:", c("Original", "Log(Base 10)"), selected = scaleYFFPanel$logScale) # Scale of y-axis

        ),


        ################
        # X-Axis Table #
        ################
        tabPanel("X-Axis",
                 tags$h4("Primary Label Options"), # Label title
                 textInput("xAxisFFLabelPanel", label = "Label for X-Axis", value = xAxisLabelFFPanel$title),
                 numericInput("xAxisLabelFFSize", label = "Label Text Size", value = xAxisLabelSizeFFPanel$size),
                 pickerInput("xaxisLabelFaceFPanel", label = "Font Face", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = xAxisLabFaceFFPanel$face), # Face of main x-axis label
                 tags$h4("Axis Options"),
                 textInput("dateBreaksTSFFPanel", "Number of Date Breaks (Label):", value = dateBreaksReactiveFFPanel$breaksDate), # Number of date breaks
                 numericInput("xAxisTickFPanel", "Tick Label Size:", value = xAxisTickSizeFF$size) #Tick size

        ),

        ############
        # Line tab #
        ############
        tabPanel("Lines",
                 h4("Median Line"), # Median Title
                 pickerInput("lineTypeForecastPanel", label = "Line Type:", choices = c("Solid", "Dashed", "Dotted", "Dotdash", "Longdash", "Twodash"), selected = c(lineTypeFFPanel$type)), # Line type
                 textInput("lineColorForecastPanel", label = "Line Color:", value = lineColorFFPanel$color), # Line color
                 numericInput("lineWidthForecastPanel", label = "Line Width:", value = lineWidthFFPanel$width), # Line width
                 h4("PI Bounds"), # Bounds title
                 pickerInput("lineTypeBoundPanel", label = "Line Type:", choices = c("Solid", "Dashed", "Dotted", "Dotdash", "Longdash", "Twodash"), selected = c(lineTypeBoundsFFPanel$type)), # Line type
                 textInput("lineColorBoundPanel", label = "Line Color:", value = lineColorBoundsFFPanel$color), # Line color
                 numericInput("lineWidthBoundsPanel", label = "Line Width:", value = lineWidthBoundsFFPanel$width), # Line width
                 textInput("RibbonColorBoundPanel", label = "Ribbon Color:", value = ribbonColorFFPanel$color) # Ribbon color

        ),

        #############
        # Point tab #
        #############
        tabPanel("Points",
                 h4("Observed Data"),
                 numericInput("dotSizeFFPanel", "Dot Size:", value = dotSizeReactiveFFPanel$sizeVal, step = 0.01), # Data dot size option
                 textInput("dotColorFFPanel", "Dot Color", value = dotColorReactiveFFPanel$color) # Dot color
        )

      ))) # End of creating the button

  })

  ############################################
  # Update the reactive value - Y-Axis Label #
  ############################################
  observeEvent(input$yaxisLabelFPanel, {

    # Updating the y-axis label
    yAxisLabFFPanel$lab <- input$yaxisLabelFPanel

  })

  ###################################################
  # Updating the reactive value - Y-Axis Label Size #
  ###################################################
  observeEvent(input$yaxisLabelSizeFPanel, {

    # Updating the y-axis label size
    yAxisLabSizeFFPanel$size <- input$yaxisLabelSizeFPanel

  })

  ###################################################
  # Updating the reactive value - Y-Axis Label Face #
  ###################################################
  observeEvent(input$yaxisLabelFaceFPanel, {

    # Updating the y-axis label face
    yAxisLabFaceFFPanel$face <- input$yaxisLabelFaceFPanel

  })

  ##################################################
  # Updating the reactive value - Y-Axis Tick Size #
  ##################################################
  observeEvent(input$yAxisTickSizeFPanel, {

    # Updating the y-axis tick size
    yAxisTickLabelSizeFFPanel$size <- input$yAxisTickSizeFPanel

  })

  ###############################################
  # Updating the reactive value - Y-Axis Breaks #
  ###############################################
  observeEvent(input$yAxisBreaksFPanel, {

    # Updating the number of y-axis breaks
    yAxisBreaksFFPanel$value <- input$yAxisBreaksFPanel

  })

  ##############################################
  # Updating the reactive value - X-Axis Label #
  ##############################################
  observeEvent(input$xAxisFFLabelPanel, {

    # Updating the x-axis label
    xAxisLabelFFPanel$title <- input$xAxisFFLabelPanel

  })

  ###################################################
  # Updating the reactive value - X-Axis Label Size #
  ###################################################
  observeEvent(input$xAxisLabelFFSize, {

    # Updating the x-axis label size
    xAxisLabelSizeFFPanel$size <- input$xAxisLabelFFSize

  })

  ###################################################
  # Updating the reactive value - X-Axis Label Face #
  ###################################################
  observeEvent(input$xaxisLabelFaceFPanel, {

    # Updating the x-axis label face
    xAxisLabFaceFFPanel$face <- input$xaxisLabelFaceFPanel

  })

  ##################################################
  # Updating the reactive value - X-Axis tick size #
  ##################################################
  observeEvent(input$xAxisTickFPanel, {

    # Updating the x-axis tick label size
    xAxisTickSizeFF$size <- input$xAxisTickFPanel

  })

  ##################################################
  # Updating the reactive value - Median Line Type #
  ##################################################
  observeEvent(input$lineTypeForecastPanel, {

    # Updating the median line type
    lineTypeFFPanel$type <- input$lineTypeForecastPanel

  })

  ###################################################
  # Updating the reactive value - Median Line Color #
  ###################################################
  observeEvent(input$lineColorForecastPanel, {

    # Updating the median line color
    lineColorFFPanel$color <- input$lineColorForecastPanel

  })

  ###################################################
  # Updating the reactive value - Median Line Width #
  ###################################################
  observeEvent(input$lineWidthForecastPanel, {

    # Updating the median line width
    lineWidthFFPanel$width <- input$lineWidthForecastPanel

  })

  #################################################
  # Updating the reactive value - Bound Line Type #
  #################################################
  observeEvent(input$lineTypeBoundPanel,{

    # Updating the bound line type
    lineTypeBoundsFFPanel$type <- input$lineTypeBoundPanel

  })

  ##################################################
  # Updating the reactive value - Bound Line Color #
  ##################################################
  observeEvent(input$lineColorBoundPanel, {

    # Updating the bound line color
    lineColorBoundsFFPanel$color <- input$lineColorBoundPanel

  })

  ##################################################
  # Updating the reactive value - Bound Line Width #
  ##################################################
  observeEvent(input$lineWidthBoundsPanel,{

    # Updating the bound line width
    lineWidthBoundsFFPanel$width <- input$lineWidthBoundsPanel

  })

  ##############################################
  # Updating the reactive value - Ribbon color #
  ##############################################
  observeEvent(input$RibbonColorBoundPanel,{

    # Updating the ribbon color
    ribbonColorFFPanel$color <- input$RibbonColorBoundPanel

  })

  #############################################
  # Update the reactive value - data dot size #
  #############################################
  observeEvent(input$dotSizeFFPanel,{

    # Updating the data point size
    dotSizeReactiveFFPanel$sizeVal <- input$dotSizeFFPanel

  })

  ##############################################
  # Update the reactive value - Data dot color #
  ##############################################
  observeEvent(input$dotColorFFPanel, {

    # Update the data point color
    dotColorReactiveFFPanel$color <- input$dotColorFFPanel

  })


  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleFFPanel, {

    # Updating the scale
    scaleYFFPanel$logScale <- input$logScaleFFPanel

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
                   startYReactiveFFPanel$check, dotSizeReactiveFFPanel$sizeVal,
                   lineTypeFFPanel$type, lineColorFFPanel$color, lineWidthFFPanel$width,
                   dotColorReactiveFFPanel$color, lineTypeBoundsFFPanel$type,
                   lineWidthBoundsFFPanel$width, lineColorBoundsFFPanel$color,
                   ribbonColorFFPanel$color, yAxisLabSizeFFPanel$size,
                   yAxisLabFaceFFPanel$face, yAxisTickLabelSizeFFPanel$size,
                   yAxisBreaksFFPanel$value, xAxisLabelFFPanel$title,
                   xAxisLabelSizeFFPanel$size, xAxisLabFaceFFPanel$face,
                   xAxisTickSizeFF$size, input$run)

    # Returning the list
    return(events)

  })

  #####################################################
  # Observing changes in inputs/other reactive values #
  #####################################################
  observeEvent(eventsTriggerIFig(), ignoreInit = T, {

    # Requiring the model input
    req(input$modelType)

    # Requiring the location
    req(input$locations)

    # Requiring forematted forecasts to run
    if(!is.null(foremattedForecasts$forecasts)){

      ###############################################
      # Function to create list of forecast figures #
      ###############################################
      isolate({

        individual <- forecast.figures(formatted.forecast.input = foremattedForecasts$forecasts, # Formatted figures list
                                       data.type.input = dateValues$dates, # Date input
                                       smoothing.input = input$smoothingInput, # Smoothing input
                                       scaleYAxis.input = scaleYFFPanel$logScale, # Scale y-axis
                                       yAxisLabel.input = yAxisLabFFPanel$lab, # Y-axis label
                                       dateBreaks.input = dateBreaksReactiveFFPanel$breaksDate, # Date breaks
                                       startYPoint.input = startYReactiveFFPanel$check, # Y-axis start point
                                       dotSize.input = dotSizeReactiveFFPanel$sizeVal, # Dot size
                                       linetype.input = lineTypeFFPanel$type, # Median line type
                                       lineColor.input = lineColorFFPanel$color, # Median line color
                                       lineWidth.input = lineWidthFFPanel$width, # Median line width
                                       dotColor.input = dotColorReactiveFFPanel$color, # Dot color
                                       boundtype.input = lineTypeBoundsFFPanel$type, # Bounds line type
                                       boundWidth.input = lineWidthBoundsFFPanel$width, # Bound width
                                       boundColor.input = lineColorBoundsFFPanel$color, # Bound color
                                       ribbonColor.input = ribbonColorFFPanel$color, # Ribbon color
                                       yLabelSize.input = yAxisLabSizeFFPanel$size, # Y-axis label size
                                       yLabelFace.input = yAxisLabFaceFFPanel$face, # Y-Axis label face
                                       yTickSize.input = yAxisTickLabelSizeFFPanel$size, # Y-Axis tick size
                                       yTickBreaks.input = yAxisBreaksFFPanel$value, # Y-Axis breaks
                                       xAxisLabel.input = xAxisLabelFFPanel$title, # X-Axis Label
                                       xAxisLabelSize.input = xAxisLabelSizeFFPanel$size, # X-Axis label size
                                       xAxisLabelFace.input = xAxisLabFaceFFPanel$face, # X-axis label face
                                       xAxisTickSize.input = xAxisTickSizeFF$size) # X-Axis tick size

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
        shinyalert("Unable to produce the following figures due to the presence of infinity UBs: ",
                   paste(namesErrorIndFig, collapse = "\n"), type = "error")

      }

      #########################################################
      # Returning the list of lists if individual figures run #
      #########################################################

      # Determining which figures are not NA
      notNAIndFig <- individual[!is.na(individual)]

      # Saving the exported list to a reactive value
      figuresForecast$figure <- notNAIndFig

    } # End of 'if' statement

  }) # End of 'observe' statement

#------------------------------------------------------------------------------#
# Filtering the forecast figures -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the list of formatted forecast figures by the    #
# type of model, location, and calibration period length. This section also    #
# creates the title which is shown on the main dashboard, and the filtering    #
# matches that used for the formatted forecast data.                           #
#------------------------------------------------------------------------------#

  ####################################################################
  # Creating the needed reactive value to store the forecast figures #
  ####################################################################

  finalForecastFigureCombined <- reactiveValues()

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    # Requiring the model input
    req(input$modelType)

    # Requiring the location
    req(input$locations)

    # Running only if an error is not returned above
    if(all(!is.null(foremattedForecasts$forecasts))){

      #####################################################
      # Function to filter the formatted forecast figures #
      #####################################################

      filteredForecastsFigure <- filteringFormattedIndivFigs(formattedForecast.input = figuresForecast$figure, # Formatted forecast
                                                             modelFilterFF.input = input$modelsForecastFigs, # Model filter
                                                             locationFilterFF.input = input$locationsForecastFigs, # Location filter
                                                             calibrationFilterFF.input = input$calibrationForecastFigs, # Calibration period length filter
                                                             indicator.input = formattedForecastMAINIndicator()) # Indicator for filtering

      ############################################
      # Saving the results to the reactive value #
      ############################################
      finalForecastFigureCombined$data <- filteredForecastsFigure

    } # End of 'if' statement for filtering

  }) # End of 'observe'


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
# figures by location, forecast period, and calibration period length. Each    #
# panel has graphs for the models selected in the side panel.                  #
#------------------------------------------------------------------------------#

  ###################################################################
  # Initialize `reactiveValues` to store the forecast figure panels #
  ###################################################################
  figuresForecastPanel <- reactiveValues()

  #####################################################
  # Observing changes in inputs/other reactive values #
  #####################################################
  observeEvent(eventsTriggerIFig(), ignoreInit = T, {

    # Requiring the model input
    req(input$modelType)

    # Requiring the location
    req(input$locations)

    # Requiring forematted forecasts to run
    if(!is.null(foremattedForecasts$forecasts)){

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
                                                   dotSize.input = dotSizeReactiveFFPanel$sizeVal, # Dot size
                                                   linetype.input = lineTypeFFPanel$type, # Median line type
                                                   lineColor.input = lineColorFFPanel$color, # Median line color
                                                   lineWidth.input = lineWidthFFPanel$width, # Median line width
                                                   dotColor.input = dotColorReactiveFFPanel$color, # Dot color
                                                   boundtype.input = lineTypeBoundsFFPanel$type, # Bounds line type
                                                   boundWidth.input = lineWidthBoundsFFPanel$width, # Bound width
                                                   boundColor.input = lineColorBoundsFFPanel$color, # Bound color
                                                   ribbonColor.input = ribbonColorFFPanel$color, # Ribbon color
                                                   yLabelSize.input = yAxisLabSizeFFPanel$size, # Y-axis label size
                                                   yLabelFace.input = yAxisLabFaceFFPanel$face, # Y-Axis label face
                                                   yTickSize.input = yAxisTickLabelSizeFFPanel$size, # Y-Axis tick size
                                                   yTickBreaks.input = yAxisBreaksFFPanel$value, # Y-Axis breaks
                                                   xAxisLabel.input = xAxisLabelFFPanel$title, # X-Axis Label
                                                   xAxisLabelSize.input = xAxisLabelSizeFFPanel$size, # X-Axis label size
                                                   xAxisLabelFace.input = xAxisLabFaceFFPanel$face, # X-axis label face
                                                   xAxisTickSize.input = xAxisTickSizeFF$size, # X-Axis tick size
                                                   quantile.input = input$quantileSelection) # Quantile input

             # Saving the exported list to a reactive value
             figuresForecastPanel$figure <- panelOutput

        }) # End of 'isolate' statement

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

      finalFiguresDashboard$data <- figuresForecastPanel$figure

    #################################
    # Showing the individual figure #
    #################################
    }else{

      finalFiguresDashboard$data <- rev(finalForecastFigureCombined$data)

    }

  }) # End of 'observe'


#------------------------------------------------------------------------------#
# Wiping the plots if the data set is changed ----------------------------------
#------------------------------------------------------------------------------#
# About: This section wipes the existing plots if the primary data set is      #
# changed.                                                                     #
#------------------------------------------------------------------------------#

  observeEvent(clearingOut(), {

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

   output$panelForecastTitle <- renderText({

     ######################
     # Rendering no title #
     ######################
     if(is.null(finalForecastCombined$data)){

       return(NULL)

     #######################
     # Rendering the title #
     #######################
     }else{

       return(names(finalFiguresDashboard$data[current_index_formatted()]))

     }

   }) # End of title rendering


#------------------------------------------------------------------------------#
# Rendering the plots ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the correct plots, and allows the arrows to work.#
#------------------------------------------------------------------------------#

  output$Forecast.Figure <- renderPlotly({

    ################################################
    # Returning the ggplotly to the main dashboard #
    ################################################
    return(

      ggplotly(finalFiguresDashboard$data[[current_index_formatted()]], tooltip = "text")

    )

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
# About: This section uses the figure specifications from the above menu pop-  #
# up to save the forecast figures or panels. If there is only one figure, the  #
# code will output 1 figure. If there are multiple figures, a '.zip' file with #
# the figures will be saved the user's directory.                              #
#------------------------------------------------------------------------------#

  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({

    #########################
    # Saving a single image #
    #########################
    if(length(finalFiguresDashboard$data) == 1){

      ##############################################
      # Creating the option to download the figure #
      ##############################################
      output$downloadForecastFigure <- downloadHandler(

        ####################################
        # Function to create the file-name #
        ####################################
        filename = function() {

          # Closing the figure specification
          removeModal()

          # File name
          paste("Forecast-Figure.", input$extFig, sep = "")

        },

        #############################
        # Function to save the file #
        #############################
        content = function(file) {

          # Static version of plot
          figure <- finalFiguresDashboard$data[[1]]

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

    #######################
    # Saving a zip folder #
    #######################
    }else{

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

    } # End of 'if-else' for type of saved object

  }) # End of 'observe'

    
      


# MODEL METRICS PAGE -----------------------------------------------------------
  
#------------------------------------------------------------------------------#
# Reading in the updated time series '.csv' ------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the updated time series for evaluation aganist  #
# the forecasts produced in earlier steps. It is used when calculating the     #
# MSE, MAE, WIS, and 95% PI for GAM, GLM, and Prophet for model fits and for   #
# all four models when evaluating model forecast performance.                  #
#------------------------------------------------------------------------------#
  
  ######################################
  # Reactive value to save data within #
  ######################################
  updatedDataValueMETRICS <- reactiveValues()
  
  ###########################
  # Reading in the new data #
  ###########################
  updatedFileMETRICS <- reactive({
    
    # Runs if no error occurs
    tryCatch({
      
      ############################################
      # Name of file from the 'fileInput' picker #
      ############################################
      file1 <- input$UpdatedMetricdataset
      
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
        
      ###########################
      # Runs if no error occurs #
      ###########################
      }else{
        
        NULL
        
      } # End of if-else checking for error
      
      #######################
      # Reading in the data #
      #######################
      return(read.csv(file1$datapath, header = T, check.names=FALSE))
      
    ###########################
    # Runs if an error occurs #
    ###########################
    }, error = function(e){
      
      NULL
      
    }) # End of 'tryCatch'
    
  }) # End of "observe"
 
#------------------------------------------------------------------------------#
# Checking the uploaded data against the original ------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the updated data against the original time series #
# data to make sure they capture the same thing. If it does not, the reactive  #
# value which holds the updated data is cleared.                               #
#------------------------------------------------------------------------------#
  
  #####################################################
  # Observing changes in inputs/other reactive values #
  #####################################################
  observe({
    
    # Requiring original data
    req(file())
    
    # Saving the data under the reactive value
    updatedDataValueMETRICS$data <- updatedFileMETRICS()
    
    ########################################################
    # Checking the uploaded data against the original data #
    ########################################################
    if(!is.null(updatedFileMETRICS())){
      
      ###################################
      # Checking the length of the data #
      ###################################
      if(nrow(updatedFileMETRICS()) <= nrow(file())){
        
        # Returning an error
        shinyalert("Please check the updated time series. It should include
                    dates past that included in the orignal time series.",
                   type = "error")
        
        # Clearing the updated data
        updatedDataValueMETRICS$data <- NULL
        
      }
      
      ########################################################
      # Checking the date make-up of the updated time series #
      ########################################################
      
      # Updated data
      dataUpdated <- updatedFileMETRICS()
      
      # Sub-setting dates 
      datesUpdated <- dataUpdated[,1]
      
      # Determining the date type
      updatedDateType <- date.type.function(dates.input = datesUpdated)
      
      # Checking the date type against that of the orignal 
      if(updatedDateType != dateValues$dates){
        
        # Returning an error
        shinyalert("Please check the updated time series. The time make-up of the 
                    updated data does not match that included in the orignal time series.",
                   type = "error")
        
        # Clearing the updated data
        updatedDataValueMETRICS$data <- NULL
        
      }
      
      ###################################################
      # Checking the location names of the updated data #
      ###################################################
      if(any(c(colnames(updatedFileMETRICS())) %!in% c(colnames(file())))){
        
        # Returning an error
        shinyalert("Please check the updated time series. The included groups or
                    locations do not match that included in the orignal time series.",
                   type = "error")
        
        # Clearing the updated data
        updatedDataValueMETRICS$data <- NULL
        
      }
      
    } # End of "if-else" checking for NULL results
    
  }) # End of 'observe' statement 
  

#------------------------------------------------------------------------------#
# Determining which data to use in model evaluation ----------------------------
#------------------------------------------------------------------------------#
# About: This section determines which data should be used in the model        #
# and forecast evaluation process.                                             #
#------------------------------------------------------------------------------#
  
  ######################################
  # Reactive value for data set to use #
  ######################################
  dataForEvaluation <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    ###########################
    # Using the original data #
    ###########################
    if(is.null(updatedDataValueMETRICS$data) || input$clearUpdateDataMetrics){
      
      dataForEvaluation$data <- file()

    ##########################
    # Using the updated data #
    ##########################
    }else{
      
      dataForEvaluation$data <- updatedDataValueMETRICS$data
      
    }
    
  })
  
  #########################################################
  # Clearing the evaluation data when the file is changed #
  #########################################################
  observeEvent(clearingOut(),{
    
    dataForEvaluation$data <- file()
    
  })
  

#------------------------------------------------------------------------------#
# Model Fit Metrics ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the model fit metrics and loads that          #
# calculated during the model fit process above. It requires the quantile      #
# forecasts, comparison data (original or comparison), and other values        #
# related to the forecasts. The outputs of this section include the crude      #
# mean squared error (MSE), mean absolute error (MAE), weighted interval       #
# score (WIS), and prediction interval coverage. The PI coverage value is      #
# determined by the quantile selected for the forecast. Finally, the metrics   #
# are combined within the AIC, BIC, and AICc calculated for the ARIMA, GLM,    #
# and GAM model fits.                                                          #
#------------------------------------------------------------------------------#
  
  ##############################################################
  # Initialize 'reactiveValues' to store the model fit metrics #
  ##############################################################
  modelFitMetricsList <- reactiveValues()

  ########################################
  # Observing changes in input variables #
  ########################################
  observe({

    # Creating the quantile variable
    quantileList <- c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet)

    # Requiring the comparison data
    req(dataForEvaluation$data)

    ###################################################################
    # Obtaining model fit metrics if quantile forecasts are available #
    ###################################################################
    if(all(!is.null(quantileList))){

      # Function to produce model fit metrics
      modelFitOutput <- modelFitMetrics(crude.data.input = dataForEvaluation$data, # Crude data
                                        date.Type.input = dateValues$dates, # Date type
                                        quantile.list.input = quantileList, # List of quantile forecasts
                                        ARIMAFit = ARIMAInfoMetrics$metrics, # ARIMA fit metrics
                                        GAMFit = GAMInfoMetrics$metrics, # GAM fit metrics
                                        GLMFit = GLMInfoMetrics$metrics, # GLM fit metrics
                                        selectedQuantile = input$quantileSelection) # Selected quantile

      # Saving the data frame under the reactive value
      modelFitMetricsList$fitMetrics <- modelFitOutput

    ##########################################################################
    # Returning a NULL reactive value if no quantile forecasts are available #
    ##########################################################################
    }else{

      # Returning a NULL
      modelFitMetricsList$fitMetrics <- NULL

      }

    }) # End of 'observe'


#------------------------------------------------------------------------------#
# Forecast Metrics ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the model forecast metrics associated with    #
# the produced quantile forecasts. The produced forecast metrics include MAE,  #
# MSE, WIS, and PI based on the quantile selected by the user during the       #
# model fitting process. The users have the option of loading in "updated"     #
# data which can be used in the forecast evaluation process.                   #
#------------------------------------------------------------------------------#

  #############################################################
  # Initialize `reactiveValues` to store the forecast metrics #
  #############################################################
  forecastMetricsListCrude <- reactiveValues()

  ############################
  # Observing changes inputs #
  ###########################
  observe({

    # Creating the quantile variable
    quantileList <- c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet)

    # Requiring the comparison data
    req(dataForEvaluation$data)

    ########################################
    # Function to produce forecast metrics #
    ########################################
    if(!is.null(quantileList)){

    forecastMetricsList <- forecastingMetrics(crude.data.input = dataForEvaluation$data, # Crude data
                                              horizon.input = input$forecastHorizon, # Horizon
                                              date.Type.input = dateValues$dates, # Date type
                                              quantile.list.input = quantileList, # Quantile list
                                              selectedQuantile = input$quantileSelection) # Selected quantile
                                        

    # Adding it to the reactive value
    forecastMetricsListCrude$forecastMetrics <- forecastMetricsList

    ##########################################################################
    # Returning a NULL reactive value if no quantile forecasts are available #
    ##########################################################################
    }else{

      # Returning a NULL
      forecastMetricsListCrude$forecastMetrics <- NULL

    }

  }) # End of 'observe'


#------------------------------------------------------------------------------#
# Determining which crude metrics to show --------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines which metrics should be actively shown on the #
# dashboard page. It is totally dependent on the page that is open. It also    #
# returns an error if there is not enough data to evaluate the forecasts.      #
# However, users can upload updated data to compare their forecast against.    #
# This is a common practice when evaluating real-time forecasts as part of a   #
# retrospective analysis.                                                      #
#------------------------------------------------------------------------------#

  ##########################################################
  # Initialize `reactiveValues` to store the shown metrics #
  ##########################################################
  modelMetricsCrude <- reactiveValues()

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    # Data frame to store metrics
    metrics <- NULL

    # Creating the quantile variable
    quantileList <- c(ARIMAInfo$arima, GAMList$GAM, GLMList$GLM, ProphetList$prophet)

    ####################################
    # Runs if the model fit is selected #
    #####################################
    if(input$metricsToShow == "Model Fit"){

      # Using model fit metrics
      metrics <- modelFitMetricsList$fitMetrics

    ########################################
    # Runs if forecast metrics is selected #
    ########################################
    }else if(input$metricsToShow == "Forecasts" & input$my_picker == "Model Metrics" & !is.null(forecastMetricsListCrude$forecastMetrics)){

      #####################################################
      # Handling when some forecasts can not be evaluated #
      #####################################################
      if(nrow(forecastMetricsListCrude$forecastMetrics) != length(quantileList)){

        # Alert
        shinyalert("There is not enough data to evaluate the forecast performance of some included forecasts and produce subsequent Winkler Scores." , type = "warning")

        # Forecast metrics
        metrics <- forecastMetricsListCrude$forecastMetrics

      #################################################
      # Handling if forecast methods can be evaluated #
      #################################################
      }else{

        # No alert occurs
        metrics <- forecastMetricsListCrude$forecastMetrics

      }

    } # End of 'if-else'

    ###############################################
    # Adding the final list to the reactive value #
    ###############################################
    modelMetricsCrude$metricsList <- metrics

  }) # End of 'observe'

#------------------------------------------------------------------------------#
# Clearing out the base crude metrics ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears out the above reactive values that contain crude  #
# metrics. This is triggered when the file name changes or the "Clear"         #
# button is hit.                                                               #
#------------------------------------------------------------------------------#
  
  ####################################
  # Observing event of file changing #
  ####################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value with model metrics to show 
    modelMetricsCrude$metricsList <- NULL
    
    # Clearing the reactive value with model fit metrics 
    modelFitMetricsList$fitMetrics <- NULL
    
    # Clearing the reactive value with model forecast metrics 
    forecastMetricsListCrude$forecastMetrics <- NULL
    
  })
  
#------------------------------------------------------------------------------#
# Creating the options and filtering the crude metrics data --------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options for the pop-up, and        #
# applies the filtering to the crude metrics. The filtering is triggered when  #
# the `Filtering Options` button is clicked.                                   #
#------------------------------------------------------------------------------#
  
  #######################################
  # Reactive value: Filtered crude data #
  #######################################
  crudeMetricsFiltered <- reactiveValues()
  
  ####################################
  # Indicator for which data to show #
  ####################################
  indicatorForFilterMetricsCrudeDASH <- reactiveVal(0)
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring metrics
     req(modelMetricsCrude$metricsList)
    
    # Requiring the calibration period
    req(input$calibrationPeriod)
    
    ##################################################
    # Creating the pop-up for crude metric filtering #
    ##################################################
    observeEvent(input$filterCrudeMetrics, ignoreInit = T,{
      
      # Changing the indicator to one (i.e., button has been clicked)
      indicatorForFilterMetricsCrudeDASH(1)
      
      # Button
      showModal(modalDialog(
        title = "Filtering Options",
        pickerInput("modelMetrics", "Model:", c(input$modelType), selected = c(input$modelType), multiple = T), # Model filtering
        pickerInput("calibrationMetrics", "Calibration period length:", c(input$calibrationPeriod), selected = c(input$calibrationPeriod), multiple = T), # Calibration period length
        pickerInput("locationMetrics", "Location:", c(input$locations), selected = c(input$locations), multiple = T), # Location

      ))
      
    }) # End of 'observeEvent'
    
    ##################################################
    # Filtering the crude metrics data: No filtering #
    ##################################################
    if(indicatorForFilterMetricsCrudeDASH() == 0){
      
      filteredMetrics <- modelMetricsCrude$metricsList 
      
    ###############################################
    # Filtering the crude metrics data: Filtering #
    ###############################################
    }else{
      
      filteredMetrics <- modelMetricsCrude$metricsList %>%
        dplyr::filter(Model %in% c(input$modelMetrics),
                      Location %in% c(input$locationMetrics),
                      Calibration %in% c(input$calibrationMetrics))
      
    }
    
    ##########################################
    # Saving the results to a reactive value #
    ##########################################
    crudeMetricsFiltered$metricsFULL <- filteredMetrics
    
    })
  
#------------------------------------------------------------------------------#
# Clearing out the base crude metrics ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears out the above reactive values that contain the    #
# filtered crude metrics. It also resets the indicator for filtering the crude #
# metrics. It is triggered when the original file is changed, the clear button #
# is hit, or there is no data for evaluation.                                  #             
#------------------------------------------------------------------------------#
  
  #########################################################
  # Observing event of file changing or button is cleared #
  #########################################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value
    crudeMetricsFiltered$metricsFULL <- NULL
    
    # Clearing the reactive value with model metrics to show 
    modelMetricsCrude$metricsList <- NULL
    
    # Clearing the reactive value with model fit metrics 
    modelFitMetricsList$fitMetrics <- NULL
    
    # Clearing the reactive value with model forecast metrics 
    forecastMetricsListCrude$forecastMetrics <- NULL
    
    # Resetting the indicator
    indicatorForFilterMetricsCrudeDASH(0)
    
  })
  
  ############################################
  # Observing changes in the evaluation data #
  ############################################
  observe({
    
    # Running if the evaluation data is NULL
    if(is.null(dataForEvaluation$data)){
      
      # Clearing the reactive value
      crudeMetricsFiltered$metricsFULL <- NULL
      
      # Clearing the reactive value with model metrics to show 
      modelMetricsCrude$metricsList <- NULL
      
      # Clearing the reactive value with model fit metrics 
      modelFitMetricsList$fitMetrics <- NULL
      
      # Clearing the reactive value with model forecast metrics 
      forecastMetricsListCrude$forecastMetrics <- NULL
      
      # Resetting the indicator
      indicatorForFilterMetricsCrudeDASH(0)
      
    } # End of 'if'
    
  }) # End of 'observe'
  

  
#------------------------------------------------------------------------------#
# Rendering the data frame for crude metrics -----------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the filtered data table containing the crude     #
# forecasting or model fit metrics. This is updated whenever the filtering     #
# options are shifted.                                                         #
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
# Downloading the crude metrics as a '.csv' file -------------------------------
#------------------------------------------------------------------------------#
# About: This section provides the back-bone to downloading the crude metrics  #
# data set to the users' system.                                               #
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
# Creating the options for the crude metric figure -----------------------------
#------------------------------------------------------------------------------#
# About: This section creates the figure customization options that show when  #
# the edit figure button is clicked.                                           #
#------------------------------------------------------------------------------#
  
  ####################################
  # Reactive value for the MSE label #
  ####################################
  MSELabel <- reactiveValues(text = "MSE")

  ####################################
  # Reactive value for the MAE label #
  ####################################
  MAELabel <- reactiveValues(text = "MAE")

  ####################################
  # Reactive value for the WIS label #
  ####################################
  WISLabel <- reactiveValues(text = "WIS")

  ###################################
  # Reactive value for the PI label #
  ###################################
  PILabel <- reactiveValues(text = "PI")

  #######################################
  # Reactive value for the Y-Label Size #
  #######################################
  yLabelSizeCM <- reactiveValues(size = 12)

  #######################################
  # Reactive value for the Y-Label face #
  #######################################
  yLabelFaceCM <- reactiveValues(face = "Plain")

  ##################################################
  # Reactive value for the Y-Axis breaks text size #
  ##################################################
  yTextSizeCM <- reactiveValues(size = 12)

  ########################################
  # Reactive value for the Y-Axis breaks #
  ########################################
  yAxisBreaksCM <- reactiveValues(breaks = 6)

  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYCrudeMetric <- reactiveValues(logScale = NULL)

  #################################################
  # Reactive value for start point for the y-axis #
  #################################################
  startYCM <- reactiveValues(start = "Zero")

  #####################################
  # Reactive value for common X-label #
  #####################################
  commonXLabelCM <- reactiveValues(label = F)

  ###################################
  # Reactive value for X-Axis Label #
  ###################################
  xAxisLabelCM <- reactiveValues(label = "")

  #######################################
  # Reactive value for the X-Label Size #
  #######################################
  xLabelSizeCM <- reactiveValues(size = 12)

  #######################################
  # Reactive value for the X-Label face #
  #######################################
  xLabelFaceCM <- reactiveValues(face = "Plain")

  ##################################################
  # Reactive value for the X-Axis breaks text size #
  ##################################################
  xTextSizeCM <- reactiveValues(size = 12)

  ############################################
  # Reactive value for number of date breaks #
  ############################################
  xDateBreaksCM <- reactiveValues(breaks = 1)

  #########################################
  # Reactive value for showing the legend #
  #########################################
  showLegendCM <- reactiveValues(show = T)

  ######################################
  # Reactive value for legend location #
  ######################################
  legendLocationCM <- reactiveValues(location = "Right")

  ############################
  # Reactive color for ARIMA #
  ############################
  ARIMAColorCM <- reactiveValues(color = "#0072B2")

  ##########################
  # Reactive color for GLM #
  ##########################
  GLMColorCM <- reactiveValues(color = "#D55E00")

  ##########################
  # Reactive color for GAM #
  ##########################
  GAMColorCM <- reactiveValues(color = "#009E73")

  ##############################
  # Reactive color for Prophet #
  ##############################
  ProphetColorCM <- reactiveValues(color = "#CC79A7")

  ##########################################
  # Reactive value for showing bar numbers #
  ##########################################
  showBarNumsCM <- reactiveValues(show = T)

  ######################################
  # Reactive value for bar number size #
  ######################################
  barNumberSizeCM <- reactiveValues(size = 6)

  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$figOptCRUDEMetrics, {

    # Creating the pop-up
    showModal(modalDialog(

    # Title
    title = "Figure Options",

    # Creating the tabbed menu
    tabsetPanel(

      ##################
      # Y-Axis Options #
      ##################
      tabPanel("Y-Axis",
               tags$h4("Primary Label Options"), # Labels title
               tags$h5("Renaming the Labels"),
               textInput("mseLabelCrudeMetric", "Mean Squared Error: ", value = MSELabel$text),
               textInput("maeLabelCrudeMetric", "Mean Absolute Error: ", value = MAELabel$text),
               textInput("wisLabelCrudeMetric", "Weighted Interval Score: ", value = WISLabel$text),
               textInput("PILabelCrudeMetric", "Prediction Interval: ", value = PILabel$text),
               tags$h5("Text Options"),
               numericInput("yLabelTextSizeCM", "Label Size: ", value = yLabelSizeCM$size),
               pickerInput("yLabalfaceCM", "Label Face: ", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = yLabelFaceCM$face),
               tags$h4("Axis Options"),
               numericInput("axisTextSizeCM", "Tick label size: ", value = yTextSizeCM$size),
               numericInput("yAxisBreaksCM", "Axis breaks: ", value =  yAxisBreaksCM$breaks),
               pickerInput("logScaleCrudeMetric", "Metrics to Show in Log Base 10:", c("MSE", "MAE", "WIS", "PI"), selected = scaleYCrudeMetric$logScale, multiple = T),
               tags$h4("Time-series Specific"),
               pickerInput("startYCrudeMetric", "Start Value:", choices = c("Zero", "Minimum value in data."), selected = startYCM$start, multiple = F)

               ),

      ##################
      # X-Axis Options #
      ##################
      tabPanel("X-Axis",
               tags$h4("Primary Label Options"), # Labels title
               textInput("xAxisLabelCrude", label = "Label: ", value =  xAxisLabelCM$label),
               checkboxInput("commonXLabelCM", "Common X-Axis Label ", value = commonXLabelCM$label),
               numericInput("xLabelTextSizeCM", "Label Size: ", value = xLabelSizeCM$size),
               pickerInput("xLabelfaceCM", "Label Face: ", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = xLabelFaceCM$face),
               tags$h4("Axis Options"),
               numericInput("axisTextSizeXCM", "Tick label size: ", value = xTextSizeCM$size),
               tags$h4("Time-series Specific"),
               numericInput("xAxisBreaksCrude", "Date Breaks: ", xDateBreaksCM$breaks)

               ),

      ##################
      # Legend Options #
      ##################
      tabPanel("Legend",
               tags$h4("Legend Label Options"), # Labels title
               checkboxInput("showLegendCrude", "Show the figure legend.", value = showLegendCM$show),
               pickerInput("legendPositionCM", "Legend Location: ", choices = c("Right", "Left", "Top", "Bottom"), selected = c(legendLocationCM$location), multiple = F),
               tags$h4("Legend Color Options"),
               textInput("ARIMAColorCrude", "Indicate the color for the ARIMA model (Color or #): ", value =  ARIMAColorCM$color),
               textInput("GLMColorCrude", "Indicate the color for the GLM model (Color or #): ", value =  GLMColorCM$color),
               textInput("GAMColorCrude", "Indicate the color for the GAM model (Color or #): ", value =  GAMColorCM$color),
               textInput("ProphetColorCrude", "Indicate the color for the Prophet model (Color or #): ", value =  ProphetColorCM$color),
               tags$h4("Bar-chart Specific"),
               checkboxInput("barNumsCrude", "Show bar-chart numbers.", value = showBarNumsCM$show),
               numericInput("barNumsSizeCrude", "Bar-chart Number Size: ", value = barNumberSizeCM$size)

               )

    ) # End of 'tabsetPanel'

    )) # End of 'showModal'

  }) # End of 'observeEvent'


  #########################################
  # Update the reactive value - MSE label #
  #########################################
  observeEvent(input$mseLabelCrudeMetric, {

    MSELabel$text <- input$mseLabelCrudeMetric

  })

  #########################################
  # Update the reactive value - MAE label #
  #########################################
  observeEvent(input$maeLabelCrudeMetric, {

    MAELabel$text <- input$maeLabelCrudeMetric

  })

  #########################################
  # Update the reactive value - WIS label #
  #########################################
  observeEvent(input$wisLabelCrudeMetric, {

    WISLabel$text <- input$wisLabelCrudeMetric

  })

  ########################################
  # Update the reactive value - PI label #
  ########################################
  observeEvent(input$PILabelCrudeMetric, {

    PILabel$text <- input$PILabelCrudeMetric

  })

  ################################################
  # Update the reactive value - Y-Axis text size #
  ################################################
  observeEvent(input$yLabelTextSizeCM, {

    yLabelSizeCM$size <- input$yLabelTextSizeCM

  })

  #################################################
  # Update the reactive value - Y-Axis label face #
  #################################################
  observeEvent(input$yLabalfaceCM, {

    yLabelFaceCM$face <- input$yLabalfaceCM

  })

  ################################################
  # Update the reactive value - Y-Axis tick size #
  ################################################
  observeEvent(input$axisTextSizeCM, {

    yTextSizeCM$size <- input$axisTextSizeCM

  })

  #############################################
  # Update the reactive value - Y-Axis breaks #
  #############################################
  observeEvent(input$yAxisBreaksCM, {

    yAxisBreaksCM$breaks <- input$yAxisBreaksCM

  })

  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleCrudeMetric, {

    scaleYCrudeMetric$logScale <- input$logScaleCrudeMetric

  })

  ###############################################
  # Update the reactive value - start of y-axis #
  ###############################################
  observeEvent(input$startYCrudeMetric, {

    startYCM$start <- input$startYCrudeMetric

  })

  ############################################
  # Update the reactive value - X-Axis Label #
  ############################################
  observeEvent(input$xAxisLabelCrude, {

    xAxisLabelCM$label <- input$xAxisLabelCrude

  })

  ###################################################
  # Update the reactive value - Common X-Axis Label #
  ###################################################
  observeEvent(input$commonXLabelCM, {

    commonXLabelCM$label <- input$commonXLabelCM

  })

  ################################################
  # Update the reactive value - X-Axis text size #
  ################################################
  observeEvent(input$xLabelTextSizeCM, {

    xLabelSizeCM$size <- input$xLabelTextSizeCM

  })

  #################################################
  # Update the reactive value - X-Axis label face #
  #################################################
  observeEvent(input$xLabelfaceCM, {

    xLabelFaceCM$face <- input$xLabelfaceCM

  })

  ################################################
  # Update the reactive value - X-Axis tick size #
  ################################################
  observeEvent(input$axisTextSizeXCM, {

    xTextSizeCM$size <- input$axisTextSizeXCM

  })

  ##################################################
  # Update the reactive value - X-Axis date breaks #
  ##################################################
  observeEvent(input$xAxisBreaksCrude, {

    xDateBreaksCM$breaks <- input$xAxisBreaksCrude

  })

  ##################################################
  # Update the reactive value - Show figure legend #
  ##################################################
  observeEvent(input$showLegendCrude, {

    showLegendCM$show <- input$showLegendCrude

  })

  #################################################
  # Updating the reactive value - Legend Location #
  #################################################
  observeEvent(input$legendPositionCM, {

    legendLocationCM$location <- input$legendPositionCM

  })

  #############################################
  # Updating the reactive value - ARIMA Color #
  #############################################
  observeEvent(input$ARIMAColorCrude, {

    ARIMAColorCM$color <- input$ARIMAColorCrude

  })

  ###########################################
  # Updating the reactive value - GLM Color #
  ###########################################
  observeEvent(input$GLMColorCrude, {

    GLMColorCM$color <- input$GLMColorCrude

  })

  ###########################################
  # Updating the reactive value - GAM Color #
  ###########################################
  observeEvent(input$GAMColorCrude, {

    GAMColorCM$color <- input$GAMColorCrude

  })

  ###############################################
  # Updating the reactive value - Prophet Color #
  ###############################################
  observeEvent(input$ProphetColorCrude, {

    ProphetColorCM$color <- input$ProphetColorCrude

  })

  ##################################################
  # Updating the reactive value - Show bar numbers #
  ##################################################
  observeEvent(input$barNumsCrude, {

    showBarNumsCM$show <- input$barNumsCrude

  })

  ##################################################
  # Updating the reactive value - Bar numbers size #
  ##################################################
  observeEvent(input$barNumsSizeCrude, {

    barNumberSizeCM$size <- input$barNumsSizeCrude

  })

#------------------------------------------------------------------------------#
# Creating the crude metric figures --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the figures containing the crude metrics. There  #
# are two possible options, even a time-series or bar-chart. If there is one   #
# forecast date selected, a bar-chart is exported. If there are many dates     #
# selected, a time-series is exported. It also customizes the figures based    #
# on the user-selected options.                                                #
#------------------------------------------------------------------------------#

  ######################################################################
  # Initialize `reactiveValues` to store the model fit metrics figures #
  ######################################################################
  modelCrudePlot <- reactiveValues()

  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({

    # Requiring the crude metrics to run
    req(crudeMetricsFiltered$metricsFULL)

    ##################################
    # Function to produce panel plot #
    ##################################
    if(nrow(crudeMetricsFiltered$metricsFULL) > 0){

      metricPanelCrude <- CrudeMetricsFigure(crudeMetrics = crudeMetricsFiltered$metricsFULL,
                                             dateType = dateValues$dates,
                                             scaleY.input = input$logScaleCrudeMetric,
                                             MSELabel.input = MSELabel$text,
                                             MAELabel.input = MAELabel$text,
                                             WISLabel.input = WISLabel$text,
                                             PILabel.input = PILabel$text,
                                             YAxisLabelSize.input = yLabelSizeCM$size,
                                             YAxisLabelFace.input = yLabelFaceCM$face,
                                             YAxisTickSize.input = yTextSizeCM$size,
                                             YAxisBreaks.input = yAxisBreaksCM$breaks,
                                             YAxisStart.input = startYCM$start,
                                             xAxisLabel.input = xAxisLabelCM$label,
                                             xCommonLabel.input = commonXLabelCM$label,
                                             XAxisLabelSize.input = xLabelSizeCM$size,
                                             XAxisLabelFace.input = xLabelFaceCM$face,
                                             XAxisTickSize.input = xTextSizeCM$size,
                                             xAxisBreaks.input = xDateBreaksCM$breaks,
                                             showLegend.input = showLegendCM$show,
                                             legendPosition.input = legendLocationCM$location,
                                             ARIMAColor.input = ARIMAColorCM$color,
                                             GLMColor.input = GLMColorCM$color,
                                             GAMColor.input = GAMColorCM$color,
                                             ProphetColor.input = ProphetColorCM$color,
                                             showBarNum.input = showBarNumsCM$show,
                                             barNumSize.input = barNumberSizeCM$size)
      
      #############################################################
      # Returning an error if some things could not be calculated #
      #############################################################
      if(all(metricPanelCrude == "ERROR" &  input$my_picker == "Model Metrics")){
        
        # Error
        shinyalert(title = "Warning", text = "Some features of the model fit page are unavailable for the ARIMA model.", type = "warning")
        
        # Returning a NULL
        modelCrudePlot$figures <- NULL
        
      }

      ############################################
      # Adding the figures to the reactive value #
      ############################################
      modelCrudePlot$figures <-  metricPanelCrude

    } # End of 'if'

  }) # End of 'observe'


#------------------------------------------------------------------------------#
# Clearing out the crude metrics figures ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears out the above reactive values that contain the    #
# crude metrics figures. It is triggered when the original file is changed,    #
# the clear button is hit, or there is no data for evaluation.                 #
#------------------------------------------------------------------------------#

  #########################################################
  # Observing event of file changing or button is cleared #
  #########################################################
  observeEvent(clearingOut(),{

    # Clearing the list of crude figures
    modelCrudePlot$figures <- NULL

  })

  ############################################
  # Observing changes in the evaluation data #
  ############################################
  observe({

    # Running if the evaluation data is NULL
    if(is.null(dataForEvaluation$data)){

      # Clearing the list of crude figures
      modelCrudePlot$figures <- NULL

    } # End of 'if'

  }) # End of 'observe'


#------------------------------------------------------------------------------#
# Setting up the forward and backwards arrows for crude figures ----------------
#------------------------------------------------------------------------------#
# About: This section creates the forward and backwards buttons for navigating #
# through the crude metrics figures. Additionally, it resets the figure list   #
# back to one when any of the filtering options for the crude metrics is       #
# changed.
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

  #########################################################################
  # Fixes the index when the data is filtered - Calibration period length #
  #########################################################################
  observeEvent(input$calibrationMetrics, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_Metrics(1)

    }) # End of isolate

  }) # End of 'observeEvent'

#------------------------------------------------------------------------------#
# Rendering the title for each of the figures ----------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the figures for the crude metrics. The title     #
# that is shown depends on the type of plot rendered: bar-chart or             #
# time-series.                                                                 #
#------------------------------------------------------------------------------#

  output$CrudeMetricsTitle <- renderText({

    # Rendering the title of the figure
    return(names(modelCrudePlot$figures)[current_index_Metrics()])

  }) # End of 'renderPlot'


#------------------------------------------------------------------------------#
# Rendering each of the crude metrics figures ----------------------------------
#------------------------------------------------------------------------------#
# About: This section creates each of the figures shown in the main UI         #
# interface. The inclusion of the `current_index_Metrics()` is what allows     #
# for the functionality of the forward and backwards buttons.                  #
#------------------------------------------------------------------------------#

   output$CrudeMetricsFigure <- renderPlot({

       # Rendering the data table
       return(modelCrudePlot$figures[[current_index_Metrics()]])

   }) # End of 'renderPlot'

#------------------------------------------------------------------------------#
# Creating the crude metrics figure download options ---------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up for the download button associated    #
# with the crude metrics figures box. It allows users to select the dpi,       #
# height, width, unit of measurement for size, and type of photo.              #
# Additionally, it allows users to download the figure with the user           #
# specifications.                                                              #
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
# About: This section uses the figure specifications from the above menu pop-  #
# up to save the crude metrics figures. If there is only one figure, the       #
# code will output 1 figure. If there are multiple figures, a '.zip' file with #
# the figures will be saved the user's directory.                              #
#------------------------------------------------------------------------------#

  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({

    #########################
    # Saving a single image #
    #########################
    if(length(modelCrudePlot$figures) == 1){

      ##############################################
      # Creating the option to download the figure #
      ##############################################
      output$downloadCrudeMetricsFigure <- downloadHandler(

        ####################################
        # Function to create the file-name #
        ####################################
        filename = function() {

          # Closing the figure specification
          removeModal()

          # File name
          paste("Crude-Metrics-Figures.", input$extFig, sep = "")

        },

        #############################
        # Function to save the file #
        #############################
        content = function(file) {

          # Static version of plot
          figure <- modelCrudePlot$figures[[1]]

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

    #######################
    # Saving a zip folder #
    #######################
    }else{

      ##############################################
      # Creating the option to download the figure #
      ##############################################
      output$downloadCrudeMetricsFigure <- downloadHandler(

        #####################
        # File name for ZIP #
        #####################
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

    } # End of 'if-else' for download options

  }) # End of 'observe'

  
#------------------------------------------------------------------------------#
# Calculating the average metrics ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the average performance metrics over forecast #
# period date for MSE, MAE, BIC, WIS, PI, AIC, and AICc. It groups the data by #
# location, calibration period length, and model. Therefore, each location,    #
# model, and calibration period length has a list of average metrics           #
# associated with it.                                                          #
#------------------------------------------------------------------------------#
  
   ######################################
   # Reactive value for average metrics #
   ######################################
   avgMetric <- reactiveValues()

   ########################################
   # Observing changes in reactive values #
   ########################################
   observe({

     ###############################
     # Requiring the crude metrics #
     ###############################
     req(modelMetricsCrude$metricsList)

     # Saving the crude metrics under another name
     metrics <- modelMetricsCrude$metricsList

     # List of needed variables
     metricsList <- c("Location", "Model", "Date", "Calibration", "MSE", "MAE", "PI", "WIS", "AICc", "AIC", "BIC")

     #####################################################
     # Running the function to calculate average metrics #
     #####################################################
     averageMetrics <- metrics %>%
       dplyr::select(any_of(metricsList))  # Selecting the needed variables

     # Running if using model fit metrics
     if("AICc" %in% c(colnames(averageMetrics))){

       averageMetrics <- averageMetrics %>%
         dplyr::group_by(Model, Location, Calibration) %>% # Grouping by location and model
         dplyr::mutate(MSE = ifelse("MSE" %in% c(colnames(averageMetrics)), round(mean(MSE), 2), NA), # Avg. MSE
                       MAE = ifelse("MAE" %in% c(colnames(averageMetrics)), round(mean(MAE), 2), NA), # Avg MAE
                       PI = ifelse("PI" %in% c(colnames(averageMetrics)), round(mean(PI), 2), NA), # Avg PI
                       WIS = ifelse("WIS" %in% c(colnames(averageMetrics)), round(mean(WIS), 2), NA), # Avg WIS
                       AICc = ifelse("AICc" %in% c(colnames(averageMetrics)), round(mean(AICc), 2), NA),# Avg AICc
                       AIC = ifelse("AIC" %in% c(colnames(averageMetrics)), round(mean(AIC), 2), NA), # Avg AIC
                       BIC = ifelse("BIC" %in% c(colnames(averageMetrics)), round(mean(BIC), 2), NA)) %>% # Avg BIC
         dplyr::select(-Date) %>% # Removing the date variable
         dplyr::distinct(Model, Location, Calibration, .keep_all = T) # Remove duplicate rows

     # Running if only Prophet is selected, or using model forecast metrics
     }else{

       averageMetrics <- averageMetrics %>%
         dplyr::group_by(Model, Location, Calibration) %>% # Grouping by location and model
         dplyr::mutate(MSE = round(mean(MSE), 2), # Avg. MSE
                       MAE = round(mean(MAE), 2), # Avg. MAE
                       PI = round(mean(PI), 2), # Avg. PI
                       WIS = round(mean(WIS), 2)) %>% # Avg. WIS
         dplyr::select(-Date) %>% # Removing the date variable
         dplyr::distinct(Model, Location, Calibration, .keep_all = T) # Remove duplicate rows

     }

     ############################################
     # Adding the metrics to the reactive value #
     ############################################
      avgMetric$metricsList <- averageMetrics

   }) # End of 'observe'

  
#------------------------------------------------------------------------------#
# Clearing out the base average metrics ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears out the above reactive values that contain        #
# average metrics. This is triggered when the file name changes or the "Clear" #
# button is hit.                                                               #
#------------------------------------------------------------------------------#
  
  ##############################################################
  # Observing event of file changing or clear button being hit #
  ##############################################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value
    avgMetric$metricsList <- NULL
    
  })
  

#------------------------------------------------------------------------------#
# Creating the options and filtering the average metrics data ------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options for the pop-up, and        #
# applies the filtering to the average metrics. The filtering is triggered     #
# when the `Filtering Options` button is clicked.                              #
#------------------------------------------------------------------------------#  

  #####################################################
  # Reactive value to store the filtered average data #
  #####################################################
  avgMetricsFiltered <- reactiveValues()
  
  ####################################
  # Indicator for which data to show #
  ####################################
  indicatorForFilterMetricsAvgDASH <- reactiveVal(0)
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the average metrics 
    req(avgMetric$metricsList)
    
    # Requiring the calibration period
    req(input$calibrationPeriod)
    
    #####################################################
    # Creating the pop-up for average metrics filtering #
    #####################################################
    observeEvent(input$filterAvgMetrics, ignoreInit = T,{
      
      # Changing the indicator to one (i.e., button has been clicked)
      indicatorForFilterMetricsAvgDASH(1)
      
      # Button
      showModal(modalDialog(
        title = "Filtering Options",
        pickerInput("modelAvgMetrics", "Model:", c(input$modelType), selected = c(input$modelType), multiple = T), # Model filtering
        pickerInput("calibrationAvgMetrics", "Calibration period length:", c(input$calibrationPeriod), selected = c(input$calibrationPeriod), multiple = T), # Calibration period length
        pickerInput("locationsAvgMetrics", "Location:", c(input$locations), selected = c(input$locations), multiple = T), # Location
        
      ))
      
    }) # End of 'observeEvent'
    
    ####################################################
    # Filtering the average metrics data: No Filtering #
    ####################################################
    if(indicatorForFilterMetricsAvgDASH() == 0){
      
      filteredMetricsAvg <- avgMetric$metricsList
      
    #################################################
    # Filtering the average metrics data: Filtering #
    #################################################
    }else{
      
      filteredMetricsAvg <- avgMetric$metricsList %>%
        dplyr::filter(Model %in% c(input$modelAvgMetrics),
                      Location %in% c(input$locationsAvgMetrics),
                      Calibration %in% c(input$calibrationAvgMetrics))
      
    }
    
    ##########################################
    # Saving the results to a reactive value #
    ##########################################
    avgMetricsFiltered$metricsFULL <- filteredMetricsAvg
    
  }) # End of 'observe'

  
#------------------------------------------------------------------------------#
# Clearing out the filtered average metrics ------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears out the above reactive values that contain        #
# filtered average metrics and indicator for filtering. This is triggered when #
# the file name changes or the "Clear" button is hit. Additionally, it clears  #                                                            
# the filtered average metrics when the data for evaluation changes.           #
#------------------------------------------------------------------------------#    

  ##############################################################
  # Observing event of file changing or clear button being hit #
  ##############################################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value
    avgMetricsFiltered$metricsFULL <- NULL
    
    # Resetting the indicator
    indicatorForFilterMetricsAvgDASH(0)
    
  })
  
  ############################################
  # Observing changes in the evaluation data #
  ############################################
  observe({
    
    # Running if the evaluation data is NULL
    if(is.null(dataForEvaluation$data)){
      
      # Clearing the reactive value
      avgMetricsFiltered$metricsFULL <- NULL
      
      # Resetting the indicator
      indicatorForFilterMetricsAvgDASH(0)
      
    } # End of 'if'
    
  }) # End of 'observe' 
  

#------------------------------------------------------------------------------#
# Rendering the data frame containing the average metrics ----------------------
#------------------------------------------------------------------------------#
# About: This section renders the data frame containing the average metrics,   #
# including filtering manipulations by the user. It then exports the table to  #
# the main dashboard.                                                          #
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
# Downloading the average metrics as a '.csv' file -----------------------------
#------------------------------------------------------------------------------#
# About: This section allows users to save their average metrics as a '.csv'   #
# to the location of their choosing on their system.                           #
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
# Creating the button to edit the average metrics ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the average metrics figures. #
# The edits selected within the pop-up will then apply to each of the metrics  #
# figures.                                                                     #
#------------------------------------------------------------------------------#

  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYAvgMetric <- reactiveValues(logScale = NULL) 
  
  ########################################
  # Reactive value for the low-end color #
  ########################################
  lowColorAvgMetric <- reactiveValues(lowColor = "Green")
  
  #########################################
  # Reactive value for the high-end color #
  #########################################
  highColorAvgMetric <- reactiveValues(highColor = "Red")
  
  ########################################
  # Reactive value for the outline color #
  ########################################
  outlineColorAvgMetric <- reactiveValues(outlineColor = "Black")
  
  #######################################
  # Reactive value for breaks in metric #
  #######################################
  breaksAvgMetric <- reactiveValues(breaks = 5)
  
  ###################################
  # Reactive value for showing text #
  ###################################
  showTextAvgMetric <- reactiveValues(showText = TRUE)
  
  #################################
  # Reactive value for text color #
  #################################
  textColorAvgMetric <- reactiveValues(textColor = "Black")
  
  ################################
  # Reactive value for text size #
  ################################
  textSizeAvgMetric <- reactiveValues(size = 6)
  
  ########################################
  # Reactive value for legend label size #
  ########################################
  legendlabelsizeAvgMetric <- reactiveValues(size = 14)
  
  #######################################
  # Reactive value for legend tick size #
  #######################################
  legendTickSizeAvgMetric <- reactiveValues(size = 10)
  
  #####################################
  # Reactive value to show the legend #
  #####################################
  showLegendAvgMetric <- reactiveValues(val = T)
  
  ######################################
  # Reactive value for legend position #
  ######################################
  legendPositionAvgMetric <- reactiveValues(position = "Right")
  
  ################
  # Y-Axis label #
  ################
  yAxisLabelAvgMetric <- reactiveValues(text = "")
  
  #####################
  # Y-Axis label face #
  #####################
  yAxisLabelFaceAvgMetric <- reactiveValues(face = "Plain")
  
  #####################
  # Y-Axis label size #
  #####################
  yAxisLabelSizeAvgMetric <- reactiveValues(size = 12)
  
  ####################
  # Y-Axis tick size #
  ####################
  yAxisTickSizeAvgMetric <- reactiveValues(size = 12)
  
  ################
  # x-Axis label #
  ################
  xAxisLabelAvgMetric <- reactiveValues(text = "")
  
  #####################
  # x-Axis label face #
  #####################
  xAxisLabelFaceAvgMetric <- reactiveValues(face = "Plain")
  
  #####################
  # x-Axis label size #
  #####################
  xAxisLabelSizeAvgMetric <- reactiveValues(size = 12)
  
  ####################
  # x-Axis tick size #
  ####################
  xAxisTickSizeAvgMetric <- reactiveValues(size = 12)

  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editAvgMetrics, {
    
    # Creating the pop-up
    showModal(modalDialog(
      
      # Title 
      title = "Figure Options",
      
      tabsetPanel(
        
        ################
        # Y-Axis Label #
        ################
        tabPanel("Y-Axis",
                 tags$h4("Label Options"),
                 textInput("labelOptionsAvgY", "Label: ", value = yAxisLabelAvgMetric$text),
                 pickerInput("labelYFaceAge", "Face: ", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = c(yAxisLabelFaceAvgMetric$face)),
                 numericInput("labelYSizeAvg", "Size: ", value = yAxisLabelSizeAvgMetric$size),
                 tags$h4("Axis Options"),
                 numericInput("TickYSizeAvg", "Size: ", value = yAxisTickSizeAvgMetric$size)
                 ), 
        
        ################
        # X-Axis Label #
        ################
        tabPanel("X-Axis",
                 tags$h4("Label Options"),
                 textInput("labelOptionsAvgx", "Label: ", value = xAxisLabelAvgMetric$text),
                 pickerInput("labelxFaceAge", "Face: ", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = c(xAxisLabelFaceAvgMetric$face)),
                 numericInput("labelxSizeAvg", "Size: ", value = xAxisLabelSizeAvgMetric$size),
                 tags$h4("Axis Options"),
                 numericInput("TickxSizeAvg", "Size: ", value = xAxisTickSizeAvgMetric$size)
        ), 
       
        ################
        # Fill options #
        ################
        tabPanel("Fill",
                 tags$h4("Color Options"), 
                 textInput("lowColorAvg", "Color for the lowest values:", value = lowColorAvgMetric$lowColor),
                 textInput("highColorAvg", "Color for the highest values:", value = highColorAvgMetric$highColor),
                 textInput("outlineColorAvg", "Color for the outline of tiles:", value = outlineColorAvgMetric$outlineColor), 
                 tags$h4("Number Options"),
                 checkboxInput("showTextAvg", "Show text within each tile", value = showTextAvgMetric$showText),
                 conditionalPanel(
                   condition = "input.showTextAvg == true", # Condition to show the text input
                   textInput("textColorAvg", "Color of Text:", value = textColorAvgMetric$textColor),
                   numericInput("textSizeAvg", "Size of Text", value = textSizeAvgMetric$size))
        ),
        
        ##################
        # Legend options #
        ##################
        tabPanel("Legend",
                 checkboxInput("showLegAvg", "Show the Legend", value =  showLegendAvgMetric$val),
                 conditionalPanel(
                   condition = "input.showLegAvg == true", 
                   pickerInput("legPosAvg", "Legend Position: ", choices = c("Right", "Left", "Bottom", "Top"), selected = c(legendPositionAvgMetric$position)), 
                   tags$h4("Label Options"),
                   numericInput("labelTextLegAvgMetric", "Label Size: ", value = legendlabelsizeAvgMetric$size),
                   numericInput("labelticksLegAvgMetric", "Tick Label Size: ", value = legendTickSizeAvgMetric$size)
                 ), 
                 tags$h4("Numeric Options"),
                 pickerInput("logScaleAvgMetric", "Metrics to Show in Log Base 10:", c("MSE", "MAE", "WIS", "PI", "AIC", "AICc", "BIC"), selected = scaleYAvgMetric$logScale, multiple = T),
                 conditionalPanel(
                   condition = "input.showLegAvg == true", 
                   numericInput("numBreaksAvg", "Number of legend breaks: ", value = breaksAvgMetric$breaks)
                 )
        ) 
        
      ) # End of `tabsetPanel`
      
    )) # Show Modal
    
  }) # End of 'observeEvent'
    
  ###############################################
  # Update the reactive value - Scale of y-axis #
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
  
  ################################################
  # Update the reactive value - Breaks in legend #
  ################################################
  observeEvent(input$numBreaksAvg,{
    
    # Updating the number of breaks
    breaksAvgMetric$breaks <- input$numBreaksAvg
    
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
  
  #########################################
  # Update the reactive value - Text Size #
  #########################################
  observeEvent(input$textSizeAvg,{
    
    # Updating the text size
    textSizeAvgMetric$size <- input$textSizeAvg
    
  })
  
  #################################################
  # Update the reactive value - Legend Label Size #
  #################################################
  observeEvent(input$labelTextLegAvgMetric,{
    
    # Updating the legend label size 
    legendlabelsizeAvgMetric$size <- input$labelTextLegAvgMetric
    
  })
  
  ################################################
  # Update the reactive value - Legend Tick Size #
  ################################################
  observeEvent(input$labelticksLegAvgMetric,{
    
    # Updating the legend tick size 
    legendTickSizeAvgMetric$size <- input$labelticksLegAvgMetric
    
  })
  
  ##################################################
  # Update the reactive value - Showing the legend #
  ##################################################
  observeEvent(input$showLegAvg,{
    
    # Updating the indicator for showing the legend
    showLegendAvgMetric$val <- input$showLegAvg
    
  })
  
  ###############################################
  # Update the reactive value - Legend Position #
  ###############################################
  observeEvent(input$legPosAvg,{
    
    # Updating the legend position 
    legendPositionAvgMetric$position <- input$legPosAvg
    
  })
  
  ############################################
  # Update the reactive value - Y-Axis Label #
  ############################################
  observeEvent(input$labelOptionsAvgY,{
    
    # Updating the y-axis label
    yAxisLabelAvgMetric$text <- input$labelOptionsAvgY
    
  })
  
  #################################################
  # Update the reactive value - Y-Axis Label Face #
  #################################################
  observeEvent(input$labelYFaceAge,{
    
    # Updating the y-axis label face
    yAxisLabelFaceAvgMetric$face <- input$labelYFaceAge
    
  })
  
  #################################################
  # Update the reactive value - Y-Axis Label Size #
  #################################################
  observeEvent(input$labelYSizeAvg,{
    
    # Updating the y-axis label size
    yAxisLabelSizeAvgMetric$size <- input$labelYSizeAvg
    
  })
  
  ################################################
  # Update the reactive value - Y-Axis Tick Size #
  ################################################
  observeEvent(input$TickYSizeAvg,{
    
    # Updating the y-axis tick size
    yAxisTickSizeAvgMetric$size <- input$TickYSizeAvg
    
  })
  
  ############################################
  # Update the reactive value - x-Axis Label #
  ############################################
  observeEvent(input$labelOptionsAvgx,{
    
    # Updating the x-axis label
    xAxisLabelAvgMetric$text <- input$labelOptionsAvgx
    
  })
  
  #################################################
  # Update the reactive value - x-Axis Label Face #
  #################################################
  observeEvent(input$labelxFaceAge,{
    
    # Updating the x-axis label face
    xAxisLabelFaceAvgMetric$face <- input$labelxFaceAge
    
  })
  
  #################################################
  # Update the reactive value - x-Axis Label Size #
  #################################################
  observeEvent(input$labelxSizeAvg,{
    
    # Updating the x-axis label size
    xAxisLabelSizeAvgMetric$size <- input$labelxSizeAvg
    
  })
  
  ################################################
  # Update the reactive value - x-Axis Tick Size #
  ################################################
  observeEvent(input$TickxSizeAvg,{
    
    # Updating the x-axis tick size
    xAxisTickSizeAvgMetric$size <- input$TickxSizeAvg
    
  })

 
#------------------------------------------------------------------------------#
# Creating the tile plot for average metrics -----------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the user-specified options from above, and the     #
# average metrics calculated at an earlier step to return a list of panel      #
# figures to the main dashboard. Figures will be produced for each unique      #
# combination of metric and calibration period length.                         #
#------------------------------------------------------------------------------#

   ##############################################################################
   # Initialize `reactiveValues` to store the average model fit metrics figures #
   ##############################################################################
   modelAvgPlot <- reactiveValues()

   ############################################
   # Observing changes in the reactive values #
   ############################################
   observe({

     # Requiring the average metrics to run
     req(avgMetricsFiltered$metricsFULL)

     ##################################
     # Function to produce panel plot #
     ##################################
     if(nrow(avgMetricsFiltered$metricsFULL) > 0){

     ##################################
     # Function to produce panel plot #
     ##################################
     metricPanelAverage <- AverageMetricsPanel(avgMetrics.input = avgMetricsFiltered$metricsFULL,
                                               dateType.input = dateValues$dates,
                                               scaleY.input = input$logScaleAvgMetric,
                                               lowColor.input = lowColorAvgMetric$lowColor,
                                               highColor.input = highColorAvgMetric$highColor,
                                               outlineColor.input = outlineColorAvgMetric$outlineColor,
                                               legendBreaks.input = breaksAvgMetric$breaks,
                                               textColor.input = textColorAvgMetric$textColor,
                                               showText.input = showTextAvgMetric$showText,
                                               textSize.input = textSizeAvgMetric$size,
                                               legendLabelSize.input = legendlabelsizeAvgMetric$size,
                                               legendTickSize.input = legendTickSizeAvgMetric$size,
                                               showLegend.input = showLegendAvgMetric$val,
                                               legendPosition.input = legendPositionAvgMetric$position,
                                               yAxisLabel.input = yAxisLabelAvgMetric$text,
                                               yAxisLabelFace.input =  yAxisLabelFaceAvgMetric$face,
                                               yAxisLabelSize.input = yAxisLabelSizeAvgMetric$size,
                                               yAxisTickSize.input = yAxisTickSizeAvgMetric$size,
                                               xAxisLabel.input = xAxisLabelAvgMetric$text,
                                               xAxisLabelFace.input =  xAxisLabelFaceAvgMetric$face,
                                               xAxisLabelSize.input = xAxisLabelSizeAvgMetric$size,
                                               xAxisTickSize.input = xAxisTickSizeAvgMetric$size)


     ############################################
     # Adding the figures to the reactive value #
     ############################################
     modelAvgPlot$figures <- metricPanelAverage

     }

   }) # End of 'observe'


#------------------------------------------------------------------------------#
# Clearing out the average metrics figures -------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears out the above reactive values that contain the    #
# average metrics figures. It is triggered when the original file is changed,  #
# the clear button is hit, or there is no data for evaluation.                 #
#------------------------------------------------------------------------------#

  #########################################################
  # Observing event of file changing or button is cleared #
  #########################################################
  observeEvent(clearingOut(),{

    # Clearing the list of average figures
    modelAvgPlot$figures <- NULL

  })

  ############################################
  # Observing changes in the evaluation data #
  ############################################
  observe({

    # Running if the evaluation data is NULL
    if(is.null(dataForEvaluation$data)){

      # Clearing the list of crude figures
      modelAvgPlot$figures <- NULL

    } # End of 'if'

  }) # End of 'observe'


#------------------------------------------------------------------------------#
# Setting up the forward and backwards arrows for average figures --------------
#------------------------------------------------------------------------------#
# About: This section creates the forward and backwards buttons for navigating #
# through the average metrics figures. Additionally, it resets the figure list #
# back to one when any of the filtering options for the average metrics is     #
# changed.
#------------------------------------------------------------------------------#

  ###########################################################################
  # Creating the reactive value to be used with the average metrics buttons #
  ###########################################################################
  current_index_AMetrics <- reactiveVal(1)

  #################################################
  # Going backwards if the previous button is hit #
  #################################################
  observeEvent(input$PreviousAMetricFigure, {

    # Isolating the action to only when the button is clicked
    isolate({

      # Running if the current index is greater than one
      if(current_index_AMetrics() > 1){

        # Changing the index of the reactive value
        current_index_AMetrics(max(current_index_AMetrics() - 1))

      }

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement

  ############################################
  # Going forwards if the next button is hit #
  ############################################
  observeEvent(input$NextAMetricFigure, {

    # Isolating the action to only when the button is clicked
    isolate({

      # Run if the current index is less than the length of the list
      if (current_index_AMetrics() < length(modelAvgPlot$figures)) {

        # Changing the index of the reactive value
        current_index_AMetrics(min(current_index_AMetrics() + 1))

      }

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement

  ######################################################
  # Fixes the index when the data is filtered - Models #
  ######################################################
  observeEvent(input$modelAvgMetrics, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_AMetrics(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  #########################################################
  # Fixes the index when the data is filtered - Locations #
  #########################################################
  observeEvent(input$locationsAvgMetrics, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_AMetrics(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  #########################################################################
  # Fixes the index when the data is filtered - Calibration period length #
  #########################################################################
  observeEvent(input$calibrationAvgMetrics, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_AMetrics(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  ##########################################################
  # Fixes the index when the figure/data button is clicked #
  ##########################################################
  observeEvent(input$AvgFigure, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_AMetrics(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  ###########################################################
  # Fixes the index when the type of metric to show changes #
  ###########################################################
  observeEvent(input$metricsToShow, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_AMetrics(1)

    }) # End of isolate

  }) # End of 'observeEvent'


#------------------------------------------------------------------------------#
# Rendering the title of the average metrics figure ----------------------------
#------------------------------------------------------------------------------#
# About: This section renders the title of the average metrics figure to the   #
# main dashboard.                                                              #
#------------------------------------------------------------------------------#

  output$AvgMetricsFigureTitle <- renderText({

    # Rendering the data table
    return(names(modelAvgPlot$figures)[current_index_AMetrics()])

  }) # End of 'renderPlot'



#------------------------------------------------------------------------------#
# Rendering the average metrics figure -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the list of figures showing the average metrics  #
# for both model fit performance and forecasting performance.                  #
#------------------------------------------------------------------------------#

   output$AvgMetricsFigure <- renderPlot({

       # Rendering the data table
       return((modelAvgPlot$figures[[current_index_AMetrics()]]))

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
# About: This section downloads the list of average metrics figures as a       #
# '.zip' folder. The user can download the figures to anywhere on their        #
# system, and specify how the figures should be saved.                         #
#------------------------------------------------------------------------------#

  ##############################################
  # Creating the option to download the figure #
  ##############################################
  output$downloadAvgMetrics <- downloadHandler(

    #####################
    # File name for ZIP #
    #####################
    filename = function(){

      paste("Average-Metrics-Figures.zip", sep = "")

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
      for (plot_name in names(modelAvgPlot$figures)) {

        # Plot
        plot_obj <- modelAvgPlot$figures[[plot_name]]

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
# About: This section calculates Winkler scores for the produced ARIMA, GLM,   #
# GAM, Prophet models. The Winkler scores provide a quantitative measure of    #
# comparison for prediction interval coverage.                                 #
#------------------------------------------------------------------------------#

  ######################################
  # Creating the needed reactive value #
  ######################################

  # To store the final Winkler Scores data
  winklerFinalAGGP <- reactiveValues()

  # Indicator for filtering
  filterWinklerMainIndicator <- reactiveVal(0)

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

      # Running if forecast data is available
      if(all(!is.null(foremattedForecasts$forecasts))){

        # Requiring the forecasts
        req(foremattedForecasts$forecasts)
        
        # Requiring the calibration period
        req(input$calibrationPeriod)

        #######################
        # Creating the pop-up #
        #######################
        observeEvent(input$filterWinklerDataMain, ignoreInit = T,{

          # Setting the indicator
          isolate({filterWinklerMainIndicator(1)})

          # Button
          showModal(modalDialog(
            title = "Filtering Options",
            pickerInput("locationsWinklerMain", "Location(s):", c(input$locations), selected = c(input$locations), multiple = T), # Location input
            pickerInput("calibrationWinklerMain", "Calibration period length:", c(input$calibrationPeriod), selected = c(input$calibrationPeriod), multiple = T), # Calibration input
            pickerInput("modelsWinklerMain", "Model(s):", c(input$modelType), selected = c(input$modelType), multiple = T) # Model Input

            ))

        })

        ##################################
        # Calculating the Winkler Scores #
        ##################################
        winkler.scores.output <- winkler.scores.AGGP(formattedForecasts = foremattedForecasts$forecasts, # Forecast files
                                                     filterIndicator.input = filterWinklerMainIndicator(), # Filtering indicator
                                                     averageIndicator.input = input$seeAverageWinklerMain, # Average indicator
                                                     metricPage.input = input$metricsToShow, # Metric filtering
                                                     quantile.input = input$quantileSelection, # Quantile selector
                                                     orginData.input = dataForEvaluation$data, # Data for evaluation
                                                     date.type.input = dateValues$dates) # Type of date data

        ####################################################
        # Filtering the average metrics data: No Filtering #
        ####################################################
        if(filterWinklerMainIndicator() == 0){

          filter.Winkler <- winkler.scores.output

        #################################################
        # Filtering the average metrics data: Filtering #
        #################################################
        }else{

          filter.Winkler <- winkler.scores.output %>%
            dplyr::filter(Location %in% c(input$locationsWinklerMain),
                          Model %in% c(input$modelsWinklerMain),
                          Calibration %in% c(input$calibrationWinklerMain))

        }
        
        #####################################################
        # Saving the final data frame in the reactive value #
        #####################################################
        winklerFinalAGGP$scores <- filter.Winkler
        
      }

  }) # End of 'observe'


#------------------------------------------------------------------------------#
# Clearing the Winkler Scores Data ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the Winkler Scores data and resets the filtering  #
# indicator when a new data set is read into the dashboard, the clear button   #
# is clicked, or the evaluation data is changed.                               #
#------------------------------------------------------------------------------#

  #########################################################
  # Observing event of file changing or button is cleared #
  #########################################################
  observeEvent(clearingOut(),{

    # Clearing the list of Winkler scores
    winklerFinalAGGP$scores <- NULL

    # Resetting the filtering indicator
    filterWinklerMainIndicator(0)

  })

  ############################################
  # Observing changes in the evaluation data #
  ############################################
  observe({

    # Running if the evaluation data is NULL
    if(is.null(dataForEvaluation$data)){

      # Clearing the list of Winkler scores
      winklerFinalAGGP$scores <- NULL

      # Resetting the filtering indicator
      filterWinklerMainIndicator(0)

    } # End of 'if'

  }) # End of 'observe'


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
# Calculating and Filtering the Skill Scores -----------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the skill scores for all possible combos      #
# of models, and then filters the skill scores based on the selected models,   #
# locations, and calibration period lengths.                                   #
#------------------------------------------------------------------------------#

  ######################################
  # Creating the needed reactive value #
  ######################################

  # To store the filtered Skill Scores data
  finalSSCombinedMAIN <- reactiveValues()

  # Indicator for which metrics to show
  filterSSIndicatorMAIN <- reactiveVal(0)

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    # Requiring the crude metrics list
    req(modelMetricsCrude$metricsList)

    ###################################################
    # Determining the options for the baseline models #
    ###################################################

    # Data used for calculations
    crudeMetrics <- modelMetricsCrude$metricsList

    # Determining the possible model choices
    modelChoices <- c(unique(crudeMetrics$Model))
    
    # Requiring the calibration period
    req(input$calibrationPeriod)

    #######################
    # Creating the pop-up #
    #######################
    observeEvent(input$filterSSDataMain, ignoreInit = T,{

      # Setting the indicator
      isolate({filterSSIndicatorMAIN(1)})

      # Button
      showModal(modalDialog(
        title = "Filtering Options",
        pickerInput("baselineModelsMAIN", "Baseline Model(s):", c(modelChoices), selected = c(modelChoices), multiple = T), # Model filtering - Baseline
        pickerInput("compareModels2MAIN", "Comparison Model(s):", c(modelChoices), selected = c(modelChoices), multiple = T), # Model filtering - Comparison
        pickerInput("locationInputSelectMAIN", "Location:", c(input$locations), selected = c(input$locations), multiple = T), # Location
        pickerInput("calibrationSSMain", "Calibration period length:", c(input$calibrationPeriod), selected = c(input$calibrationPeriod), multiple = T) # Calibration input

      ))

    })

    
    ################################
    # Calculating the Skill Scores #
    ################################
    if(!is.null(foremattedForecasts$forecasts) & nrow(modelMetricsCrude$metricsList) > 0){
      
      ####################################
      # Recalculating the Winkler Scores #
      ####################################
      winklerScores1 <- winkler.scores.AGGP(formattedForecasts = foremattedForecasts$forecasts, # Forecast files
                                            filterIndicator.input = 0, # Filtering indicator
                                            averageIndicator.input = F, # Average indicator
                                            metricPage.input = input$metricsToShow, # Metric filtering
                                            quantile.input = input$quantileSelection, # Selected quantile
                                            orginData.input = dataForEvaluation$data, # Data used for base
                                            date.type.input = dateValues$dates) # Type of date data
      
      ############################
      # Calculating skill scores #
      ############################
      skillScores <- skillScoresMain(averageIndicator = input$seeAvgSS, # Indicator to use average metrics
                                     CrudeMetrics = modelMetricsCrude$metricsList, # Crude metrics
                                     winkler.input = winklerScores1, # Winkler score
                                     metricPage.input = input$metricsToShow) # Metric type filtering


    ###########################################################
    # Determining if the scores should be filtered: Filtering #
    ###########################################################
    if(filterSSIndicatorMAIN() == 1){

      filterd <- skillScores %>%
        dplyr::filter(Location %in% c(input$locationInputSelectMAIN),
                      `Baseline Model` %in% c(input$baselineModelsMAIN),
                      `Comparison Model` %in% c(input$compareModels2MAIN),
                      Calibration %in% c(input$calibrationSSMain))

    ##############################################################
    # Determining if the scores should be filtered: No Filtering #
    ##############################################################
    }else{

      filterd <- skillScores

    }

    #########################################
    # Saving the output to a reactive value #
    #########################################
    finalSSCombinedMAIN$scores <-  filterd

    ##################
    # Returning NULL #
    ##################
    }else{

      finalSSCombinedMAIN$scores <-  NULL

    }

  })

#------------------------------------------------------------------------------#
# Clearing the Skill Scores Data -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the Skill Scores data and resets the filtering    #
# indicator when a new data set is read into the dashboard, the clear button   #
# is clicked, or the evaluation data is changed.                               #
#------------------------------------------------------------------------------#

  #########################################################
  # Observing event of file changing or button is cleared #
  #########################################################
  observeEvent(clearingOut(),{

    # Clearing the list of skill scores
    finalSSCombinedMAIN$scores <- NULL

    # Resetting the filtering indicator
    filterSSIndicatorMAIN(0)

  })

  ############################################
  # Observing changes in the evaluation data #
  ############################################
  observe({

    # Running if the evaluation data is NULL
    if(is.null(dataForEvaluation$data)){

      # Clearing the list of skill scores
      finalSSCombinedMAIN$scores <- NULL

      # Resetting the filtering indicator
      filterSSIndicatorMAIN(0)

    } # End of 'if'

  }) # End of 'observe'




#------------------------------------------------------------------------------#
# Rendering the skill scores data ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the skill scores the main dashboard.             #
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
       
       ###########################
       # Creating the ShinyAlert #
       ###########################
       shinyalert(
         
         # Welcome message
         title = "Instructions",
         
         # Enabling html rendering
         html = TRUE,
         
         # Size of modal
         size = "m", 
         
         # Creating the text message
         text = HTML(
           
           "Welcome to the Model Comparison page. This page allows you to
                   create forecast figures and panels and compare performance metrics
                   for models not included in the dashboard to the forecasts conducted
                   within the dashboard.
                   
                   <br><br>
                    
                   To use this feature, please ensure your data follows the correct
                    setup. Instructions for the proper data format can be found on the
                   `About` tab of the dashboard.,
                  
                   <br><br>
                    
                   All options MUST be selected and forecasts run on the primary dashboard
                   page before comparing models and reading in outside forecasts.",
           
         ),
         
         type = "info"
         
       )
       
     } # End of 'else'
     
   }) # End of 'observeEvent'
                  


#------------------------------------------------------------------------------#
# Reading in multiple files ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the multiple "outside" models for manipulation  #
# in the model comparison page. Unlike other file input options used in this   #
# dashboard, this option allows for multiple files to be selected.             #
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

    }) # End of 'tryCatch'

    }) # End of 'observeEvent'


#------------------------------------------------------------------------------#
# Resetting the reactive value with files --------------------------------------
#------------------------------------------------------------------------------#
# About: This section resets the reactive value storing the "outside" model    #
# forecast files when the original file is changed, or the clear button on the #
# main page is clicked.                                                        #
#------------------------------------------------------------------------------#

  ##########################################################
  # Observing the change in original file and clear button #
  ##########################################################
  observeEvent(clearingOut(), {

     # Reset the reactive value to NULL when file() changes
     listOtherForecasts$forecastData <- NULL

   })

#------------------------------------------------------------------------------#
# Checking the forecast files against the original data and for name -----------
#------------------------------------------------------------------------------#
# About: This section checks the outside forecast files against the original   #
# data to ensure that the location names match, the order of the columns are   #
# correct, the naming scheme of the data, and the date type of the data.       #
#------------------------------------------------------------------------------#

  ##################################
  # Reactive value to save results #
  ##################################
  vettedData <- reactiveValues()
  
  ############################
  # Reactive value for error #
  ############################
  errorHolder <- reactiveValues()

  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({

    # Requiring the original data
    req(file())

    # Requiring the other forecast files
    req(listOtherForecasts$forecastData)

    ###########################
    # Error checking function #
    ###########################
    errorReturnOtherForecast <- errorReturn(orignalData.input = file(),
                                            otherForecast.input = listOtherForecasts$forecastData,
                                            dateType.input = dateValues$dates,
                                            horizon.input = input$forecastHorizon)
    
    # Saving the response
    errorHolder$error <- errorReturnOtherForecast
    
    
#------------------------------------------------------------------------------#
# Checking for the original file data and results ------------------------------
#------------------------------------------------------------------------------#
# About: This section checks to see if the original data has been read in, and #
# the dashboard has been run prior to executing the remainder of the code.     #
#------------------------------------------------------------------------------#

    ##########################
    # Checking for forecasts #
    ##########################
    if(is.null(foremattedForecasts$forecasts) & input$my_picker == "Model Comparison" & length(listOtherForecasts$forecastData) > 0){
      
      ######################
      # Returning an error #
      ######################
      shinyalert(title = "Warning",
                 "Please run the main dashboard prior to loading forecast
                 files from outside models.", type = "warning")
      
      # Clearing out the outside file reactive value
      listOtherForecasts$forecastData <- NULL
      
      # Clearing out the reactive value
      vettedData$data <- NULL
      
      # Resetting the error indicator 
      errorHolder$error <- NULL
      
    }else{

      #########################################
      # Producing errors if on the right page #
      #########################################
      if(input$my_picker == "Model Comparison"){
  
        ########################################
        # Producing Error Code 1: Naming Issue #
        ########################################
        if(errorHolder$error == "ERROR1"){
  
          # Returning the error
          shinyalert(title = "Error", type = "error", text = "Please check the naming scheme of your data.")
  
          # Clearing the list with final plots
          finalFiguresOther$figures <- NULL
          
          # Clearing the list with individual plots
          individualOtherPlots$figures <- NULL
          
          # Clearing the list with panel plots
          PanelOtherPlots$figures <- NULL
          
          # Clearing vetted data
          vettedData$data <- NULL
          
          # Resetting the index
          current_index_otherModels(1)
          
        #####################################
        # Producing Error Code 2: Locations #
        #####################################
        }else if(errorHolder$error == "ERROR2"){
  
          # Returning the error
          shinyalert(title = "Error", type = "error", text = "Please check your file names. The location listed does not match any location loaded with the orignal data.")
  
          # Clearing the list with final plots
          finalFiguresOther$figures <- NULL
          
          # Clearing the list with individual plots
          individualOtherPlots$figures <- NULL
          
          # Clearing the list with panel plots
          PanelOtherPlots$figures <- NULL
          
          # Clearing vetted data
          vettedData$data <- NULL
          
          # Resetting the index
          current_index_otherModels(1)
          
        #################################
        # Producing Error Code 3: Dates #
        #################################
        }else if(errorHolder$error == "ERROR3"){
  
          # Returning the error
          shinyalert(title = "Error", type = "error", text = "The temporal sequence (daily, weekly, yearly, index) included in the forecast files does not match the orignal data. Please check your data and file name.")
  
          # Clearing the list with final plots
          finalFiguresOther$figures <- NULL
          
          # Clearing the list with individual plots
          individualOtherPlots$figures <- NULL
          
          # Clearing the list with panel plots
          PanelOtherPlots$figures <- NULL
          
          # Clearing vetted data
          vettedData$data <- NULL
          
          # Resetting the index
          current_index_otherModels(1)
          
        ###################################
        # Producing Error Code 4: Columns #
        ###################################
        }else if(errorHolder$error == "ERROR4"){
  
          # Returning the error
          shinyalert(title = "Error", type = "error", text = "Please check the names of the columns within loaded data. The should be in the following order: Date, data, median, LB, UB")
  
          # Clearing the list with final plots
          finalFiguresOther$figures <- NULL
          
          # Clearing the list with individual plots
          individualOtherPlots$figures <- NULL
          
          # Clearing the list with panel plots
          PanelOtherPlots$figures <- NULL
          
          # Clearing vetted data
          vettedData$data <- NULL
          
          # Resetting the index
          current_index_otherModels(1)
          
        ####################################
        # Producing Error Code 5: Horizons #
        ####################################
        }else if(errorHolder$error == "ERROR5"){
  
          # Returning the error
          shinyalert(title = "Error", type = "error", text = "Please check the forecasting horizon specified in the loaded data. It should match that indicated in the main dashboard side-bar.")
  
          # Clearing the list with final plots
          finalFiguresOther$figures <- NULL
          
          # Clearing the list with individual plots
          individualOtherPlots$figures <- NULL
          
          # Clearing the list with panel plots
          PanelOtherPlots$figures <- NULL
          
          # Clearing vetted data
          vettedData$data <- NULL
          
          # Resetting the index
          current_index_otherModels(1)
          
        #####################################
        # Returning NULL if no error occurs #
        #####################################
        }else if(errorReturnOtherForecast == "WORKED"){
  
          vettedData$data <- listOtherForecasts$forecastData
  
        }
        
      }
      
    } # End of other errors
    

  }) # End of 'observe'


#------------------------------------------------------------------------------#
# Creating the forecast figure edit button -------------------------------------
#------------------------------------------------------------------------------#
# This section creates the UI for the action button which triggers the edit    #
# figures button for both the individual and panel figures.                    #
#------------------------------------------------------------------------------#

  #########################
  # Button for individual #
  #########################
  output$otherFigureOptions <- renderUI({

    return(actionButton("editLegendLabelsOther", "Figure Options"))

  })

  
#------------------------------------------------------------------------------#
# Creating the "Edit Figures" pop-up -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up containing all of the figure editing  #
# options. Each of the customization selected by the user applies both to the  #
# individual figures, and to the panel figures.                                #
#------------------------------------------------------------------------------#

  ##########################
  # Line type - Median fit #
  ##########################
  lineTypeOPanel <- reactiveValues(type = "Solid")

  ###########################
  # Line color - Median fit #
  ###########################
  lineColorOPanel <- reactiveValues(color = "Red")

  ###########################
  # Line width - Median fit #
  ###########################
  lineWidthOPanel <- reactiveValues(width = 0.9)

  ######################
  # Line type - Bounds #
  ######################
  lineTypeBoundsOPanel <- reactiveValues(type = "Dashed")

  #######################
  # Line color - Bounds #
  #######################
  lineColorBoundsOPanel <- reactiveValues(color = "Black")

  #######################
  # Line Width - Bounds #
  #######################
  lineWidthBoundsOPanel <- reactiveValues(width = 0.65)

  #########################
  # Ribbon color - Bounds #
  #########################
  ribbonColorOPanel <- reactiveValues(color = "Light Grey")

  ###############################
  # Reactive value for dot size #
  ###############################
  dotSizeReactiveOPanel <- reactiveValues(sizeVal = 2)

  #############################
  # Dot color - Observed data #
  #############################
  dotColorReactiveOPanel <- reactiveValues(color = "Black")

  #######################################
  # Reactive value for the y-axis label #
  #######################################
  yAxisLabOPanel <- reactiveValues(lab = "Count")

  ########################################
  # Reactive value for Y-Axis Label Size #
  ########################################
  yAxisLabSizeOPanel <- reactiveValues(size = 10)

  ########################################
  # Reactive value for Y-Axis label Face #
  ########################################
  yAxisLabFaceOPanel <- reactiveValues(face = "Plain")

  #######################################
  # Reactive value for y-axis tick size #
  #######################################
  yAxisTickLabelSizeOPanel <- reactiveValues(size = 10)

  #############################################
  # Reactive value for starting at the y-axis #
  #############################################
  startYReactiveOPanel <- reactiveValues(check = "0")

  #####################################################
  # Reactive value for the number of breaks in y-axis #
  #####################################################
  yAxisBreaksOPanel <- reactiveValues()

  observe({

    # For the individual plots
    if(input$showPanelPlotOther == F){

      yAxisBreaksOPanel$value <- 11

    # For the panel plots
    }else{

      yAxisBreaksOPanel$value <- 5

    }

  })

  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYOPanel <- reactiveValues(logScale = "Original")

  #######################################
  # Reactive value for the x-axis label #
  #######################################
  xAxisLabelOPanel <- reactiveValues(title = "")

  ############################################
  # Reactive value for the x-axis label size #
  ############################################
  xAxisLabelSizeOPanel <- reactiveValues(size = 10)

  ########################################
  # Reactive value for X-Axis label Face #
  ########################################
  xAxisLabFaceOPanel <- reactiveValues(face = "Plain")

  #######################################
  # Reactive value for X-Axis tick size #
  #######################################
  xAxisTickSizeO <- reactiveValues(size = 10)

  ################################################
  # Reactive value for the number of date breaks #
  ################################################
  dateBreaksReactiveOPanel <- reactiveValues(breaksDate = "2")

  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editLegendLabelsOther, ignoreInit = T, {

    showModal(modalDialog(

      # Title
      title = "Figure Options",

      # Creating the tabbed menu
      tabsetPanel(

        ################
        # Y-Axis Table #
        ################
        tabPanel("Y-Axis",
                 tags$h4("Primary Label Options"), # Labels title
                 textInput("yaxisLabelOPanel", "Label for Y-Axis:", value = yAxisLabOPanel$lab), # Label for y-axis
                 numericInput("yaxisLabelSizeOPanel", "Label Text Size:", value = yAxisLabSizeOPanel$size), # Size for y-axis label
                 pickerInput("yaxisLabelFaceOPanel", label = "Font Face", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = yAxisLabFaceOPanel$face), # Face of main y-axis label
                 tags$h4("Axis Options"),
                 numericInput("yAxisTickSizeOPanel", "Tick Label Size:", value = yAxisTickLabelSizeOPanel$size), # Tick label size
                 pickerInput("zeroStartOPanel", "Y-Axis Origin:", c("0", "Minimum value in data"), selected = startYReactiveOPanel$check), # Starting of the y-axis
                 numericInput("yAxisBreaksOPanel", "Y-Axis Breaks:", value = yAxisBreaksOPanel$value), # Y-Axis breaks
                 pickerInput("logScaleOPanel", "Scale for Y-Axis:", c("Original", "Log(Base 10)"), selected = scaleYOPanel$logScale) # Scale of y-axis

        ),


        ################
        # X-Axis Table #
        ################
        tabPanel("X-Axis",
                 tags$h4("Primary Label Options"), # Label title
                 textInput("xAxisOLabelPanel", label = "Label for X-Axis", value = xAxisLabelOPanel$title),
                 numericInput("xAxisLabelOSize", label = "Label Text Size", value = xAxisLabelSizeOPanel$size),
                 pickerInput("xaxisLabelFaceOPanel", label = "Font Face", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = xAxisLabFaceOPanel$face), # Face of main x-axis label
                 tags$h4("Axis Options"),
                 textInput("dateBreaksTSOPanel", "Number of Date Breaks (Label):", value = dateBreaksReactiveOPanel$breaksDate), # Number of date breaks
                 numericInput("xAxisTickOPanel", "Tick Label Size:", value = xAxisTickSizeO$size) #Tick size

        ),

        ############
        # Line tab #
        ############
        tabPanel("Lines",
                 h4("Median Line"), # Median Title
                 pickerInput("lineTypeForecastOPanel", label = "Line Type:", choices = c("Solid", "Dashed", "Dotted", "Dotdash", "Longdash", "Twodash"), selected = c(lineTypeOPanel$type)), # Line type
                 textInput("lineColorForecastOPanel", label = "Line Color:", value = lineColorOPanel$color), # Line color
                 numericInput("lineWidthForecastOPanel", label = "Line Width:", value = lineWidthOPanel$width), # Line width
                 h4("PI Bounds"), # Bounds title
                 pickerInput("lineTypeBoundOPanel", label = "Line Type:", choices = c("Solid", "Dashed", "Dotted", "Dotdash", "Longdash", "Twodash"), selected = c(lineTypeBoundsOPanel$type)), # Line type
                 textInput("lineColorBoundOPanel", label = "Line Color:", value = lineColorBoundsOPanel$color), # Line color
                 numericInput("lineWidthBoundsOPanel", label = "Line Width:", value = lineWidthBoundsOPanel$width), # Line width
                 textInput("RibbonColorBoundOPanel", label = "Ribbon Color:", value = ribbonColorOPanel$color) # Ribbon color

        ),

        #############
        # Point tab #
        #############
        tabPanel("Points",
                 h4("Observed Data"),
                 numericInput("dotSizeOPanel", "Dot Size:", value = dotSizeReactiveOPanel$sizeVal, step = 0.01), # Data dot size option
                 textInput("dotColorOPanel", "Dot Color", value = dotColorReactiveOPanel$color) # Dot color
        )

      ))) # End of creating the button

  }) # End of 'observeEvent'


  ############################################
  # Update the reactive value - Y-Axis Label #
  ############################################
  observeEvent(input$yaxisLabelOPanel, {

    # Updating the y-axis label
    yAxisLabOPanel$lab <- input$yaxisLabelOPanel

  })

  ###################################################
  # Updating the reactive value - Y-Axis Label Size #
  ###################################################
  observeEvent(input$yaxisLabelSizeOPanel, {

    # Updating the y-axis label size
    yAxisLabSizeOPanel$size <- input$yaxisLabelSizeOPanel

  })

  ###################################################
  # Updating the reactive value - Y-Axis Label Face #
  ###################################################
  observeEvent(input$yaxisLabelFaceOPanel, {

    # Updating the y-axis label face
    yAxisLabFaceOPanel$face <- input$yaxisLabelFaceOPanel

  })

  ##################################################
  # Updating the reactive value - Y-Axis Tick Size #
  ##################################################
  observeEvent(input$yAxisTickSizeOPanel, {

    # Updating the y-axis tick size
    yAxisTickLabelSizeOPanel$size <- input$yAxisTickSizeOPanel

  })

  ###############################################
  # Updating the reactive value - Y-Axis Breaks #
  ###############################################
  observeEvent(input$yAxisBreaksOPanel, {

    # Updating the number of y-axis breaks
    yAxisBreaksOPanel$value <- input$yAxisBreaksOPanel

  })

  ##############################################
  # Updating the reactive value - X-Axis Label #
  ##############################################
  observeEvent(input$xAxisOLabelPanel, {

    # Updating the x-axis label
    xAxisLabelOPanel$title <- input$xAxisOLabelPanel

  })

  ###################################################
  # Updating the reactive value - X-Axis Label Size #
  ###################################################
  observeEvent(input$xAxisLabelOSize, {

    # Updating the x-axis label size
    xAxisLabelSizeOPanel$size <- input$xAxisLabelOSize

  })

  ###################################################
  # Updating the reactive value - X-Axis Label Face #
  ###################################################
  observeEvent(input$xaxisLabelFaceOPanel, {

    # Updating the x-axis label face
    xAxisLabFaceOPanel$face <- input$xaxisLabelFaceOPanel

  })

  ##################################################
  # Updating the reactive value - X-Axis tick size #
  ##################################################
  observeEvent(input$xAxisTickOPanel, {

    # Updating the x-axis tick label size
    xAxisTickSizeO$size <- input$xAxisTickOPanel

  })

  ##################################################
  # Updating the reactive value - Median Line Type #
  ##################################################
  observeEvent(input$lineTypeForecastOPanel, {

    # Updating the median line type
    lineTypeOPanel$type <- input$lineTypeForecastOPanel

  })

  ###################################################
  # Updating the reactive value - Median Line Color #
  ###################################################
  observeEvent(input$lineColorForecastOPanel, {

    # Updating the median line color
    lineColorOPanel$color <- input$lineColorForecastOPanel

  })

  ###################################################
  # Updating the reactive value - Median Line Width #
  ###################################################
  observeEvent(input$lineWidthForecastOPanel, {

    # Updating the median line width
    lineWidthOPanel$width <- input$lineWidthForecastOPanel

  })

  #################################################
  # Updating the reactive value - Bound Line Type #
  #################################################
  observeEvent(input$lineTypeBoundOPanel,{

    # Updating the bound line type
    lineTypeBoundsOPanel$type <- input$lineTypeBoundOPanel

  })

  ##################################################
  # Updating the reactive value - Bound Line Color #
  ##################################################
  observeEvent(input$lineColorBoundOPanel, {

    # Updating the bound line color
    lineColorBoundsOPanel$color <- input$lineColorBoundOPanel

  })

  ##################################################
  # Updating the reactive value - Bound Line Width #
  ##################################################
  observeEvent(input$lineWidthBoundsOPanel,{

    # Updating the bound line width
    lineWidthBoundsOPanel$width <- input$lineWidthBoundsOPanel

  })

  ##############################################
  # Updating the reactive value - Ribbon color #
  ##############################################
  observeEvent(input$RibbonColorBoundOPanel,{

    # Updating the ribbon color
    ribbonColorOPanel$color <- input$RibbonColorBoundOPanel

  })

  #############################################
  # Update the reactive value - data dot size #
  #############################################
  observeEvent(input$dotSizeOPanel,{

    # Updating the data point size
    dotSizeReactiveOPanel$sizeVal <- input$dotSizeOPanel

  })

  ##############################################
  # Update the reactive value - Data dot color #
  ##############################################
  observeEvent(input$dotColorOPanel, {

    # Update the data point color
    dotColorReactiveOPanel$color <- input$dotColorOPanel

  })


  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleOPanel, {

    # Updating the scale
    scaleYOPanel$logScale <- input$logScaleOPanel

  })

  #####################################################
  # Update the reactive value - Number of date breaks #
  #####################################################
  observeEvent(input$dateBreaksTSOPanel,{

    # Updating the number of date breaks
    dateBreaksReactiveOPanel$breaksDate <- input$dateBreaksTSOPanel

  })

  ###############################################
  # Update the reactive value - Start at Y axis #
  ###############################################
  observeEvent(input$zeroStartOPanel,{

    # Updating the start point for the y-axis
    startYReactiveOPanel$check <- input$zeroStartOPanel

  })

#------------------------------------------------------------------------------#
# Producing list of forecast figures -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the list of formatted forecasts read in above and  #
# produces a list of forecast figures. The output of this section is a list of #
# ggplot figures.                                                              #
#------------------------------------------------------------------------------#

  #############################################################
  # Initialize `reactiveValues` to store the forecast figures #
  #############################################################
  individualOtherPlots <- reactiveValues()

  #######################################################
  # List of events to trigger the re-running of figures #
  #######################################################
  eventsTriggerIOFig <- reactive({

    # List of events
    events <- list(scaleYOPanel$logScale, vettedData$data,
                   yAxisLabOPanel$lab, dateBreaksReactiveOPanel$breaksDate,
                   startYReactiveOPanel$check, dotSizeReactiveOPanel$sizeVal,
                   lineTypeOPanel$type, lineColorOPanel$color, lineWidthOPanel$width,
                   dotColorReactiveOPanel$color, lineTypeBoundsOPanel$type,
                   lineWidthBoundsOPanel$width, lineColorBoundsOPanel$color,
                   ribbonColorOPanel$color, yAxisLabSizeOPanel$size,
                   yAxisLabFaceOPanel$face, yAxisTickLabelSizeOPanel$size,
                   yAxisBreaksOPanel$value, xAxisLabelOPanel$title,
                   xAxisLabelSizeOPanel$size, xAxisLabFaceOPanel$face,
                   xAxisTickSizeO$size)

    # Returning the list
    return(events)

  })

  #####################################################
  # Observing changes in inputs/other reactive values #
  #####################################################
  observeEvent(eventsTriggerIOFig(), {

    # Requiring formatted forecasts to run
    req(foremattedForecasts$forecasts)

    req(vettedData$data)

    # Checking for NULL
    if(!is.null(vettedData$data)){

      ###############################################
      # Function to create list of forecast figures #
      ###############################################
      isolate({

        individual <- forecast.figures.other(formatted.forecast.input = vettedData$data, # Formatted figures list
                                             data.type.input = dateValues$dates, # Date input
                                             smoothing.input = input$smoothingInput, # Smoothing input
                                             scaleYAxis.input = scaleYOPanel$logScale, # Scale y-axis
                                             yAxisLabel.input = yAxisLabOPanel$lab, # Y-axis label
                                             dateBreaks.input = dateBreaksReactiveOPanel$breaksDate, # Date breaks
                                             startYPoint.input = startYReactiveOPanel$check, # Y-axis start point
                                             dotSize.input = dotSizeReactiveOPanel$sizeVal, # Dot size
                                             linetype.input = lineTypeOPanel$type, # Median line type
                                             lineColor.input = lineColorOPanel$color, # Median line color
                                             lineWidth.input = lineWidthOPanel$width, # Median line width
                                             dotColor.input = dotColorReactiveOPanel$color, # Dot color
                                             boundtype.input = lineTypeBoundsOPanel$type, # Bounds line type
                                             boundWidth.input = lineWidthBoundsOPanel$width, # Bound width
                                             boundColor.input = lineColorBoundsOPanel$color, # Bound color
                                             ribbonColor.input = ribbonColorOPanel$color, # Ribbon color
                                             yLabelSize.input = yAxisLabSizeOPanel$size, # Y-axis label size
                                             yLabelFace.input = yAxisLabFaceOPanel$face, # Y-Axis label face
                                             yTickSize.input = yAxisTickLabelSizeOPanel$size, # Y-Axis tick size
                                             yTickBreaks.input = yAxisBreaksOPanel$value, # Y-Axis breaks
                                             xAxisLabel.input = xAxisLabelOPanel$title, # X-Axis Label
                                             xAxisLabelSize.input = xAxisLabelSizeOPanel$size, # X-Axis label size
                                             xAxisLabelFace.input = xAxisLabFaceOPanel$face, # X-axis label face
                                             xAxisTickSize.input = xAxisTickSizeO$size) # X-Axis tick size

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
        shinyalert("Unable to produce the following figures due to the presence of infinity UBs: ",
                   paste(namesErrorIndFig, collapse = "\n"), type = "error")

      }else{

        NULL

      }

      #########################################################
      # Returning the list of lists if individual figures run #
      #########################################################

      # Determining which figures are not NA
      notNAIndFig <- individual[!is.na(individual)]

      # Saving the exported list to a reactive value
      individualOtherPlots$figures <- notNAIndFig

    }else{individualOtherPlots$figures <- NULL}

  }) # End of 'observe' statement



#------------------------------------------------------------------------------#
# Producing list of forecast figure panels -------------------------------------
#------------------------------------------------------------------------------#
# About: This section, like the one above, takes in the list of formatted      #
# forecast figures to create panel figures. Panel figures refers to groups of  #
# figures by location, forecast period, and calibration period length. Each    #
# panel has graphs for the models selected in the side panel.                  #
#------------------------------------------------------------------------------#

  ###################################################################
  # Initialize `reactiveValues` to store the forecast figure panels #
  ###################################################################
  PanelOtherPlots <- reactiveValues()

  #####################################################
  # Observing changes in inputs/other reactive values #
  #####################################################
  observeEvent(eventsTriggerIOFig(), {

    # Requiring formatted forecasts to run
    req(foremattedForecasts$forecasts)

    # Requiring formatted forecasts
    req(vettedData$data)

    # Requiring formatted forecasts to run
    if(!is.null(vettedData$data)){

      #########################
      # Panel figures Isolate #
      #########################
      isolate({

        #################
        # Panel figures #
        #################
        panelOutput <- panel.forecast.figures.other(formatted.forecast.input = foremattedForecasts$forecasts, # Formatted forecast files
                                                    formatted.forecast.other.input = vettedData$data, # Other formatted forecast files
                                                    data.type.input = dateValues$dates, # Date type
                                                    smoothing.input = input$smoothingInput, # Smoothing input
                                                    scaleYAxis.input = scaleYOPanel$logScale, # Scale y-axis
                                                    yAxisLabel.input = yAxisLabOPanel$lab, # Y-axis label
                                                    dateBreaks.input = dateBreaksReactiveOPanel$breaksDate, # Date breaks
                                                    startYPoint.input = startYReactiveOPanel$check, # Y-axis start point
                                                    dotSize.input = dotSizeReactiveOPanel$sizeVal, # Dot size
                                                    linetype.input = lineTypeOPanel$type, # Median line type
                                                    lineColor.input = lineColorOPanel$color, # Median line color
                                                    lineWidth.input = lineWidthOPanel$width, # Median line width
                                                    dotColor.input = dotColorReactiveOPanel$color, # Dot color
                                                    boundtype.input = lineTypeBoundsOPanel$type, # Bounds line type
                                                    boundWidth.input = lineWidthBoundsOPanel$width, # Bound width
                                                    boundColor.input = lineColorBoundsOPanel$color, # Bound color
                                                    ribbonColor.input = ribbonColorOPanel$color, # Ribbon color
                                                    yLabelSize.input = yAxisLabSizeOPanel$size, # Y-axis label size
                                                    yLabelFace.input = yAxisLabFaceOPanel$face, # Y-Axis label face
                                                    yTickSize.input = yAxisTickLabelSizeOPanel$size, # Y-Axis tick size
                                                    yTickBreaks.input = yAxisBreaksOPanel$value, # Y-Axis breaks
                                                    xAxisLabel.input = xAxisLabelOPanel$title, # X-Axis Label
                                                    xAxisLabelSize.input = xAxisLabelSizeOPanel$size, # X-Axis label size
                                                    xAxisLabelFace.input = xAxisLabFaceOPanel$face, # X-axis label face
                                                    xAxisTickSize.input = xAxisTickSizeO$size, # X-Axis tick size
                                                    quantile.input = input$quantileSelection) # Quantile input

        #Saving the exported list to a reactive value
        PanelOtherPlots$figures <- panelOutput

      }) # End of 'isolate' statement

    }else{PanelOtherPlots$figures <- NULL} # End of "if statement"

  }) # End of 'observeEvent'


#------------------------------------------------------------------------------#
# Determining the plots to show ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if the individual plots should be rendered to #
# the main dashboard, or the panel plots. The user selects this by clicking    #
# the panel indicator.                                                         #
#------------------------------------------------------------------------------#

  ###################################################
  # Creating the reactive value to store final list #
  ###################################################
  finalFiguresOther <- reactiveValues()

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    req(individualOtherPlots$figures)

    #################################
    # Saving the individual figures #
    #################################
    if(input$showPanelPlotOther == F){

      finalFiguresOther$figures <- individualOtherPlots$figures

    ############################
    # Saving the panel figures #
    ############################
    }else{

      finalFiguresOther$figures <- PanelOtherPlots$figures

    }

  })

#------------------------------------------------------------------------------#
# Clearing the other forecast figures ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the other forecast figures if the original        #
# data is changed, the clear button is hit, or the other model forecast files  #
# are changed.                                                                 #
#------------------------------------------------------------------------------#

  ####################################################################
  # Clearing the list if the clear button is clicked or data changed #
  ####################################################################
  observeEvent(clearingOut(), {

    # Clearing the list with final plots
    finalFiguresOther$figures <- NULL

    # Clearing the list with individual plots
    individualOtherPlots$figures <- NULL

    # Clearing the list with panel plots
    PanelOtherPlots$figures <- NULL

    # Clearing vetted data
    vettedData$data <- NULL

    # Resetting the index
    current_index_otherModels(1)

  })
  
  ####################################################################
  # Clearing the list if the clear button is clicked or data changed #
  ####################################################################
  observeEvent(input$run, {
    
    # Clearing the list with final plots
    finalFiguresOther$figures <- NULL
    
    # Clearing the list with individual plots
    individualOtherPlots$figures <- NULL
    
    # Clearing the list with panel plots
    PanelOtherPlots$figures <- NULL
    
    # Clearing vetted data
    vettedData$data <- NULL
    
    # Resetting the index
    current_index_otherModels(1)
    
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
      if (current_index_otherModels() < length(finalFiguresOther$figures)) {

        # Changing the index of the reactive value
        current_index_otherModels(min(current_index_otherModels() + 1))

      }

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement

  ################################################
  # Resetting if the panel indicator is selected #
  ################################################
  observeEvent(input$showPanelPlotOther, {

    # Isolating the action to only when the button is clicked
    isolate({

      current_index_otherModels(1)

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement



#------------------------------------------------------------------------------#
# Creating the title for other forecast forecasts ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the titles for the individual and panel figures  #
# shown on the 'Model Comparison' page. The title that shows depends on if the #
# panel figures are showing or the individual figures.                         #
#------------------------------------------------------------------------------#

  output$OtherForecastTitle <- renderText({

    # Requiring the forecast figures
    req(vettedData$data)

    ######################
    # Rendering no title #
    ######################
    if(is.null(finalFiguresOther$figures)){

      return(NULL)

    #######################
    # Rendering the title #
    #######################
    }else{

      return(names(finalFiguresOther$figures[current_index_otherModels()]))

    }

  }) # End of title rendering


#------------------------------------------------------------------------------#
# Rendering the other forecast figures -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders either the individual or panel figures with      #
# the other model forecasts. The figure that is rendered depends on if the     #
# indicator for the panels is selected or not.                                 #
#------------------------------------------------------------------------------#

  ########################
  # Rendering the figure #
  ########################
  observe({

    # Requiring the forecast figures
    req(vettedData$data)

    ########################
    # Plotting the figures #
    ########################
    output$otherModelFigure <- renderPlot(

      finalFiguresOther$figures[[current_index_otherModels()]]

    )

  }) # End of 'observe'
  

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
        downloadButton("downloadOtherFigsIndividual", "Download Forecast Figure(s)"),
        easyClose = TRUE

      ))

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement


#------------------------------------------------------------------------------#
# Downloading the figure(s) ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the figure specifications from the above menu pop-  #
# up to save the forecast figures or panels. If there is only one figure, the  #
# code will output 1 figure. If there are multiple figures, a '.zip' file with #
# the figures will be saved the user's directory.                              #
#------------------------------------------------------------------------------#

  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({

    #########################
    # Saving a single image #
    #########################
    if(length(finalFiguresOther$figures) == 1){

      ##############################################
      # Creating the option to download the figure #
      ##############################################
      output$downloadOtherFigsIndividual <- downloadHandler(

        ####################################
        # Function to create the file-name #
        ####################################
        filename = function() {

          # Closing the figure specification
          removeModal()

          # File name
          paste("Other-Forecast-Figure.", input$extFig, sep = "")

        },

        #############################
        # Function to save the file #
        #############################
        content = function(file) {

          # Static version of plot
          figure <- finalFiguresOther$figures[[1]]

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

    #######################
    # Saving a zip folder #
    #######################
    }else{

      output$downloadOtherFigsIndividual <- downloadHandler(

        #####################
        # File name for ZIP #
        #####################
        filename = function(){

          paste("Other-Forecast-Figures.zip", sep = "")

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
          for (plot_name in names(finalFiguresOther$figures)) {

            # Plot
            plot_obj <- finalFiguresOther$figures[[plot_name]]

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

    } # End of 'if-else' for type of saved object

  }) # End of 'observe'


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
# About: This section resets the reactive value storing the "outside" model    #
# metric files when the original file is changed, or the clear button on the #
# main page is clicked.                                                        #
#------------------------------------------------------------------------------#

  #########################################################
  # Observing the change in original file and clear button #
  ##########################################################
  observeEvent(clearingOut(), {

    # Reset the reactive value to NULL when file() changes
    metricsOtherReactive$metricData <- NULL

  })


#------------------------------------------------------------------------------#
# Checking the format and name of the other performance metrics ----------------
#------------------------------------------------------------------------------#
# About: This section checks the format of the performance metrics files read  #
# in by the user, and the naming scheme used. It will then return errors if    #
# needed.                                                                      #
#------------------------------------------------------------------------------#

  ##################################
  # Reactive value to save results #
  ##################################
  vettedMetrics <- reactiveValues()
  
  ##################################
  # Reactive values to save errors #
  ##################################
  errorMetricSave <- reactiveValues()

  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({

    # Requiring the original data
    req(file())

    # Requiring the metrics
    req(metricsOtherReactive$metricData)

    ###########################
    # Error checking function #
    ###########################
    errorReturnOtherMetrics <- errorReturnMetrics(orignaldata.input = file(),
                                                  otherMetrics.input = metricsOtherReactive$metricData,
                                                  horizon.input = input$forecastHorizon)
    
    # Saving the result
    errorMetricSave$error <- errorReturnOtherMetrics

    #########################################
    # Producing errors if on the right page #
    #########################################
    if(input$my_picker == "Model Comparison"){
      
      #############################
      # Checking for metric files #
      #############################
      if(all(is.null(foremattedForecasts$forecasts) & input$my_picker == "Model Comparison" & length(metricsOtherReactive$metricData) > 0)){
        
          ######################
          # Returning an error #
          ######################
          shinyalert(title = "Warning",
                     "Please run the main dashboard prior to loading metrics
                   files from outside models.", type = "warning")
          
          # Clearing out the outside file reactive value
          metricsOtherReactive$metricData <- NULL
          
          # Clearing the errors
          errorMetricSave$error <- NULL
          
        }else{
  
        ########################################
        # Producing Error Code 1: Naming Issue #
        ########################################
        if(errorMetricSave$error == "ERROR1"){
  
          # Returning the error
          shinyalert(title = "Error", type = "error", text = "Please check the naming scheme of your metrics data.")
  
          # Clearing out the reactive value
          vettedMetrics$data <- NULL
          
          # Clearing out the outside file reactive value
          metricsOtherReactive$metricData <- NULL
          
        ###################################
        # Producing Error Code 2: Horizon #
        ###################################
        }else if(errorMetricSave$error == "ERROR2"){
  
          # Returning the error
          shinyalert(title = "Error", type = "error", text = "Please check the forecasting horizon specified in the loaded metrics data. It should match that indicated in the main dashboard side-bar.")
  
          # Clearing out the reactive value
          vettedMetrics$data <- NULL
          
          # Clearing out the outside file reactive value
          metricsOtherReactive$metricData <- NULL
  
        ###################################
        # Producing Error Code 3: Columns #
        ###################################
        }else if(errorMetricSave$error == "ERROR3"){
  
          # Returning the error
          shinyalert(title = "Error", type = "error", text = "Please check the names of the columns within loaded metrics data. The should be in the following order: Location, Model, Date, and then the forecast metrics.")
  
          # Clearing out the reactive value
          vettedMetrics$data <- NULL
          
          # Clearing out the outside file reactive value
          metricsOtherReactive$metricData <- NULL
  
        #####################################
        # Producing Error Code 4: Locations #
        #####################################
        }else if(errorMetricSave$error == "ERROR4"){
  
          # Returning the error
          shinyalert(title = "Error", type = "error", text = "Please check your metric file locations. The location listed does not match any location loaded with the orignal data.")
  
          # Clearing out the reactive value
          vettedMetrics$data <- NULL
          
          # Clearing out the outside file reactive value
          metricsOtherReactive$metricData <- NULL
  
        #####################################
        # Returning NULL if no error occurs #
        #####################################
        }else if(errorMetricSave$error == "WORKED"){
  
          vettedMetrics$data <- metricsOtherReactive$metricData
  
        }
          
      }
      
    } # End of other errors 

  }) # End of 'observe'

#------------------------------------------------------------------------------#
# Forming the crude metrics data set -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the crude metrics data from the dashboard, and   #
# the crude metrics read in by the user. This section will only run if the     #
# outside metric files pass all checks.                                        #
#------------------------------------------------------------------------------#
  
  ##########################################
  # Reactive value for filtering indicator #
  ##########################################
  indicatorForFilterMetrics <- reactiveVal(0)
  
  ###################################################
  # Reactive value to store the filtered crude data #
  ###################################################
  finalCrudeCombined <- reactiveValues()
  
  #####################################################
  # Reactive value to store the unfiltered crude data #
  #####################################################
  finalCrudeUnfiltered <- reactiveValues()
  
  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({

    # Requiring the new metrics
    req(vettedMetrics$data)

    # Ensuring the read-in metrics is not NULL
    if(!is.null(vettedMetrics$data)){
      
      combinedMetrics <- combining.metrics(original.fit.input = modelFitMetricsList$fitMetrics,
                                           orignal.forecast.input = forecastMetricsListCrude$forecastMetrics,
                                           new.input = vettedMetrics$data)
      
      # Adding it to the unfiltered reactive value
      finalCrudeUnfiltered$metrics <- combinedMetrics
      
    }
    
#------------------------------------------------------------------------------#
# Creating the filtering pop-up ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options pop-up for the combined    #
# performance metrics. Filtering options include, type, location, model, and   #
# calibration period length.                                                   #
#------------------------------------------------------------------------------#
    
    #######################
    # Creating the pop-up #
    #######################
    observeEvent(input$filterCrudeCombinedMetrics, ignoreInit = T,{
      
      # Requiring the combined metrics
      req(combinedMetrics)
      
      # Changing the indicator to one (i.e., button has been clicked)
      isolate({indicatorForFilterMetrics(1)})
      
      # Isolating button click behavior 
      isolate({
        
        # Button 
        showModal(modalDialog(
          title = "Filtering Options",
          pickerInput("ModelCrudeData1", "Model:", c(unique(combinedMetrics$Model)), selected = c(unique(combinedMetrics$Model)), multiple = T), # Model filtering
          pickerInput("perfType1", "Performance Metric Type:", c(unique(combinedMetrics$Type)), selected = c(unique(combinedMetrics$Type)), multiple = T), # Metric Type
          pickerInput("locationChoicesCrude1", "Location:", c(unique(combinedMetrics$Location)), selected = c(unique(combinedMetrics$Location)), multiple = T), # Location
          pickerInput("calibrationChoices1", "Calibration:", c(unique(combinedMetrics$Calibration)), selected = c(unique(combinedMetrics$Calibration)), multiple = T) # Calibration 
          
        ))
        
      })
      
    }) # End of 'observeEvent'

#------------------------------------------------------------------------------#
# Filtering the crude metrics data ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the crude metrics data based upon the user       #
# selected variables above. If no indicators are selected, the original data is#
# exported.                                                                    #
#------------------------------------------------------------------------------#
    
    ######################
    # Filtering the data #
    ######################
    if(indicatorForFilterMetrics() == 1){
      
      # Filtering the data 
      dataToExport <- combinedMetrics %>%
        dplyr::filter(Model %in% c(input$ModelCrudeData1), # Model filtering
                      Type %in% c(input$perfType1), # Metric type filtering
                      Location %in% c(input$locationChoicesCrude1), # Location filtering
                      Calibration %in% c(input$calibrationChoices1)) # Calibration filtering
                      
    ############################
    # No filtering to the data #
    ############################
    }else{
      
      # Not filtering the data 
      dataToExport <- combinedMetrics 
      
    }

    ############################################
    # Saving the results in the reactive value #
    ############################################
    finalCrudeCombined$data <- dataToExport
    
    
  }) # End of 'observe'
  
#------------------------------------------------------------------------------#
# Clearing the metrics ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the crude metrics list and resets the indicator   #
# for filtering when the original file is changed, the clear button is clicked #
# the original location is changed, or the model type is changed. Additionally,#
# it clears the results if the other metrics files are changed.                #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Clearing the crude metrics - `ClearingOut` #
  ##############################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value
    finalCrudeCombined$data <- NULL
    
    # Clearing the reactive value
    finalCrudeUnfiltered$metrics <- NULL
    
    # Clearing the filtering indicator
    indicatorForFilterMetrics(0)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
  })
  
  ############################################
  # Clearing the crude metrics - File change #
  ############################################
  observeEvent(input$metricsOther,{
    
    # Clearing the reactive value
    finalCrudeCombined$data <- NULL
    
    # Clearing the reactive value
    finalCrudeUnfiltered$metrics <- NULL
    
    # Clearing the filtering indicator
    indicatorForFilterMetrics(0)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
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
    
    # Returning the data frame with horizontal scrolling 
    return(datatable(finalCrudeCombined$data, options = list(scrollX = TRUE)))
    
  }) # End of 'renderDataTable'

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
      write.csv(finalCrudeCombined$data, file, row.names = FALSE)
      
    }
    
  ) # End of download button   

#------------------------------------------------------------------------------#
# Creating the options for the crude metric figure -----------------------------
#------------------------------------------------------------------------------#
# About: This section creates the figure customization options that show when  #
# the edit figure button is clicked.                                           #
#------------------------------------------------------------------------------#
  
  ####################################
  # Reactive value for the MSE label #
  ####################################
  MSELabelO <- reactiveValues(text = "MSE")
  
  ####################################
  # Reactive value for the MAE label #
  ####################################
  MAELabelO <- reactiveValues(text = "MAE")
  
  ####################################
  # Reactive value for the WIS label #
  ####################################
  WISLabelO <- reactiveValues(text = "WIS")
  
  ###################################
  # Reactive value for the PI label #
  ###################################
  PILabelO <- reactiveValues(text = "PI")
  
  #######################################
  # Reactive value for the Y-Label Size #
  #######################################
  yLabelSizeCMO <- reactiveValues(size = 12)
  
  #######################################
  # Reactive value for the Y-Label face #
  #######################################
  yLabelFaceCMO <- reactiveValues(face = "Plain")
  
  ##################################################
  # Reactive value for the Y-Axis breaks text size #
  ##################################################
  yTextSizeCMO <- reactiveValues(size = 12)
  
  ########################################
  # Reactive value for the Y-Axis breaks #
  ########################################
  yAxisBreaksCMO <- reactiveValues(breaks = 6)
  
  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYCrudeMetricO <- reactiveValues(logScale = NULL)
  
  #################################################
  # Reactive value for start point for the y-axis #
  #################################################
  startYCMO <- reactiveValues(start = "Zero")
  
  #####################################
  # Reactive value for common X-label #
  #####################################
  commonXLabelCMO <- reactiveValues(label = F)
  
  ###################################
  # Reactive value for X-Axis Label #
  ###################################
  xAxisLabelCMO <- reactiveValues(label = "")
  
  #######################################
  # Reactive value for the X-Label Size #
  #######################################
  xLabelSizeCMO <- reactiveValues(size = 12)
  
  #######################################
  # Reactive value for the X-Label face #
  #######################################
  xLabelFaceCMO <- reactiveValues(face = "Plain")
  
  ##################################################
  # Reactive value for the X-Axis breaks text size #
  ##################################################
  xTextSizeCMO <- reactiveValues(size = 12)
  
  ############################################
  # Reactive value for number of date breaks #
  ############################################
  xDateBreaksCMO <- reactiveValues(breaks = 1)
  
  #########################################
  # Reactive value for showing the legend #
  #########################################
  showLegendCMO <- reactiveValues(show = T)
  
  ######################################
  # Reactive value for legend location #
  ######################################
  legendLocationCMO <- reactiveValues(location = "Right")
  
  ##########################################
  # Reactive value for showing bar numbers #
  ##########################################
  showBarNumsCMO <- reactiveValues(show = T)
  
  ######################################
  # Reactive value for bar number size #
  ######################################
  barNumberSizeCMO <- reactiveValues(size = 6)
  
  ###############################
  # Reactive value for dot size #
  ###############################
  dotSizeCMO <- reactiveValues(size = 1.25)
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$figOptCombCrudeMetrics, {
    
    # Creating the pop-up
    showModal(modalDialog(
      
      # Title
      title = "Figure Options",
      
      # Creating the tabbed menu
      tabsetPanel(
        
        ##################
        # Y-Axis Options #
        ##################
        tabPanel("Y-Axis",
                 tags$h4("Primary Label Options"), # Labels title
                 tags$h5("Renaming the Labels"),
                 textInput("mseLabelCrudeMetricO", "Mean Squared Error: ", value = MSELabelO$text),
                 textInput("maeLabelCrudeMetricO", "Mean Absolute Error: ", value = MAELabelO$text),
                 textInput("wisLabelCrudeMetricO", "Weighted Interval Score: ", value = WISLabelO$text),
                 textInput("PILabelCrudeMetricO", "Prediction Interval: ", value = PILabelO$text),
                 tags$h5("Text Options"),
                 numericInput("yLabelTextSizeCMO", "Label Size: ", value = yLabelSizeCMO$size),
                 pickerInput("yLabalfaceCMO", "Label Face: ", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = yLabelFaceCMO$face),
                 tags$h4("Axis Options"),
                 numericInput("axisTextSizeCMO", "Tick label size: ", value = yTextSizeCMO$size),
                 numericInput("yAxisBreaksCMO", "Axis breaks: ", value =  yAxisBreaksCMO$breaks),
                 pickerInput("logScaleCrudeMetricO", "Metrics to Show in Log Base 10:", c("MSE", "MAE", "WIS", "PI"), selected = scaleYCrudeMetricO$logScale, multiple = T),
                 tags$h4("Time-series Specific"),
                 pickerInput("startYCrudeMetricO", "Start Value:", choices = c("Zero", "Minimum value in data."), selected = startYCMO$start, multiple = F)
                 
        ),
        
        ##################
        # X-Axis Options #
        ##################
        tabPanel("X-Axis",
                 tags$h4("Primary Label Options"), # Labels title
                 textInput("xAxisLabelCrudeO", label = "Label: ", value =  xAxisLabelCMO$label),
                 checkboxInput("commonXLabelCMO", "Common X-Axis Label ", value = commonXLabelCMO$label),
                 numericInput("xLabelTextSizeCMO", "Label Size: ", value = xLabelSizeCMO$size),
                 pickerInput("xLabelfaceCMO", "Label Face: ", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = xLabelFaceCMO$face),
                 tags$h4("Axis Options"),
                 numericInput("axisTextSizeXCMO", "Tick label size: ", value = xTextSizeCMO$size),
                 tags$h4("Time-series Specific"),
                 numericInput("xAxisBreaksCrudeO", "Date Breaks: ", xDateBreaksCMO$breaks)
                 
        ),
        
        ##################
        # Legend Options #
        ##################
        tabPanel("Legend and Shapes",
                 tags$h4("Legend Label Options"), # Labels title
                 checkboxInput("showLegendCrudeO", "Show the figure legend.", value = showLegendCMO$show),
                 pickerInput("legendPositionCMO", "Legend Location: ", choices = c("Right", "Left", "Top", "Bottom"), selected = c(legendLocationCMO$location), multiple = F),
                 tags$h4("Dot-chart Specific"),
                 numericInput("dotSizeCrudeO", "Dot Size:", value = dotSizeCMO$size), 
                 tags$h4("Bar-chart Specific"),
                 checkboxInput("barNumsCrudeO", "Show bar-chart numbers.", value = showBarNumsCMO$show),
                 numericInput("barNumsSizeCrudeO", "Bar-chart Number Size: ", value = barNumberSizeCMO$size)
                 
        )
        
      ) # End of 'tabsetPanel'
      
    )) # End of 'showModal'
    
  }) # End of 'observeEvent'
  
  
  #########################################
  # Update the reactive value - MSE label #
  #########################################
  observeEvent(input$mseLabelCrudeMetricO, {
    
    MSELabelO$text <- input$mseLabelCrudeMetricO
    
  })
  
  #########################################
  # Update the reactive value - MAE label #
  #########################################
  observeEvent(input$maeLabelCrudeMetricO, {
    
    MAELabelO$text <- input$maeLabelCrudeMetricO
    
  })
  
  #########################################
  # Update the reactive value - WIS label #
  #########################################
  observeEvent(input$wisLabelCrudeMetricO, {
    
    WISLabelO$text <- input$wisLabelCrudeMetricO
    
  })
  
  ########################################
  # Update the reactive value - PI label #
  ########################################
  observeEvent(input$PILabelCrudeMetricO, {
    
    PILabelO$text <- input$PILabelCrudeMetricO
    
  })
  
  ################################################
  # Update the reactive value - Y-Axis text size #
  ################################################
  observeEvent(input$yLabelTextSizeCMO, {
    
    yLabelSizeCMO$size <- input$yLabelTextSizeCMO
    
  })
  
  #################################################
  # Update the reactive value - Y-Axis label face #
  #################################################
  observeEvent(input$yLabalfaceCMO, {
    
    yLabelFaceCMO$face <- input$yLabalfaceCMO
    
  })
  
  ################################################
  # Update the reactive value - Y-Axis tick size #
  ################################################
  observeEvent(input$axisTextSizeCMO, {
    
    yTextSizeCMO$size <- input$axisTextSizeCMO
    
  })
  
  #############################################
  # Update the reactive value - Y-Axis breaks #
  #############################################
  observeEvent(input$yAxisBreaksCMO, {
    
    yAxisBreaksCMO$breaks <- input$yAxisBreaksCMO
    
  })
  
  ###############################################
  # Update the reactive value - scale of y-axis #
  ###############################################
  observeEvent(input$logScaleCrudeMetricO, {
    
    scaleYCrudeMetricO$logScale <- input$logScaleCrudeMetricO
    
  })
  
  ###############################################
  # Update the reactive value - start of y-axis #
  ###############################################
  observeEvent(input$startYCrudeMetricO, {
    
    startYCMO$start <- input$startYCrudeMetricO
    
  })
  
  ############################################
  # Update the reactive value - X-Axis Label #
  ############################################
  observeEvent(input$xAxisLabelCrudeO, {
    
    xAxisLabelCMO$label <- input$xAxisLabelCrudeO
    
  })
  
  ###################################################
  # Update the reactive value - Common X-Axis Label #
  ###################################################
  observeEvent(input$commonXLabelCMO, {
    
    commonXLabelCMO$label <- input$commonXLabelCMO
    
  })
  
  ################################################
  # Update the reactive value - X-Axis text size #
  ################################################
  observeEvent(input$xLabelTextSizeCMO, {
    
    xLabelSizeCMO$size <- input$xLabelTextSizeCMO
    
  })
  
  #################################################
  # Update the reactive value - X-Axis label face #
  #################################################
  observeEvent(input$xLabelfaceCMO, {
    
    xLabelFaceCMO$face <- input$xLabelfaceCMO
    
  })
  
  ################################################
  # Update the reactive value - X-Axis tick size #
  ################################################
  observeEvent(input$axisTextSizeXCMO, {
    
    xTextSizeCMO$size <- input$axisTextSizeXCMO
    
  })
  
  ##################################################
  # Update the reactive value - X-Axis date breaks #
  ##################################################
  observeEvent(input$xAxisBreaksCrudeO, {
    
    xDateBreaksCMO$breaks <- input$xAxisBreaksCrudeO
    
  })
  
  ##################################################
  # Update the reactive value - Show figure legend #
  ##################################################
  observeEvent(input$showLegendCrudeO, {
    
    showLegendCMO$show <- input$showLegendCrudeO
    
  })
  
  #################################################
  # Updating the reactive value - Legend Location #
  #################################################
  observeEvent(input$legendPositionCMO, {
    
    legendLocationCMO$location <- input$legendPositionCMO
    
  })
  
  ##################################################
  # Updating the reactive value - Show bar numbers #
  ##################################################
  observeEvent(input$barNumsCrudeO, {
    
    showBarNumsCMO$show <- input$barNumsCrudeO
    
  })
  
  ##################################################
  # Updating the reactive value - Bar numbers size #
  ##################################################
  observeEvent(input$barNumsSizeCrudeO, {
    
    barNumberSizeCM$sizeO <- input$barNumsSizeCrudeO
    
  })
  
  ##########################################
  # Updating the reactive value - dot size #
  ##########################################
  observeEvent(input$dotSizeCrudeO, {
    
    dotSizeCMO$size <- input$dotSizeCrudeO
    
  })

#------------------------------------------------------------------------------#
# Creating the crude metric figures --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the figures containing the crude metrics. There  #
# are two possible options, even a time-series or bar-chart. If there is one   #
# forecast date selected, a bar-chart is exported. If there are many dates     #
# selected, a time-series is exported. It also customizes the figures based    #
# on the user-selected options.                                                #
#------------------------------------------------------------------------------#
  
  ######################################################################
  # Initialize `reactiveValues` to store the model fit metrics figures #
  ######################################################################
  CrudeMetricsOtherPlots <- reactiveValues()
  
  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({
    
    # Requiring the crude metrics to run
    req(finalCrudeCombined$data)
    
    ##################################
    # Function to produce panel plot #
    ##################################
    if(nrow(finalCrudeCombined$data) > 0){
      
      metricPanelCrude <- CrudeMetricsFigure.OTHER(crudeMetrics = finalCrudeCombined$data,
                                                   dateType = dateValues$dates,
                                                   scaleY.input = input$logScaleCrudeMetricO,
                                                   MSELabel.input = MSELabelO$text,
                                                   MAELabel.input = MAELabelO$text,
                                                   WISLabel.input = WISLabelO$text,
                                                   PILabel.input = PILabelO$text,
                                                   YAxisLabelSize.input = yLabelSizeCMO$size,
                                                   YAxisLabelFace.input = yLabelFaceCMO$face,
                                                   YAxisTickSize.input = yTextSizeCMO$size,
                                                   YAxisBreaks.input = yAxisBreaksCMO$breaks,
                                                   YAxisStart.input = startYCMO$start,
                                                   xAxisLabel.input = xAxisLabelCMO$label,
                                                   xCommonLabel.input = commonXLabelCMO$label,
                                                   XAxisLabelSize.input = xLabelSizeCMO$size,
                                                   XAxisLabelFace.input = xLabelFaceCMO$face,
                                                   XAxisTickSize.input = xTextSizeCMO$size,
                                                   xAxisBreaks.input = xDateBreaksCMO$breaks,
                                                   showLegend.input = showLegendCMO$show,
                                                   legendPosition.input = legendLocationCMO$location,
                                                   showBarNum.input = showBarNumsCMO$show,
                                                   barNumSize.input = barNumberSizeCMO$size,
                                                   dotSize.input = dotSizeCMO$size)
      
      ############################################
      # Adding the figures to the reactive value #
      ############################################
      CrudeMetricsOtherPlots$figures <-  metricPanelCrude
      
    } # End of 'if'
    
  }) # End of 'observe'
  

#------------------------------------------------------------------------------#
# Setting up the forward and backwards arrows for crude figures ----------------
#------------------------------------------------------------------------------#
# About: This section creates the forward and backwards buttons for navigating #
# through the crude metrics figures. Additionally, it resets the figure list   #
# back to one when any of the filtering options for the crude metrics is       #
# changed.
#------------------------------------------------------------------------------#

  ###################################################################
  # Creating the reactive value to be used with the metrics buttons #
  ###################################################################
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
      if(current_index_crudeMetricPanel() < length(CrudeMetricsOtherPlots$figures)) {

        # Changing the index of the reactive value
        current_index_crudeMetricPanel(min(current_index_crudeMetricPanel() + 1))

      }

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement

  ######################################################
  # Fixes the index when the data is filtered - Models #
  ######################################################
  observeEvent(input$ModelCrudeData1, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_crudeMetricPanel(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  #########################################################
  # Fixes the index when the data is filtered - Locations #
  #########################################################
  observeEvent(input$locationChoicesCrude1, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_crudeMetricPanel(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  #########################################################################
  # Fixes the index when the data is filtered - Calibration period length #
  #########################################################################
  observeEvent(input$calibrationChoices1, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_crudeMetricPanel(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  ####################################################
  # Fixes the index when the data is filtered - Type #
  ####################################################
  observeEvent(input$perfType1, {
    
    # Isolate the behavior to when the button is clicked
    isolate({
      
      current_index_crudeMetricPanel(1)
      
    }) # End of isolate
    
  }) # End of 'observeEvent'
  
  
#------------------------------------------------------------------------------#
# Rendering the title for each of the figures ----------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the figures for the crude metrics. The title     #
# that is shown depends on the type of plot rendered: bar-chart or             #
# time-series.                                                                 #
#------------------------------------------------------------------------------#

  output$crudeMetricOtherPanelTitle <- renderText({

    # Rendering the title of the figure
    return(names(CrudeMetricsOtherPlots$figures)[current_index_crudeMetricPanel()])

  }) # End of 'renderPlot'


#------------------------------------------------------------------------------#
# Rendering each of the crude metrics figures ----------------------------------
#------------------------------------------------------------------------------#
# About: This section creates each of the figures shown in the main UI         #
# interface.                                                                   #
#------------------------------------------------------------------------------#

  output$crudeMetricOtherPanel <- renderPlot({

    # Rendering the data table
    return(CrudeMetricsOtherPlots$figures[[current_index_crudeMetricPanel()]])

  }) # End of 'renderPlot'
  
  
#------------------------------------------------------------------------------#
# Clearing the metrics ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the crude metrics list and resets the indicator   #
# for arrows when the original file is changed, the clear button is clicked    #
# the original location is changed, or the model type is changed. Additionally,#
# it clears the results if the other metrics files are changed.                #
#------------------------------------------------------------------------------#
  
  ##############################################
  # Clearing the crude metrics - `ClearingOut` #
  ##############################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value
    CrudeMetricsOtherPlots$figures <- NULL
    
    # Clearing the arrow indicator 
    current_index_crudeMetricPanel(1)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
    
  })
  
  ############################################
  # Clearing the crude metrics - File change #
  ############################################
  observeEvent(input$metricsOther,{
    
    # Clearing the reactive value
    CrudeMetricsOtherPlots$figures <- NULL
    
    # Clearing the arrow indicator 
    current_index_crudeMetricPanel(1)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
  })

#------------------------------------------------------------------------------#
# Creating the crude metrics figure download options ---------------------------
#------------------------------------------------------------------------------#
# About: This section creates the pop-up for the download button associated    #
# with the crude metrics figures box. It allows users to select the dpi,       #
# height, width, unit of measurement for size, and type of photo.              #
# Additionally, it allows users to download the figure with the user           #
# specifications.                                                              #
#------------------------------------------------------------------------------#

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
        downloadButton("downloadOtherCrudeMetrics", "Download Crude Metrics Figure(s)"),
        easyClose = TRUE

      ))

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement


#------------------------------------------------------------------------------#
# Downloading the figure(s) ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the figure specifications from the above menu pop-  #
# up to save the crude metrics figures. If there is only one figure, the       #
# code will output 1 figure. If there are multiple figures, a '.zip' file with #
# the figures will be saved the user's directory.                              #
#------------------------------------------------------------------------------#

  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({

    #########################
    # Saving a single image #
    #########################
    if(length(CrudeMetricsOtherPlots$figures) == 1){

      ##############################################
      # Creating the option to download the figure #
      ##############################################
      output$downloadOtherCrudeMetrics <- downloadHandler(

        ####################################
        # Function to create the file-name #
        ####################################
        filename = function() {

          # Closing the figure specification
          removeModal()

          # File name
          paste("Crude-Metrics-Figures.", input$extFig, sep = "")

        },

        #############################
        # Function to save the file #
        #############################
        content = function(file) {

          # Static version of plot
          figure <- CrudeMetricsOtherPlots$figures[[1]]

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

    #######################
    # Saving a zip folder #
    #######################
    }else{

      ##############################################
      # Creating the option to download the figure #
      ##############################################
      output$downloadOtherCrudeMetrics <- downloadHandler(

        #####################
        # File name for ZIP #
        #####################
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

    } # End of 'if-else' for download options

  }) # End of 'observe'
  
#------------------------------------------------------------------------------#
# Calculating the average metrics ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the average metrics based on the combined     #
# crude metrics from above. The average metrics can also be filtered and are   #
# later plotted.                                                               #
#------------------------------------------------------------------------------#

  ##########################################
  # Reactive value for filtering indicator #
  ##########################################
  indicatorForFilterMetricsAvg <- reactiveVal(0)
  
  ###################################################
  # Reactive value to store the filtered crude data #
  ###################################################
  finalAvgCombined <- reactiveValues()
  
  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({
    
    # Requiring the original metrics
    req(finalCrudeUnfiltered$metrics)
    
    # Ensuring the read-in metrics is not NULL
    if(!is.null(finalCrudeUnfiltered$metrics)){
      
      averageMetrics <- average.compare.metrics(metrics.input = finalCrudeUnfiltered$metrics)
      
    }

#------------------------------------------------------------------------------#
# Creating the filtering pop-up ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the filtering options pop-up for the average     #
# combined performance metrics. Filtering options include, type, location,     #
# model, and calibration period length.                                        #
#------------------------------------------------------------------------------#

    #######################
    # Creating the pop-up #
    #######################
    observeEvent(input$filterAvgCombinedMetrics, ignoreInit = T,{

      # Requiring the combined metrics
      req(averageMetrics)

      # Changing the indicator to one (i.e., button has been clicked)
      isolate({indicatorForFilterMetricsAvg(1)})

      # Isolating button click behavior
      isolate({

        # Button
        showModal(modalDialog(
          title = "Filtering Options",
          pickerInput("ModelCrudeDataA", "Model:", c(unique(averageMetrics$Model)), selected = c(unique(averageMetrics$Model)), multiple = T), # Model filtering
          pickerInput("perfTypeA", "Performance Metric Type:", c(unique(averageMetrics$Type)), selected = c(unique(averageMetrics$Type)), multiple = T), # Metric Type
          pickerInput("locationChoicesCrudeA", "Location:", c(unique(averageMetrics$Location)), selected = c(unique(averageMetrics$Location)), multiple = T), # Location
          pickerInput("calibrationChoicesA", "Calibration:", c(unique(averageMetrics$Calibration)), selected = c(unique(averageMetrics$Calibration)), multiple = T) # Calibration

        ))

      })

    }) # End of 'observeEvent'

#------------------------------------------------------------------------------#
# Filtering the average metrics data -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the average metrics data based upon the user     #
# selected variables above. If no indicators are selected, the original data is#
# exported.                                                                    #
#------------------------------------------------------------------------------#

    ######################
    # Filtering the data #
    ######################
    if(indicatorForFilterMetricsAvg() == 1){

      # Filtering the data
      dataToExport <- averageMetrics %>%
        dplyr::filter(Model %in% c(input$ModelCrudeDataA), # Model filtering
                      Type %in% c(input$perfTypeA), # Metric type filtering
                      Location %in% c(input$locationChoicesCrudeA), # Location filtering
                      Calibration %in% c(input$calibrationChoicesA)) # Calibration filtering

    ############################
    # No filtering to the data #
    ############################
    }else{

      # Not filtering the data
      dataToExport <- averageMetrics

    }

    ############################################
    # Saving the results in the reactive value #
    ############################################
    finalAvgCombined$metricsFULL <- dataToExport 
    
    
  }) # End of 'observe'

#------------------------------------------------------------------------------#
# Clearing the metrics ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the crude metrics list and resets the indicator   #
# for arrows when the original file is changed, the clear button is clicked    #
# the original location is changed, or the model type is changed. Additionally,#
# it clears the results if the other metrics files are changed.                #
#------------------------------------------------------------------------------#
  
  ################################################
  # Clearing the average metrics - `ClearingOut` #
  ################################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value
    finalAvgCombined$metricsFULL <- NULL
    
    # Clearing the filtering indicator 
    indicatorForFilterMetricsAvg(0)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
  })
  
  ##############################################
  # Clearing the average metrics - File change #
  ##############################################
  observeEvent(input$metricsOther,{
    
    # Clearing the reactive value
    finalAvgCombined$metricsFULL <- NULL
    
    # Clearing the filtering indicator 
    indicatorForFilterMetricsAvg(0)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
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
    
    # Returning the data frame with horizontal scrolling 
    return(datatable(finalAvgCombined$metricsFULL, options = list(scrollX = TRUE)))
    
  }) # End of 'renderDataTable'
  
  
#------------------------------------------------------------------------------#
# Downloading the combined metrics data as a '.csv' ----------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the combined crude metrics as a '.csv' file to the  #
# directory of their choosing.                                                 #
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
# Creating the button to edit the average metrics ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the buttons to edit the average metrics figures. #
# The edits selected within the pop-up will then apply to each of the metrics  #
# figures.                                                                     #
#------------------------------------------------------------------------------#
  
  ############################################
  # Reactive value for the y-axis scale type #
  ############################################
  scaleYAvgMetricA <- reactiveValues(logScale = NULL) 
  
  ########################################
  # Reactive value for the low-end color #
  ########################################
  lowColorAvgMetricA <- reactiveValues(lowColor = "Green")
  
  #########################################
  # Reactive value for the high-end color #
  #########################################
  highColorAvgMetricA <- reactiveValues(highColor = "Red")
  
  ########################################
  # Reactive value for the outline color #
  ########################################
  outlineColorAvgMetricA <- reactiveValues(outlineColor = "Black")
  
  #######################################
  # Reactive value for breaks in metric #
  #######################################
  breaksAvgMetricA <- reactiveValues(breaks = 5)
  
  ###################################
  # Reactive value for showing text #
  ###################################
  showTextAvgMetricA <- reactiveValues(showText = TRUE)
  
  #################################
  # Reactive value for text color #
  #################################
  textColorAvgMetricA <- reactiveValues(textColor = "Black")
  
  ################################
  # Reactive value for text size #
  ################################
  textSizeAvgMetricA <- reactiveValues(size = 6)
  
  ########################################
  # Reactive value for legend label size #
  ########################################
  legendlabelsizeAvgMetricA <- reactiveValues(size = 14)
  
  #######################################
  # Reactive value for legend tick size #
  #######################################
  legendTickSizeAvgMetricA <- reactiveValues(size = 10)
  
  #####################################
  # Reactive value to show the legend #
  #####################################
  showLegendAvgMetricA <- reactiveValues(val = T)
  
  ######################################
  # Reactive value for legend position #
  ######################################
  legendPositionAvgMetricA <- reactiveValues(position = "Right")
  
  ################
  # Y-Axis label #
  ################
  yAxisLabelAvgMetricA <- reactiveValues(text = "")
  
  #####################
  # Y-Axis label face #
  #####################
  yAxisLabelFaceAvgMetricA <- reactiveValues(face = "Plain")
  
  #####################
  # Y-Axis label size #
  #####################
  yAxisLabelSizeAvgMetricA <- reactiveValues(size = 12)
  
  ####################
  # Y-Axis tick size #
  ####################
  yAxisTickSizeAvgMetricA <- reactiveValues(size = 12)
  
  ################
  # x-Axis label #
  ################
  xAxisLabelAvgMetricA <- reactiveValues(text = "")
  
  #####################
  # x-Axis label face #
  #####################
  xAxisLabelFaceAvgMetricA <- reactiveValues(face = "Plain")
  
  #####################
  # x-Axis label size #
  #####################
  xAxisLabelSizeAvgMetricA <- reactiveValues(size = 12)
  
  ####################
  # x-Axis tick size #
  ####################
  xAxisTickSizeAvgMetricA <- reactiveValues(size = 12)
  
  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$figOptCombAvgMetrics, {
    
    # Creating the pop-up
    showModal(modalDialog(
      
      # Title 
      title = "Figure Options",
      
      tabsetPanel(
        
        ################
        # Y-Axis Label #
        ################
        tabPanel("Y-Axis",
                 tags$h4("Label Options"),
                 textInput("labelOptionsAvgYA", "Label: ", value = yAxisLabelAvgMetricA$text),
                 pickerInput("labelYFaceAgeA", "Face: ", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = c(yAxisLabelFaceAvgMetricA$face)),
                 numericInput("labelYSizeAvgA", "Size: ", value = yAxisLabelSizeAvgMetricA$size),
                 tags$h4("Axis Options"),
                 numericInput("TickYSizeAvgA", "Size: ", value = yAxisTickSizeAvgMetricA$size)
        ), 
        
        ################
        # X-Axis Label #
        ################
        tabPanel("X-Axis",
                 tags$h4("Label Options"),
                 textInput("labelOptionsAvgxA", "Label: ", value = xAxisLabelAvgMetricA$text),
                 pickerInput("labelxFaceAgeA", "Face: ", choices = c("Plain", "Bold", "Italic", "Bold & Italic"), selected = c(xAxisLabelFaceAvgMetricA$face)),
                 numericInput("labelxSizeAvgA", "Size: ", value = xAxisLabelSizeAvgMetricA$size),
                 tags$h4("Axis Options"),
                 numericInput("TickxSizeAvgA", "Size: ", value = xAxisTickSizeAvgMetricA$size)
        ), 
        
        ################
        # Fill options #
        ################
        tabPanel("Fill",
                 tags$h4("Color Options"), 
                 textInput("lowColorAvgA", "Color for the lowest values:", value = lowColorAvgMetricA$lowColor),
                 textInput("highColorAvgA", "Color for the highest values:", value = highColorAvgMetricA$highColor),
                 textInput("outlineColorAvgA", "Color for the outline of tiles:", value = outlineColorAvgMetricA$outlineColor), 
                 tags$h4("Number Options"),
                 checkboxInput("showTextAvgA", "Show text within each tile", value = showTextAvgMetricA$showText),
                 conditionalPanel(
                   condition = "input.showTextAvg == true", # Condition to show the text input
                   textInput("textColorAvgA", "Color of Text:", value = textColorAvgMetricA$textColor),
                   numericInput("textSizeAvgA", "Size of Text", value = textSizeAvgMetricA$size))
        ),
        
        ##################
        # Legend options #
        ##################
        tabPanel("Legend",
                 checkboxInput("showLegAvgA", "Show the Legend", value =  showLegendAvgMetricA$val),
                 conditionalPanel(
                   condition = "input.showLegAvg == true", 
                   pickerInput("legPosAvgA", "Legend Position: ", choices = c("Right", "Left", "Bottom", "Top"), selected = c(legendPositionAvgMetricA$position)), 
                   tags$h4("Label Options"),
                   numericInput("labelTextLegAvgMetricA", "Label Size: ", value = legendlabelsizeAvgMetricA$size),
                   numericInput("labelticksLegAvgMetricA", "Tick Label Size: ", value = legendTickSizeAvgMetricA$size)
                 ), 
                 tags$h4("Numeric Options"),
                 pickerInput("logScaleAvgMetricA", "Metrics to Show in Log Base 10:", c("MSE", "MAE", "WIS", "PI", "AIC", "AICc", "BIC"), selected = scaleYAvgMetricA$logScale, multiple = T),
                 conditionalPanel(
                   condition = "input.showLegAvg == true", 
                   numericInput("numBreaksAvgA", "Number of legend breaks: ", value = breaksAvgMetricA$breaks)
                 )
        ) 
        
      ) # End of `tabsetPanel`
      
    )) # Show Modal
    
  }) # End of 'observeEvent'
  
  ###############################################
  # Update the reactive value - Scale of y-axis #
  ###############################################
  observeEvent(input$logScaleAvgMetricA, {
    
    scaleYAvgMetricA$logScale <- input$logScaleAvgMetricA
    
  })
  
  ##########################################
  # Update the reactive value - Low colors #
  ##########################################
  observeEvent(input$lowColorAvgA,{
    
    # Updating the low color
    lowColorAvgMetricA$lowColor <- input$lowColorAvgA
    
  })
  
  ###########################################
  # Update the reactive value - High colors #
  ###########################################
  observeEvent(input$highColorAvgA,{
    
    # Updating the high color
    highColorAvgMetricA$highColor <- input$highColorAvgA
    
  })
  
  ##############################################
  # Update the reactive value - Outline colors #
  ##############################################
  observeEvent(input$outlineColorAvgA,{
    
    # Updating the outline color 
    outlineColorAvgMetricA$outlineColor <- input$outlineColorAvgA
    
  })
  
  ################################################
  # Update the reactive value - Breaks in legend #
  ################################################
  observeEvent(input$numBreaksAvgA,{
    
    # Updating the number of breaks
    breaksAvgMetricA$breaks <- input$numBreaksAvgA
    
  })
  
  #########################################
  # Update the reactive value - Show Text #
  #########################################
  observeEvent(input$showTextAvgA,{
    
    # Updating the text color
    showTextAvgMetricA$showText <- input$showTextAvgA
    
  })
  
  ###########################################
  # Update the reactive value - Text colors #
  ###########################################
  observeEvent(input$textColorAvgA,{
    
    # Updating the text color
    textColorAvgMetricA$textColor <- input$textColorAvgA
    
  })
  
  #########################################
  # Update the reactive value - Text Size #
  #########################################
  observeEvent(input$textSizeAvgA,{
    
    # Updating the text size
    textSizeAvgMetricA$size <- input$textSizeAvgA
    
  })
  
  #################################################
  # Update the reactive value - Legend Label Size #
  #################################################
  observeEvent(input$labelTextLegAvgMetricA,{
    
    # Updating the legend label size 
    legendlabelsizeAvgMetricA$size <- input$labelTextLegAvgMetricA
    
  })
  
  ################################################
  # Update the reactive value - Legend Tick Size #
  ################################################
  observeEvent(input$labelticksLegAvgMetricA,{
    
    # Updating the legend tick size 
    legendTickSizeAvgMetricA$size <- input$labelticksLegAvgMetricA
    
  })
  
  ##################################################
  # Update the reactive value - Showing the legend #
  ##################################################
  observeEvent(input$showLegAvgA,{
    
    # Updating the indicator for showing the legend
    showLegendAvgMetricA$val <- input$showLegAvgA
    
  })
  
  ###############################################
  # Update the reactive value - Legend Position #
  ###############################################
  observeEvent(input$legPosAvgA,{
    
    # Updating the legend position 
    legendPositionAvgMetricA$position <- input$legPosAvgA
    
  })
  
  ############################################
  # Update the reactive value - Y-Axis Label #
  ############################################
  observeEvent(input$labelOptionsAvgYA,{
    
    # Updating the y-axis label
    yAxisLabelAvgMetricA$text <- input$labelOptionsAvgYA
    
  })
  
  #################################################
  # Update the reactive value - Y-Axis Label Face #
  #################################################
  observeEvent(input$labelYFaceAgeA,{
    
    # Updating the y-axis label face
    yAxisLabelFaceAvgMetricA$face <- input$labelYFaceAgeA
    
  })
  
  #################################################
  # Update the reactive value - Y-Axis Label Size #
  #################################################
  observeEvent(input$labelYSizeAvgA,{
    
    # Updating the y-axis label size
    yAxisLabelSizeAvgMetricA$size <- input$labelYSizeAvgA
    
  })
  
  ################################################
  # Update the reactive value - Y-Axis Tick Size #
  ################################################
  observeEvent(input$TickYSizeAvgA,{
    
    # Updating the y-axis tick size
    yAxisTickSizeAvgMetricA$size <- input$TickYSizeAvgA
    
  })
  
  ############################################
  # Update the reactive value - x-Axis Label #
  ############################################
  observeEvent(input$labelOptionsAvgxA,{
    
    # Updating the x-axis label
    xAxisLabelAvgMetricA$text <- input$labelOptionsAvgxA
    
  })
  
  #################################################
  # Update the reactive value - x-Axis Label Face #
  #################################################
  observeEvent(input$labelxFaceAgeA,{
    
    # Updating the x-axis label face
    xAxisLabelFaceAvgMetricA$face <- input$labelxFaceAgeA
    
  })
  
  #################################################
  # Update the reactive value - x-Axis Label Size #
  #################################################
  observeEvent(input$labelxSizeAvgA,{
    
    # Updating the x-axis label size
    xAxisLabelSizeAvgMetricA$size <- input$labelxSizeAvgA
    
  })
  
  ################################################
  # Update the reactive value - x-Axis Tick Size #
  ################################################
  observeEvent(input$TickxSizeAvgA,{
    
    # Updating the x-axis tick size
    xAxisTickSizeAvgMetricA$size <- input$TickxSizeAvgA
    
  })
  
  
#------------------------------------------------------------------------------#
# Creating the tile plot for average metrics -----------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the user-specified options from above, and the     #
# average metrics calculated at an earlier step to return a list of panel      #
# figures to the main dashboard. Figures will be produced for each unique      #
# combination of metric, calibration period length, and type                   #
#------------------------------------------------------------------------------#
  
  ##############################################################################
  # Initialize `reactiveValues` to store the average model fit metrics figures #
  ##############################################################################
  AvgMetricsOtherPlots <- reactiveValues()
  
  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({
    
    # Requiring the average metrics to run
    req(finalAvgCombined$metricsFULL)
    
    ##################################
    # Function to produce panel plot #
    ##################################
    if(nrow(finalAvgCombined$metricsFULL) > 0){
      
      ##################################
      # Function to produce panel plot #
      ##################################
      metricPanelAverage <- AverageMetricsPanel.other(avgMetrics.input = finalAvgCombined$metricsFULL,
                                                      dateType.input = dateValues$dates,
                                                      scaleY.input = input$logScaleAvgMetricA,
                                                      lowColor.input = lowColorAvgMetricA$lowColor,
                                                      highColor.input = highColorAvgMetricA$highColor,
                                                      outlineColor.input = outlineColorAvgMetricA$outlineColor,
                                                      legendBreaks.input = breaksAvgMetricA$breaks,
                                                      textColor.input = textColorAvgMetricA$textColor,
                                                      showText.input = showTextAvgMetricA$showText,
                                                      textSize.input = textSizeAvgMetricA$size,
                                                      legendLabelSize.input = legendlabelsizeAvgMetricA$size,
                                                      legendTickSize.input = legendTickSizeAvgMetricA$size,
                                                      showLegend.input = showLegendAvgMetricA$val,
                                                      legendPosition.input = legendPositionAvgMetricA$position,
                                                      yAxisLabel.input = yAxisLabelAvgMetricA$text,
                                                      yAxisLabelFace.input =  yAxisLabelFaceAvgMetricA$face,
                                                      yAxisLabelSize.input = yAxisLabelSizeAvgMetricA$size,
                                                      yAxisTickSize.input = yAxisTickSizeAvgMetricA$size,
                                                      xAxisLabel.input = xAxisLabelAvgMetricA$text,
                                                      xAxisLabelFace.input =  xAxisLabelFaceAvgMetricA$face,
                                                      xAxisLabelSize.input = xAxisLabelSizeAvgMetricA$size,
                                                      xAxisTickSize.input = xAxisTickSizeAvgMetricA$size)
      
      
      ############################################
      # Adding the figures to the reactive value #
      ############################################
      AvgMetricsOtherPlots$figures <- metricPanelAverage
      
    }
    
  }) # End of 'observe'
  

#------------------------------------------------------------------------------#
# Clearing out the average metrics figures -------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears out the above reactive values that contain the    #
# average metrics figures. It is triggered when the original file is changed,  #
# the clear button is hit, or there is no data for evaluation.                 #
#------------------------------------------------------------------------------#

  ################################################
  # Clearing the average metrics - `ClearingOut` #
  ################################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value
    AvgMetricsOtherPlots$figures <- NULL
    
    # Clearing the arrow indicator 
    current_index_avgMetricPanel(1)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
  })
  
  ##############################################
  # Clearing the average metrics - File change #
  ##############################################
  observeEvent(input$metricsOther,{
    
    # Clearing the reactive value
    AvgMetricsOtherPlots$figures <- NULL
    
    # Clearing the arrow indicator 
    current_index_avgMetricPanel(1)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
  }) # End of 'observe'
  

#------------------------------------------------------------------------------#
# Setting up the forward and backwards arrows for average figures --------------
#------------------------------------------------------------------------------#
# About: This section creates the forward and backwards buttons for navigating #
# through the average metrics figures. Additionally, it resets the figure list #
# back to one when any of the filtering options for the average metrics is     #
# changed.
#------------------------------------------------------------------------------#

  ###########################################################################
  # Creating the reactive value to be used with the average metrics buttons #
  ###########################################################################
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
      if(current_index_avgMetricPanel() < length(AvgMetricsOtherPlots$figures)) {

        # Changing the index of the reactive value
        current_index_avgMetricPanel(min(current_index_avgMetricPanel() + 1))

      }

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement

  ######################################################
  # Fixes the index when the data is filtered - Models #
  ######################################################
  observeEvent(input$ModelCrudeDataA, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_avgMetricPanel(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  #########################################################
  # Fixes the index when the data is filtered - Locations #
  #########################################################
  observeEvent(input$locationChoicesCrudeA, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_avgMetricPanel(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  #########################################################################
  # Fixes the index when the data is filtered - Calibration period length #
  #########################################################################
  observeEvent(input$calibrationChoicesA, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_avgMetricPanel(1)

    }) # End of isolate

  }) # End of 'observeEvent'

  ###########################################################
  # Fixes the index when the type of metric to show changes #
  ###########################################################
  observeEvent(input$perfTypeA, {

    # Isolate the behavior to when the button is clicked
    isolate({

      current_index_avgMetricPanel(1)

    }) # End of isolate

  }) # End of 'observeEvent'


#------------------------------------------------------------------------------#
# Rendering the title of the average metrics figure ----------------------------
#------------------------------------------------------------------------------#
# About: This section renders the title of the average metrics figure to the   #
# main dashboard.                                                              #
#------------------------------------------------------------------------------#

  output$avgMetricOtherPanelTitle <- renderText({

    # Rendering the data table
    return(names(AvgMetricsOtherPlots$figures)[current_index_avgMetricPanel()])

  }) # End of 'renderPlot'

#------------------------------------------------------------------------------#
# Rendering the average metrics figure -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the list of figures showing the average metrics  #
# for both model fit performance and forecasting performance.                  #
#------------------------------------------------------------------------------#

  output$avgMetricOtherPanel <- renderPlot({

    # Rendering the data table
    return((AvgMetricsOtherPlots$figures[[current_index_avgMetricPanel()]]))

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
        downloadButton("downloadOtherAvgMetricsFig", "Download Average Metrics Figure"),
        easyClose = TRUE

      ))

    }) # End of 'isolate' statement

  }) # End of 'observeEvent' statement

#------------------------------------------------------------------------------#
# Downloading the figure -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section downloads the list of average metrics figures as a       #
# '.zip' folder. The user can download the figures to anywhere on their        #
# system, and specify how the figures should be saved.                         #
#------------------------------------------------------------------------------#

  ##############################################
  # Creating the option to download the figure #
  ##############################################
  output$downloadOtherAvgMetricsFig <- downloadHandler(

    #####################
    # File name for ZIP #
    #####################
    filename = function(){

      paste("Average-Metrics-Figures.zip", sep = "")

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
# About: This section calculates Winkler scores for the produced dashboard     #
# forecasts and the read-in files from the users. Winkler scores allow us to   #
# compare the performance of the prediction interval coverage.                 #
#------------------------------------------------------------------------------#
  
  ######################################
  # Creating the needed reactive value #
  ######################################

  # To store the final Winkler Scores data
  winklerScoresModelCompare <- reactiveValues()

  # Indicator for filtering
  filterWinklerCompareIndicator <- reactiveVal(0)

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    # Requiring the previous formatted forecasts
    req(foremattedForecasts$forecasts)

    # Requiring vetted formatted forecasts
    req(vettedData$data)

    # Running if forecast data is available
    if(all(!is.null(vettedData$data))){

      #######################
      # Creating the pop-up #
      #######################
      observeEvent(input$filterWinklerOtherMetrics, ignoreInit = T,{

        # Setting the indicator
        isolate({filterWinklerCompareIndicator(1)})
        
        # Button
        showModal(modalDialog(
          title = "Filtering Options",
          pickerInput("typeWinklerCompare", "Metric Type:", c(unique(winkler.scores.output$Type)), selected = c(unique(winkler.scores.output$Type)), multiple = T), # Type input
          pickerInput("locationWinklerCompare", "Location:", c(unique(winkler.scores.output$Location)), selected = c(unique(winkler.scores.output$Location)), multiple = T), # Location input
          pickerInput("ModelWinklerCompare", "Model:", c(unique(winkler.scores.output$Model)), selected = c(unique(winkler.scores.output$Model)), multiple = T), # Model input
          pickerInput("calibrationWinklerCompare", "Calibration Length:", c(unique(winkler.scores.output$Calibration)), selected = c(unique(winkler.scores.output$Calibration)), multiple = T) # Calibration input
        ))

      })

      #######################################
      # Running the Winkler scores function #
      #######################################
      winkler.scores.output <- Winkler.Scores.Model.Comparison(formatted.forecast.Other = vettedData$data, # Formatted forecast inputted
                                                               formatted.forecast.DASHBOARD = foremattedForecasts$forecasts, # Formatted forecast dashboard
                                                               date.type.input = dateValues$dates, # Date type
                                                               avgWinler.input = input$winklerOtherAvg, # Winkler Scores
                                                               quantile.input = input$quantileSelection) # Quantile selection
      ######################
      # Filtering the data #
      ######################
      if(filterWinklerCompareIndicator() == 1){
        
        finalWinkler <- winkler.scores.output %>%
          dplyr::filter(Type %in% c(input$typeWinklerCompare),
                        Location %in% c(input$locationWinklerCompare),
                        Model %in% c(input$ModelWinklerCompare),
                        Calibration %in% c(input$calibrationWinklerCompare))
                        
        
      ######################
      # Not-Filtering Data #
      ######################
      }else{
        
        finalWinkler <- winkler.scores.output 
        
      }
      
      #########################################
      # Saving the output to a reactive value #
      #########################################
      winklerScoresModelCompare$scores <- finalWinkler
      
    }

  }) # End of 'observe'


#------------------------------------------------------------------------------#
# Clearing out the Winkler Scores ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears out the above reactive values that contain the    #
# Winkler Scores. It is triggered when the original file is changed,           #
# the clear button is hit, or there is no data for evaluation.                 #
#------------------------------------------------------------------------------#
  
  ################################################
  # Clearing the Winkler metrics - `ClearingOut` #
  ################################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value
    winklerScoresModelCompare$scores <- NULL
    
    # Clearing the arrow indicator 
    filterWinklerCompareIndicator(0)
    
    # Clearing out the reactive value
    vettedData$data <- NULL
    
  })
  
  ##############################################
  # Clearing the Winkler metrics - File change #
  ##############################################
  observeEvent(input$dataset2,{
    
    # Clearing the reactive value
    winklerScoresModelCompare$scores <- NULL
    
    # Clearing the arrow indicator 
    filterWinklerCompareIndicator(0)
    
    # Clearing out the reactive value
    vettedData$data <- NULL
    
  }) # End of 'observe'
  

#------------------------------------------------------------------------------#
# Rendering the Winkler Scores data frame --------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes in the winkler scores calculated above and returns #
# them to the main UI as a data table.                                         #
#------------------------------------------------------------------------------#

  ###########################################
  # Rendering the Winkler Scores Data Frame #
  ###########################################
  output$winklerScoresOther <- renderDataTable({winklerScoresModelCompare$scores})


#------------------------------------------------------------------------------#
# Downloading the Winkler Scores -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows users to download the Winkler Scores data as a    #
# '.csv' file to the directory of their choosing.                              #
#------------------------------------------------------------------------------#

  ##############################################
  # Creating the option to download the figure #
  ##############################################
  output$downloadWinklerMetrics <- downloadHandler(

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
      write.csv(winklerScoresModelCompare$scores, file, row.names = FALSE)

    }

  ) # End of download button
  
#------------------------------------------------------------------------------#
# Clearing out the "vetted" data when new models are run -----------------------
#------------------------------------------------------------------------------#
# About: This section clears out the vetted data when new models are run in    #
# the main dashboard.                                                          #
#------------------------------------------------------------------------------#

  ##############################################
  # Clearing the average metrics - File change #
  ##############################################
  observeEvent(input$run,{
    
    # Clearing the reactive value
    vettedData$data <- NULL
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
  }) # End of 'observe'
  

#------------------------------------------------------------------------------#
# Calculating and Filtering the Skill Scores -----------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the skill scores for all possible combos      #
# of models, and then filters the skill scores based on the selected models,   #
# locations, and calibration period lengths.                                   #
#------------------------------------------------------------------------------#

  ######################################
  # Creating the needed reactive value #
  ######################################

  # To store the filtered Skill Scores data
  finalSSCombinedOther <- reactiveValues()

  # Indicator for which metrics to show
  filterSSIndicatorOther <- reactiveVal(0)

  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({

    req(finalCrudeUnfiltered$metrics)
    
    req(vettedMetrics$data)

    ###################################################
    # Determining the options for the baseline models #
    ###################################################
    
    # Data used for calculations
    crudeMetrics <- finalCrudeUnfiltered$metrics
    
    # Determining the possible model choices
    modelChoices <- c(unique(crudeMetrics$Model))
    
    #########################################
    # Determining the options for locations #
    #########################################
    
    # Determining the possible location choices
    locationChoices <- c(unique(crudeMetrics$Location))
    
    ###################################################
    # Determining the options for calibration periods #
    ###################################################
    
    # Determining the possible calibration period lengths 
    calibrationChoices <- c(unique(crudeMetrics$Calibration))

    #######################
    # Creating the pop-up #
    #######################
    observeEvent(input$filterSkillScoresOtherMetrics, ignoreInit = T,{

      # Setting the indicator
      isolate({filterSSIndicatorOther(1)})

      # Button
      showModal(modalDialog(
        title = "Filtering Options",
        pickerInput("baselineModelsOther", "Baseline Model(s):", c(modelChoices), selected = c(modelChoices), multiple = T), # Model filtering - Baseline
        pickerInput("compareModels2Other", "Comparison Model(s):", c(modelChoices), selected = c(modelChoices), multiple = T), # Model filtering - Comparison
        pickerInput("locationInputSelectOther", "Location:", c(locationChoices), selected = c(locationChoices), multiple = T), # Location
        pickerInput("calibrationSSOther", "Calibration period length:", c(calibrationChoices), selected = c(calibrationChoices), multiple = T), # Calibration input
        pickerInput("metricTypeSSOther", "Type: ", c("Fit", "Forecast"), selected = c("Fit", "Forecast"), multiple = T) # Metric Type 

      ))

    })

    ####################################
    # Recalculating the Winkler Scores #
    ####################################
    winklerScores <- Winkler.Scores.Model.Comparison(formatted.forecast.Other = vettedData$data, # Formatted forecast inputted
                                                     formatted.forecast.DASHBOARD = foremattedForecasts$forecasts, # Formatted forecast dashboard
                                                     date.type.input = dateValues$dates, # Date type
                                                     avgWinler.input = F, # Average Winkler Score
                                                     quantile.input = input$quantileSelection) # Selected quantile

    ################################
    # Calculating the Skill Scores #
    ################################
    if(nrow(finalCrudeUnfiltered$metrics) > 0){

       skillScores <- skillScoresOther(averageIndicator = input$seeAvgSSOther, # Indicator to use average metrics
                                       CrudeMetrics = finalCrudeUnfiltered$metrics, # Crude metrics
                                       winkler.input = winklerScores) # Winkler score

    ###########################################################
    # Determining if the scores should be filtered: Filtering #
    ###########################################################
    if(filterSSIndicatorOther() == 1){

        filterd <- skillScores %>%
          dplyr::filter(Location %in% c(input$locationInputSelectOther),
                        `Baseline Model` %in% c(input$baselineModelsOther),
                        `Comparison Model` %in% c(input$compareModels2Other),
                        Calibration %in% c(input$calibrationSSOther),
                        Type %in% c(input$metricTypeSSOther))

    ##############################################################
    # Determining if the scores should be filtered: No Filtering #
    ##############################################################
    }else{

        filterd <- skillScores

     }

    #########################################
    # Saving the output to a reactive value #
    #########################################
    finalSSCombinedOther$scores <-  filterd

    ##################
    # Returning NULL #
    ##################
    }else{

      finalSSCombinedOther$scores <-  NULL

    }

  })

#------------------------------------------------------------------------------#
# Clearing the Skill Scores Data -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section clears the Skill Scores data and resets the filtering    #
# indicator when a new data set is read into the dashboard, the clear button   #
# is clicked, or the evaluation data is changed.                               #
#------------------------------------------------------------------------------#

  #####################################################
  # Clearing the Skill Scores metrics - `ClearingOut` #
  #####################################################
  observeEvent(clearingOut(),{
    
    # Clearing the reactive value
    finalSSCombinedOther$scores <- NULL
    
    # Clearing the filtering indicator 
    filterSSIndicatorOther(0)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
  })
  
  ###################################################
  # Clearing the Skill Scores metrics - File change #
  ###################################################
  observeEvent(input$metricsOther,{
    
    # Clearing the reactive value
    finalSSCombinedOther$scores <- NULL
    
    # Clearing the filtering indicator 
    filterSSIndicatorOther(0)
    
    # Clearing out the reactive value
    vettedMetrics$data <- NULL
    
  }) # End of 'observe'
  

#------------------------------------------------------------------------------#
# Rendering the skill scores data ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the skill scores the main dashboard.             #
#------------------------------------------------------------------------------#

  ############################
  # Rendering the data frame #
  ############################
  output$skillScoresOtherOUTPUT <- renderDataTable({finalSSCombinedOther$scores})


#------------------------------------------------------------------------------#
# Downloading the skill scores as a '.csv' -------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides interactivity to download button. Therefore, it #
# allows users to download the skill scores data as a '.csv' file to the       #
# directory of their choosing.                                                 #
#------------------------------------------------------------------------------#

  output$downloadSSMetricsOther <- downloadHandler(
  
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
      write.csv(finalSSCombinedOther$scores, file, row.names = FALSE)
  
    }
  
  ) # End of download button



    
  }
shinyApp(ui = ui, server = server)
