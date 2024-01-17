#------------------------------------------------------------------------------#
#                                                                              #
#           Calculating model fit metrics for each individual forecast         #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file reads in the individual quantile forecasts and model fits produced #
# in an earlier step, the observed data, and the outputs the mean squared      #
# error (MSE), mean absolute error, 95% prediction interval coverage, and      #
# weighted interval scores (WIS). One file is outputted for each forecasting   #
# period, therefore, if the forecasting horizon is greater than one, the       #
# outputted value is average across forecasting horizons for a single          #
# forecasting period.                                                          #
#                                                                              #
# A detailed description of the calculations used for MSE, MAE, 95% PI         #
# coverage, and WIS can be found at:                                           #
#                                                                              #
# (1) Gneiting T, Raftery AE. Strictly proper scoring rules, prediction, and   #
#     estimation. Journal of the American statistical Association. 2007;102    #
#     (477):359-378. https://doi.org/10.1198/016214506000001437.               #
# (2) Kuhn M, Johnson K. Applied predictive modeling. Vol 26. Springer; 2013.  #
# (3) University of Nicosia. M4Competition Competitor’s Guide: Prizes and      # 
#     Rules. 2018. Accessed June 28, 2023. http://www.unic.ac.cy/test/wp-      #
#     content/uploads/sites/2/2018/09/M4-Competitors-Guide.pdf.                #
# (4) Bracher J, Ray EL, Gneiting T, Reich NG. Evaluating epidemic forecasts   #
#     in an interval format. PLoS computational biology. 2021;17(2):e1008618.  #
#     https://doi.org/10.1371/journal.pcbi.1008618.                            #
# (5) Cramer EY, Ray EL, Lopez VK, et al. Evaluation of individual and         #
#     ensemble probabilistic forecasts of COVID-19 mortality in the United     #
#     States. Proceedings of the National Academy of Sciences. 2022;119(15):e21#
#     13561119. https://doi.org/10.1073/pnas.2113561119.                       #
#                                                                              #
# Applications of the metrics in past works can be found at:                   #
#                                                                              #
# (1) Chowell G, Dahal S, Tariq A, Roosa K, Hyman JM, Luo R. An ensemble       #
#     n-sub-epidemic modeling framework for short-term forecasting epidemic    #
#     trajectories: Application to the COVID-19 pandemic in the USA. PLoS      #
#     Comput Biol. 2022;18: e1010602. doi:10.1371/journal.pcbi.1010602.        #
# (2) Bleichrodt A, Dahal S, Maloney K, Casanova L, Luo R, Chowell G.          #
#     Real-time forecasting the trajectory of monkeypox outbreaks at the       #
#     national and global levels, July-October 2022. BMC Med. 2023;21: 19.     #
#     doi:10.1186/s12916-022-02725-2.                                          #
#------------------------------------------------------------------------------#
# Authors: Amanda Bleichrodt and Gerardo Chowell                               #
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Below takes in the inputs into the function, and saves them under new #        
# variable names to prevent over-writing of values.                            #
#------------------------------------------------------------------------------#

#############################
# Reading in the crude data #
#############################
data.input.MF <- crude.data.input

######################################
# Saving the calibration period size #
######################################
calibration.input.MF <- calibration.input

#############
# Date type #
#############
date.Type.input.MF <- date.Type.input

##############################
# List of quantile forecasts #
##############################
quantile.forecast.input.MF <- quantile.list.input 

#------------------------------------------------------------------------------#
# Looping through the quantile forecast ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops through each quantile forecast, determines if the  #
# model fit statistics can be determined (i.e., it can not for ARIMA models),  #
# and subsets the observed data for later calculations.                        #
#------------------------------------------------------------------------------#
for(q in 1:length(quantile.forecast.input.MF)){
  
  #############################
  # Indexed quantile forecast #
  #############################
  indexedQuantile <- as.data.frame(quantile.forecast.input.MF[[q]])
  
  # Name of the indexed quantile
  indexedQuantileName <- names(quantile.forecast.input.MF[q])
  
  #######################################################
  # Pulling information from the quantile forecast name #
  #######################################################
  
  # Model name
  modelName <- strsplit(indexedQuantileName, "[-]")[[1]][1]
  
  # Location name
  location <- strsplit(indexedQuantileName, "[-]")[[1]][2]
  
  # Forecast period
  str_after_nth(strings, pattern, n)
  
}

#------------------------------------------------------------------------------#
# Reading in quantile forecast -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the inputs from above, and calls the appropriate   #
# quantile forecasts from the 'Forecast' folder in the user's working          #
# working directory. It then saves the file names of the quantile forecasts    #
# in a vector called 'quantile.forecasts'.                                     #
#------------------------------------------------------------------------------#

#############################################################
# Creating a label for the date-duration used in file names #
#############################################################
date.label <- base::switch(date.Type.input.M, # Input variable 
                           "week" = "weekly", # Weekly data
                           "day" = "daily", # Daily data
                           "year" = "yearly", # Yearly data
                           "index" = "time-index") # Time index data

###########################################################
# Creating a list of the file-names of quantile forecasts #
###########################################################
quantile.forecasts <- list.files(
  path = paste0("Forecasts/", model.type.input.M, "/Quantile Forecasts/", filename.crude.input.M), 
  pattern = paste0(model.type.input.M, "-Quantile-", process.input.M, "-", date.label, "-", data.type.input.M, ".+calibration-", calibration.index.input.M, "-horizon-", horizon.input.M, ".+.csv"),  # Pattern
  recursive = TRUE, # Indicates a pattern in files 
  full.names = T) # Calling full file names

#------------------------------------------------------------------------------#
# Looping through quantile forecasts -------------------------------------------
#-------------------------------------------------------------------------------
# About: The below code chunk creates the loop that goes through the file names#
# included within the quantile forecast folder of interest.                    #
#------------------------------------------------------------------------------#

##############################
# Looping through file-names #
##############################
for(i in 1:length(quantile.forecasts)){
  
#------------------------------------------------------------------------------#
# Determining the forecast data ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines what information from the quantile forecasts  #
# is considered the forecasts vs calibration period. This is only and issue    #
# when working with the GLM, GAM, and Prophet models, as the quantile          #
# predictions also exist for the model fit. This section also determines the   #
# dates of observed data needed in the following sections to evaluate the      #
# forecast performance.                                                        #
#------------------------------------------------------------------------------#
  
  ##################################################
  # Pulling the forecast period from the file-name #
  ##################################################
  forecast.period.date <- qdapRegex::ex_between(quantile.forecasts[i], paste0("horizon-", horizon.input.M, "-"), ".csv")[[1]]
  
  #####################################
  # Creating a vector of needed dates #
  #####################################
  
}





##########################################################################
# Cleaning up the file names to ensure only those of interest are called #
##########################################################################

# Pulling location names from the observed data file
locations <- colnames(crude.data.input.M[-1])




data <- read_csv("Forecasts/ARIMA/Quantile Forecasts/ARIMA-Quantile-mpox-weekly-cases-Brazil-calibration-11-horizon-4-2022-07-28.csv")

observedData <- crude.data %>%
  filter(Week %in% c("2022-08-04", "2022-08-11", "2022-08-18", "2022-08-25")) %>%
  select(Brazil)

true_value <- observedData$Brazil

data.reorder <- data %>%
  mutate(true_value = true_value) %>%
  select(true_value, means, `lower.95%`, `upper.95%`)



# Checking in forecasts can be evaluated ---------------------------------------





Metrics <- data.reorder %>%
  mutate(inCoverage = ifelse(true_value <= `upper.95%` & true_value >= `lower.95%`, 1, 0),
         PercentCoverage = (inCoverage/nrow(Metrics))*100,
         mean95PI = mean(PercentCoverage)) %>%
  select(true_value, means, mean95PI) %>%
  mutate(MSE = ((true_value-means)^2),
         MAE = (abs(true_value - means)), 
         meanMAE = mean(MAE), 
         meanMSE = mean(MSE)) %>%
  select(meanMSE, meanMAE, mean95PI) %>%
  distinct(.keep_all = T)



# WIS

data.WIS <- data 

alphas <- c(0.02, 0.05, seq(0.1, 0.9, by = 0.1))

w0=1/2;

sum1=0;

K=length(alphas);
WISF <- data.frame(row.names = NULL)


for(j in 1:4){

  
colx=12;

sum1=0;

y=true_value[j]
IS <- list()


for(k in 1:K){
  
alpha <- alphas[k]

#[alpha/2  1-alpha/2]

w_k=alpha/2;

Lt1 <- data.WIS[j,colx]
colnames(Lt1) <- "Lt1"
Ut1 <- data.WIS[j,colx+11]
colnames(Ut1) <- "Ut1"

Lt=Lt1$Lt1
Ut=Ut1$Ut1

IS[k]= c((Ut-Lt)+(2/alpha)*(Lt-y)*(y<Lt)+(2/alpha)*(y-Ut)*(y>Ut))

sum1=sum1+w_k*as.numeric(IS[k])

colx=colx-1

}

m = data.WIS[j, 1]
m <- m$means
         
WISF[j,1]=(1/(K+1/2))*(w0*abs(y-m) + sum1);
         
}

mean((WISF[,1]))