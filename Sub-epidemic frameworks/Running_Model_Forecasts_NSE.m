
% Once you have fit the model to data, the next step is to produce
% forecasts. The following code will allow you to produce forecasts for
% each of your columns of data, once information has been indicated FIRST
% in the 'options_forecast.m' and then in the following section. 

% Once you have made the desired selections, please hit the big, green, run
% button at the top of the page.

%------------------------------------------------------%
% Please only make changes to the following variables: %
%------------------------------------------------------%

% Below is where you indicate values for forecasting from you model fits
% you ran in an earlier step. This is the only section you will make
% changes in this code file. PLEASE completed the 'options_forecast.m' file
% prior to making changes in this file.

% Data columns: How many columns were in your input folder?
columns = 52;

% Forecast date: This is the same date entered in the 'options.m' 
% `caddate1` parameter. Please ensure that it is within single quotes. 
date = '05-11-2020';

% Forecasting horizon: What is the desired length of your forecast?
horizon = 4;

% Weight for ensembles: This is the same as you would indicate in the
% 'options_forecast.m' file for the `weight_type1` parameter.
weight = 1;

%------------------------------------------------------%
% Producing the forecasts: DO NOT TOUCH ANY CODE BELOW %
%------------------------------------------------------%

% Looping through columns 
for i = 1:columns

plotForecast_subepidemicFramework(i,date,horizon,weight)

end 