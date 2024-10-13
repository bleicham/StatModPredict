# StatModPredict: A User-Friendly R-Shiny Interface for Fitting and Forecasting with Statistical Models. 

**StatModPredict** is a user-friendly R-Shiny dashboard interface for fitting and forecasting time series data using user-specified variations of auto-regressive integrated moving average (ARIMA), generalized linear models (GLM), generalized additive models (GAM), and Facebook's Prophet model building around the existing auto.arima [1], glm [2] and glm.nb [3], gam [4], and prophet [5] functions in R. The dashboard eliminates the need for previous coding experience and facilitates real-time and retrospective forecasting efforts for various processes. At a minimum, the dashboard takes time-series data and returns model fits and forecasts, associated figures, and model fit and forecasting performance (when applicable). Additionally, the dashboard facilitates model comparison by allowing users to incorporate previously conducted forecasts and performance metrics for models not included in the dashboard into the interface. 

# Video Tutorial 
There is a video tutorial available on YouTube which employs the data included within the "Tutorial" folder above. The tutorial can be found at: https://www.youtube.com/watch?v=zgZOvqhvqw8

# Software Requirments 
- **Language:** R (\>=4.3)
- **Integrated development environment:** RStudio(\>=2024.04.1 Build 748)
- **Packages:** pacman & Shiny

# Preparing the data

## Primary data
The primary data refers to the data set used to fit all models, produce forecasts, and to evaluate the model fit and forecasts. The dashboard allows users to utilize any incident data, provided it follows the correct formatting guidelines and is a *.csv file. Users must structure their data in a "wide" format, where the first column corresponds to the time series dates (i.e., daily, weekly, yearly, or time index), and the remaining columns include the counts of the event of interest for each location or group at each time point. All columns must have headers without dashes in their name. However, there is no restriction on the file's name. 

When working with daily or weekly data, years must be formatted with four digits (YYYY); any conventional format can be used for the month and day. However, if working with yearly data, only a four-digit year can be used (YYYY); for time indexes, the first row of data corresponds to a time index of 1. Thus, records over time should go from the top to the bottom of the dataset. Examples of the input data structure can be found in the 'Example Data' folder above.

## "Outside Forecasts"
The file must include a '.csv' extension and use the following naming scheme: <Model Framework>-<Model>-horizon-<Horizon Number>-calibration-<Calibration Size>-<Location>-<Forecast Date>. Each entry in "<>" indicates file name elements specific to the forecast of interest. <Model Framework> refers to the overall framework or general model structure used (i.e., NSE), and <Model> refers to the specific model indicator or name (i.e., ranked(1)). No restrictions exist on the name used for <Model Framework> or <Model>. <Horizon Number> is the length of the forecasting horizon, and <Calibration Size> refers to the length of the data used to calibrate the model. Regarding <Location>, it must match one of the locations, including capitalization, used in the data employed throughout the rest of the dashboard. Finally, <Forecast Date> is the last data date used to calibrate the model. If working with yearly data, it needs to be a four-digit year ("YYYY").  If working with weekly or daily data, <Forecast Date> needs to be in "MM-DD-YYYY" format. Regarding the data format, the column names must match: Date, data, median, LB, UB. The dashboard will work with daily, weekly, yearly, or time index forecasts. Multiple outside forecasts can be read into the dashboard at a time. Examples can be found in the "Tutorial" folder above. 

## "Outside Metrics"
The file must include a '.csv' extension and use the following naming scheme: Performance-<Type>-horizon-<Horizon Number>-calibration-<Calibration Size>- <Location>-<Forecast Date>. Each "<>" entry indicates file name elements specific to the location, horizon, and calibration length of interest. <Type> refers to the type of performance metrics shown in the file; either "Fit" for model fit metrics or "Forecast" for forecast performance metrics. <Horizon Number> is the length of the forecasting horizon, and <Calibration Size> refers to the length of the data used to calibrate the model. Regarding <Location>, it must match one of the locations, including capitalization, used in the data employed throughout the rest of the dashboard. Finally, <Forecast Date> is the last data date used to calibrate the model. If working with yearly data, it needs to be a four-digit year ("YYYY").  If working with weekly or daily data, <Forecast Date> needs to be in "MM-DD-YYYY" format. Regarding the data format, the first three columns of the data must be labeled: "Location", "Model", and "Date", with each row corresponding to the metrics for the given model. Each of the remaining columns should include the performance metrics of interest. The cell should be left blank if some metrics are unavailable for a given model. Multiple outside performance metrics can be read into the dashboard at a time. Examples can be found in the "Tutorial" folder above.  

# References 
[1] Hyndman RJ. auto.arima: Fit best ARIMA model to univariate time series. 8.22.0 ed. forecast: RDocumentation. https://www.rdocumentation.org/packages/forecast/versions/8.23.0/topics/auto.arima. 

[2]	R-core. glm: Fitting Generalized Linear Models. 3.6.2 ed. stats: RDocumentation. https://rdrr.io/r/stats/glm.html#:~:text=Fitting%20Generalized%20Linear%20Models%20Description%20glm%20is%20used,predictor%20and%20a%20description%20of%20the%20error%20distribution. 

[3]	Ripley B. glm.nb: Fit a Negative Binomial Generlized Linear Model 7.3-60.0.1 ed. MASS: RDocumentation; 2024. https://www.rdocumentation.org/packages/MASS/versions/7.3-61/topics/glm.nb. 

[4]	Wood SN. gam: Generalized additive models with integrated smoothness estimation 1.9-1 ed. mgcv: RDocumentation; 2023. https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/gam.

[5] Taylor SJ. prophet: Prophet forecaster. 1.0 ed. prophet: RDocumentation; 2021. https://www.rdocumentation.org/packages/prophet/versions/1.0/topics/prophet. 



