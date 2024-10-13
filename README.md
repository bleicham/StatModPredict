# StatModPredict: A User-Friendly R-Shiny Interface for Fitting and Forecasting with Statistical Models. 

**StatModPredict** is a user-friendly R-Shiny dashboard interface for fitting and forecasting time series data using user-specified variations of auto-regressive integrated moving average (ARIMA), generalized linear models (GLM), generalized additive models (GAM), and Facebook's Prophet model building around the existing auto.arima [1], glm [2] and glm.nb [3], gam [4], and prophet [5] functions in R. The dashboard eliminates the need for previous coding experience and facilitates real-time and retrospective forecasting efforts for various processes. At a minimum, the dashboard takes time-series data and returns model fits and forecasts, associated figures, and model fit and forecasting performance (when applicable). Additionally, the dashboard facilitates model comparison by allowing users to incorporate previously conducted forecasts and performance metrics for models not included in the dashboard into the interface. 

# Video Tutorial 
There is a video tutorial available on YouTube which employs the data included within the "Tutorial" folder above. The tutorial can be found at: https://www.youtube.com/watch?v=zgZOvqhvqw8

# Software Requirments 
- **Language:** R (>=4.3)
- **Integrated development environment:** RStudio(>=2024.04.1 Build 748)
- **Packages:** pacman & Shiny

# Preparing the data
## Primary data
The primary data refers to the data set used to fit all models, produce forecasts, and to evaluate the model fit and forecasts. 
## "Outside Forecasts"
## "Outside Metrics"

# References 
[1] Hyndman RJ. auto.arima: Fit best ARIMA model to univariate time series. 8.22.0 ed. forecast: RDocumentation. https://www.rdocumentation.org/packages/forecast/versions/8.23.0/topics/auto.arima. 

[2]	R-core. glm: Fitting Generalized Linear Models. 3.6.2 ed. stats: RDocumentation. https://rdrr.io/r/stats/glm.html#:~:text=Fitting%20Generalized%20Linear%20Models%20Description%20glm%20is%20used,predictor%20and%20a%20description%20of%20the%20error%20distribution. 

[3]	Ripley B. glm.nb: Fit a Negative Binomial Generlized Linear Model 7.3-60.0.1 ed. MASS: RDocumentation; 2024. https://www.rdocumentation.org/packages/MASS/versions/7.3-61/topics/glm.nb. 

[4]	Wood SN. gam: Generalized additive models with integrated smoothness estimation 1.9-1 ed. mgcv: RDocumentation; 2023. https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/gam.

[5] Taylor SJ. prophet: Prophet forecaster. 1.0 ed. prophet: RDocumentation; 2021. https://www.rdocumentation.org/packages/prophet/versions/1.0/topics/prophet. 



