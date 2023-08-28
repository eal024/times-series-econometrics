
# Simulate data
set.seed(100)
obs <- 500
trend <- 1:obs*0.1
noise <- rnorm(n = obs, 0,1)

y <- arima.sim( list(order = c(1,0,0), ar = .5), n = obs)   + trend + noise # Data generatig process.

plot.ts(y) # Ploting the data

# Create a model
library(forecast)

500*0.8 # Splitting the data 80/20 train/test
y_train <- y[1:400]

## Predict the trend
trend_model <- lm( formula = y_train ~ I(1:length(y_train) ) )

## Extract the trend
y_exl_trend <- y_train - trend_model$coefficients[[2]]*1:length(y_train) 


plot.ts(y_exl_trend)

# Algorithm for get the best AR(p) model
ar_model <- auto.arima(y_exl_trend)

forecast_ar <- predict(ar_model, 0.2*500)

y_forcast <- trend_model$coefficients[[2]]*401:500 + forecast_ar$pred

plot.ts( y[401:500] , ylim = c(30,60))
lines( as.numeric(y_forcast, col = "red"))

