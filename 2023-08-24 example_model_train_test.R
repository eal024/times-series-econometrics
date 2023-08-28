
library(forecast)

# Model a trend series with de-trending and differencing
# Which one forecsats best?

## Data
ts_trend <- 1:100*0.3 + rnorm(100, 0.1, .75)

plot.ts(ts_trend)

# Training data
df_train <- data.frame( time = 1:75, var = ts_trend[1:75])

model_trend <- lm( var ~ time , data = df_train) # Trend model

train_detrend <- df_train$var - model_trend$coefficients[2]*1:75 

# The data detrended
plot.ts(train_detrend)

## What about the differenced values?
diff(ts_trend) |> plot.ts()

## Creating the best model with the detrended data
model_detrend <- auto.arima( train_detrend)
model_diff <- auto.arima( diff(ts_trend))

## Testing the models
# Predicting
forecast_model_detrend <- predict( model_detrend, 25) 
forecast_model_diff    <- predict( model_diff, 25)

forecast_full_detrend <- model_trend$coefficients[2]*76:100 + forecast_model_detrend$pred
forecast_full_diff <- train[75] + cumsum(forecast_model_diff$pred)

## What models fitts best?
plot.ts( ts_trend[76:100])
lines( forecast_full_diff, col = "red")
lines( as.numeric(forecast_full_detrend), col= "blue")

# Measure RMSE: 
mean( ((ts_trend[76:100] - forecast_full_detrend)^2)^0.5 )
mean( ((ts_trend[76:100] - forecast_full_diff)^2)^0.5 )











