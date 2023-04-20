

## 
library(tidyverse)
library(tseries)
library(xts)

## Box-Jenkins (1976) model selection: Three stage method

df <- vroom::vroom("data/Quarterly.csv") |> janitor::clean_names()

xtdat <- xts( df[c(9)], as.Date(df$date, "%d/%m/%Y") )

# 1) Identification stage
  # Visually
  # structural breaks
  # non-stationary
  # deal with non-stationary
  # autocorrelation, partial autocorrelation ACF and PACF

# 2) Estimation stage
 # Estiamte each of the models

# 3) Diagnostic checking: Ensure that residals are white noise


# 1 Id stage --------------------------------------------------------------

# Visually
# structural breaks
# non-stationary
# deal with non-stationary
# autocorrelation, partial autocorrelation ACF and PACF

# Upward trend, signifcant. Non-stationariy, since there is a trend
autoplot(xtdat)

# The ACF, pACF confirm that
acf(xtdat)  # Slowly declining (acf), indication of non stationary (slowly decaying)
pacf(xtdat) # Parterly autocorr function: one significant spike -- signaling that diff. can be used

# How transform the data?
xtdat$inf <- na.omit(100*(diff(log(xtdat$cpi))))

## Need to diff two times
## Look at the Autocorrelation function, again.
acf( na.omit(xtdat$inf) )
acf( na.omit(diff(xtdat$inf)) )


xtdat$dinf <- na.omit(diff(xtdat$inf))

## Seems ok.



# 2 Estimation stage ------------------------------------------------------

library(forecast)

# Auto choice model. Based on AIC, BIC
auto.arima( xtdat$dinf)

## Estimate the model
model <- arima( xtdat$dinf, order = c(1,0,2))

# 3 diagnostic
# H0: That the E(e) = 0, white noise
# P > 0.05, 
Box.test( model$residuals) # 

# Grapichal look
res <- model$residuals

hist(res)
qqnorm(res)
qqline(res)

## Forecasting
pred <- predict( model, n.ahead = 30)

y <- c(xtdat$dinf, pred$pred) 

ts.plot( ts(xtdat$dinf),  pred$pred, xlim = c(150, 220))
lines( pred$pred, type = "p", col = "red")

pred
plot( pred$pred, type = "o", main = "Forecast value")






