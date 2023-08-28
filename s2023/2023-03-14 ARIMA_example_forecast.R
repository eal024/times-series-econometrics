
library(tidyverse)

data("co2")

class(co2)

maunaloa <- co2

# Data, co2 over time
head(maunaloa)

# ggplot auto function
autoplot(maunaloa)

# Start and end date
tsp(co2)

head(maunaloa, 1)
tail(maunaloa, 1)

# Cycle of the time series
cycle(co2)

# season
# Seasonal annual average, gross descriptions
# Graphical rep: 
boxplot(co2~cycle(co2))


# Decomposite in season, mm.
# Y(t) is the concentration of co2 at time t,
# T(t) is the trend component at time t,
# S(t) is the seasonal component at time t,
# e(t) is the random error component at time t.
decompose(co2) |> autoplot()

## Test of Stationarity

# ADF: Augmentet Dickey-Fuller
library(forecast)
tseries::adf.test(co2)
# p-value: 0.2269: accept the null-hup. that the time sereis is non stationary
ggplot2::autoplot(acf(co2, plot = F)) + labs(title = "corre. of co2, 1959--1998")

# Alternativ
acf(co2)
acf(co2, plot = F)

## See the seasonal pathern
autoplot( pacf(co2, plot = F)) + labs( title = "corr. co2 from 59 to 1998")
# Indication of positive relationship with 12 month cycle

## Remove trend and season effect
# Three paramter: AR, I, MA

# Need to remove unequal variance, and trend
# This is don by log and trend

tseries::adf.test( diff(log(co2)), alternative = "stationary", k = 0)
## The series are now statonary. p-value = 0.01 can reject non statoarny.

# the d-componenet is 1 (from one diff)

acf(diff(log(co2)))
pacf(diff(log(co2)))

#Use the plot to guide model selection: 
# The ACF plot can help you identify potential models for your data.
# For example, if you see significant spikes at lags 1 and 12,
# this suggests that the data has a monthly seasonal pattern and an ARIMA
# model with seasonal components may be appropriate.

## Fitting the model

# Linear model
autoplot(co2) + geom_smooth( method = "lm", se = F)
# Does not capture the seasonal and additive effects

# ARIMA
model1 <- auto.arima( co2, seasonal = T)

summary(model1)

# ARIMA(1,1,1)(1,1,2)[12]
#[12]: indication of 12 periodes (month)

ggfortify::ggtsdiag(model1)
# Ljung-Box test show a p-value pretty high; the SARIMA model is a fairly good fit.

# ARIMA() fits the model using 
# maximum likelihood estimation (assuming Gaussian residual series)

res <- residuals(model1)

hist(res)
qqnorm(res)
qqline(res)

# Points seem linear. data seems normally distributed, with mean 0


# Forecast  ---------------------------------------------------------------

library(forecast)

# Forecast
# CI 95%
# h: foreceast horizon
model_forec <- forecast(model1, level = c(95), h = 36)

autoplot(model_forec)

