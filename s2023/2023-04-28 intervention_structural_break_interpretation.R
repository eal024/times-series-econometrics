

library(tidyverse)
library(lubridate)
library(forecast)

#
intervention_date <- ymd("1955-08-04")

data <- vroom::vroom("data/Crest.csv") |> 
  mutate( date = dmy(date),
          int_date = ifelse( date == intervention_date , 1, 0)
  )

#
tscrest <- ts( data$Crest_share, frequency = 52, start = c(1953,1))

plot.ts(tscrest)

# Divide the sample
pre <- data[ data$date <= intervention_date, ]
post <- data[ data$date > intervention_date, ]

model1 <- Arima( tscrest, c(1,0,0))

tsdiag( model1, gof.lag = 30)

# Interpretation of Ljung-Bac statistic:
# test that checks if autocorrelation exists in a time
# H0: The residuals are independently distributed.
# Ideally, we would like to fail to reject the null hypothesis.
# we would like to see the p-value of the test be greater than 0.05, 
# because this means the residuals for our time series model are independent

# Q = n(n+2) Σpk2 / (n-k)
# Ex. X-sq. = Q = 6.0721 and the p-value of the test is 0.8092
# which is much larger than 0.05. Thus, we fail to reject the null
# Adf test: P-value less than alpha=0.05. Null hypothesis is rejected. It is stationary.


tsdiag( arima_pre <- Arima( pre$Crest_share, c(1,0,0)) )
tsdiag( arima_post <- Arima( post$Crest_share, c(1,0,0)) )

# Model with the intervetion

data |> View()

# step.ADA -> after the intervetion
arima_icl_interv <- Arima(  data$Crest_share, order = c(1,0,0), xreg = data$step.ADA)



# Example 2: Concerned spurios: test with first diff -----------------------------------------------

# Concerned spurios: test with first diff
set.seed(1234)
n <- 300
e.1 <- rnorm(n,0,1)
e.2 <- rnorm(n,0,1)
x <- arima.sim(n=n,list(order=c(0,1,0)),innov = e.1)
y <- arima.sim(n=n,list(order=c(0,1,0)),innov = e.2)

summary(lm( y ~x)) # Significant
summary(lm( diff(y) ~x[-1]))  # non-sig
summary(lm( diff(y) ~diff(x) )) # non sig.

plot( lm(y~x) |> residuals() , type = "l") # A time trend
plot( lm( diff(y) ~diff(x) ) |> residuals() , type = "l") # A time trend


# Terror ------------------------------------------------------------------

library(quantmod)
library(urca)
library(strucchange)
library(tseries)
library(stats)
library(forecast)

terror <- vroom::vroom("data/terrorism.csv") |> 
  janitor::clean_names() |> 
  mutate( date = dmy(date))

# xts object
xts_terror <- xts( terror[2:3], order.by = terror$date)

plot(xts_terror$domestic)
plot(xts_terror$transnational)

## Seem to be a structural break -- seen from the plot
pre <- xts_terror[index(xts_terror) < ymd("1998-01-01")]
post <- xts_terror[index(xts_terror) >= ymd("1998-01-01")]

acf(pre$transnational) ## Seem unstable
acf(post$transnational) ## seem stable

# Dummy for account for the decline
terror$z <- ifelse( index(xts_terror) < ymd("1998-01-01"), 0, 1)
xts_terror$z <- ifelse( index(xts_terror) < ymd("1998-01-01"), 0, 1)

summary( terror |>  lm( formula = transnational ~ z ))


model_2 <- Arima(xts_terror$transnational, order=c(2,0,0),xreg=xts_terror$z)

summary(model_2)
