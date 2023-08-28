
# Solution Q2 S2, terror data

library(quantmod)
library(urca)
library(tseries)
library(forecast)
library(stats)

# data
data <- read.csv("data/terrorism.csv") |> janitor::clean_names() |> transform( date = as.Date(date, "%d/%m/%Y"))

# Convert to xts
dataxts <- xts(data[2:3], data$date)

# Plotting the data
plot( data$domestic, x = data$date, type = "l")

# There seems to appears a structural break around 1997
# This is conected to state departemtet US diplmatic law enforcemtn measures for reducing terror
plot( data$transnational, x = data$date, type = "l") 

# 4.2 Look at the data before and after 1997
library(lubridate)
ypre <- data$transnational[ data$date <= as.Date("1997-10-01")]
ypost <- data$transnational[ data$date > as.Date("1997-10-01")]

# Looking at the residuals
acf(ypre)  # acf decays slowly     -> Before 1998, the models may have been AR(2)
pacf(ypre) # two signifcant spices
acf(ypost) # 
pacf(ypost)

# Looking at alternative model
auto.arima(ypre) # 

# 4.3 Generate dummy
data$z <- ifelse( data$date >= ymd("1998-01-01"),1,0)

# Convert to xts
dataxts <- xts(data[, 2:ncol(data)], data[,1])

model1 <- lm(data = dataxts, transnational ~ z)

library(dynlm)
model1_dynlm <- dynlm(data = ts(dataxts), transnational ~ z)

summary(model1)
summary(model1_dynlm)

acf(model1$residuals)

model2 <- Arima( dataxts$transnational, order = c(2,0,0), xreg = dataxts$z)

summary(model2)
acf(model1$residuals) # 
acf(model2$residuals) # The correct model seems to be AR(2) = white noise


# The long run effect: 
summary(model2) # Intercept minus B z
28.6971-14.86

# ARIMA-reports intercept mean value intercept.and the change in mean assoicated with the dummy

# 4.4 ACF/pacf the whole sample
acf(dataxts$transnational)  # Since the acf decays slowly -> indication of a unit root

# Lets test this with the adf-test
summary(ur.df(dataxts$transnational, lags = 12, type = "drift", selectlags = "AIC")) # 
summary(ur.df(dataxts$transnational, lags = 1, type = "drift")) # Same here

# 
kpss.test(dataxts$transnational, lshort = F)
# 

# 4.5 ARMA(1,1)











