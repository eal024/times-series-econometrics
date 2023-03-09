

# 
library(quantmod)

getSymbols("PRICENPQUSDM", src = "FRED")

# Real data
y <- PRICENPQUSDM

plot(y)

# Some info
length(y)

# Divide the data
y_new <- tail(y, 150) # the 150 last observations
plot(y_new)

library(urca)
# Estimating the drift model 
summary(ur.df(y_new, type = "none")) # ADF-model

## Replica: The ADF-"none" model
summary(lm( diff(y_new) ~  0 +  lag(y_new,1) + lag(diff(y_new), 1) ))

# Look at the autcorrelation
acf( na.omit(diff(y_new)) )
   
### Choose the best model:
p <- 5
q <- 5
store_aic <- matrix(NA, p+1, q+1)

for(i in 0:5){
  for(j in 0:5){
    temp.aic <- Arima( diff(y_new), order = c(i,0,j), include.mean = F )$aic
    store_aic[i+1,j+1] <- temp.aic
  }
}

min(store_aic) == store_aic 
Arima(diff(y_new), order = c(2,0,3), include.mean = F)

Arima(diff(y_new), order = c(0,0,1), include.mean = F )

# Automatic optimum -------------------------------------------------------

model.auto <- auto.arima( diff(y_new))
model.auto # Differ from Arima

## Automation 

# AR = 2, MA = 1
y <- arima.sim( list( order = c(2,0,1), ar = c(.3, -.6), ma = c(0.7)), n= 10^3)

# Autochoose the optimal model
auto.arima(y)

## Real data
library(quantmod)


getSymbols("DEXTHUS", src = "FRED")

str(DEXTHUS)
plot.ts(DEXTHUS)




















