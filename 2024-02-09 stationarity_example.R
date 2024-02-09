
# Stationarity testing, example and codes from:
# https://rpubs.com/richkt/269797

# Check for stationarity: mean and variance is constant over time

# Example data
t                <- 0:300
y_stationary_int <- rnorm(  length(t),mean=1,sd=1)               # the stationary time series (ts)
y_trend_int      <- cumsum( rnorm(length(t),mean=1,sd=4))+t/100  # our ts with a trend

# lets normalize each for simplicity
y_stationary <- y_stationary_int/max(y_stationary_int) 
y_trend      <- y_trend_int/max(y_trend_int) 

 
# Data: stationary data
plot.ts(ts( y_stationary))

# Non stastionar data
plot.ts(ts( y_trend))

# ACF: Autocorrelation function
# stationary: the lags is non significant.
acf(ts( y_stationary))

# non-stationary: lags is  significant.
acf(ts( y_trend),   lag.max = 30)
acf(ts( y_trend),   lag.max = 300)

# 1) Ljung Box test for independence: Built in test for stationarity.
# Ljung-Box: Examine if there is a sig. evidence for non-zero correlation at given lag 
# H0: independece in a given time: non-stationary: Low P-value
Box.test(y_stationary, lag = 25, type = "Ljung-Box") # P-value above 5% (20%)
Box.test(y_trend, lag = 25, type = "Ljung-Box") # P-value bellow 5% (0,0000%)

# The Augmented Dickey-Fuller (ADF) t-test of unit root
library(urca)

# Unit root. (Large P-value if unit root == trend)

# Test of stationar data
urca::ur.df(y_stationary) |> summary()

# t is 6 > (-2.58 (1%): t-critical values
# Reject the H0 of a unit root. 
# Assume data is stationary

options(warn=-1)
tseries::adf.test(y_stationary)


# Test of non-stator data
urca::ur.df(y_trend) |> summary() # The result indicaiton stationarity. Which we know is wrong
urca::ur.df(y_trend, type = "drift", lags = 6) |> summary()
# Now the test gives 5.3 which is bellow critical values for phi1

# Alternativ
urca::ur.df(y_trend, type = "drift", selectlags = "AIC", lags = 10) |> summary()
# result just above phi1 

