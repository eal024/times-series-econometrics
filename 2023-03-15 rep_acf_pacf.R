
library(forecast)

# acf: Autocorrelation function
y <- arima.sim( list(order = c(1, 0, 0), ar = .7), n = 200)

acf(y)

