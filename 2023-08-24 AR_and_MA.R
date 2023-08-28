

# Simulating data, for investigation

# AR
n <- 100
y <- arima.sim( n = n, list(ar = 0.5)) + 2 # AR(1)
yy <- arima.sim( n = n, list(ar = c(0.5, 0.25, 0.15))) + 2 # AR(1)
z <- arima.sim( n = n, list(ar = c(-0.5))) + 2 # AR(1)

plot.ts(y)
acf(y)

plot.ts(yy)
acf(y)

plot.ts(z)
acf(z)

# MA
n <- 100
y <- arima.sim( n = n, list(ma = 0.5)) + 2 # AR(1)
yy <- arima.sim( n = n, list(ar = c(0.5, 0.25, 0.15))) + 2 # AR(1)
z <- arima.sim( n = n, list(ar = c(-0.5))) + 2 # AR(1)

plot.ts(y)
acf(y)

plot.ts(yy)
acf(y)
pacf(y)

plot.ts(z)
acf(z)


