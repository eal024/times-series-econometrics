


# Simulating ARMA-data ----------------------------------------------------

# Simulate data
set.seed(100)
obs <- 100

# Moving average MA(1)
y <- arima.sim( list(order = c(0,0,1), ma = 1.9), n = obs)  # Data generating process.
plot.ts(y) # Ploting the data

y1 <- arima.sim( list(order = c(0,0,1), ma = -0.9), n = obs)  # Data generating process.
plot.ts(y1) # Ploting the data


## AR(1)
plot.ts( arima.sim( list(order = c(1,0,0), ar = 0.9), n = obs) )
plot.ts( arima.sim( list(order = c(1,0,0), ar = -0.9), n = obs) )


# Simulating data with trend ----------------------------------------------

set.seed(123)
obs <- 500
ar    <- arima.sim( list(order = c(1,0,0), ar = 0.9), n = obs)
trend <- 1:obs*0.1
noise <- rnorm(n = obs, 0,1)

hist(noise)
plot( trend, type = "l")

y <- ar + trend + noise

plot.ts(y)


# Trend -------------------------------------------------------------------











