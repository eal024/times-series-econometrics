

# MA
n <- 100
y <- arima.sim( n = n, list(ma  = 0.5) )  # MA(1)
z <- arima.sim( n = n, list(ma = c(-0.5))) # MA(1)

# yy <- arima.sim( n = n, list(ar = c(0.5, 0.25, 0.15))) + 2 # AR(1)


plot.ts(y) # 
forecast::Acf(y) # Here is correct -- should be the first  
forecast::Pacf(y)

plot.ts(z) #
forecast::Acf(z) # Here is correct -- should be the first  
forecast::Pacf(z)


# MA( order of 2)

yy <- arima.sim( n = n, list(ma  = c(0.5, 0.3)) ) + 2 # MA(1)

plot.ts(yy)
forecast::Acf(yy) # Two positive
forecast::Pacf(yy)
