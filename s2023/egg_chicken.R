

## Granger egg -- chicken

data <- ChickEgg

plot(data[,1])
plot(data[,2])

acf(data[,1])
acf(data[,2]) # non-stationary


urca::summary(urca::ur.df( data[,1], type = "none" , lags = 6, selectlags = c("BIC")))

urca::summary(urca::ur.df( data[,2], type = "none" , lags = 6, selectlags = c("BIC")))
# Low  test-stat: cant reject H0 of non-stationary


dchick <- diff(data[,1])
degg <- diff(data[,2])

urca::summary(urca::ur.df( degg, type = "none" , lags = 6, selectlags = c("BIC")))

# H0: null hyp. of no Granger causalit
lmtest::grangertest(chicken ~ egg , data = data)
lmtest::grangertest(egg ~ chicken , data = data)

# Egg explain chicken --- egg came first
lmtest::grangertest(dchick ~ degg)
lmtest::grangertest(degg ~ dchick)





