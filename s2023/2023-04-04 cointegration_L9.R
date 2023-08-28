
# Simulation

# 1) Non-stationary variable that share a common trend are cointegrated


# 
set.seed(1)
n <- 1000

# Random walk
mu <- cumsum( 0.1 + rnorm(n, 0, 2) )

plot(ts(mu)) # Plotting the data

e1 <- rnorm(n, 0, 3)
z <- mu + e1  # z = mu + e1
plot( ts(z))

# y
y <- mu*2
e2 <- rnorm(1000, 0,3)
y + e2 # y = mu + e2, which mean z and y share a common stocahstic trend up to a scalar
plot( ts(y))

plot(y,z)
reg1 <- lm( y ~ z)
summary(reg1)
plot(z,y)
lines(2*1:length(y), col =  "red")

reg2 <- lm( z ~y)
summary(reg2)
plot(y,z)
lines(0.5*1:length(y), col =  "red")
hist( ts(reg2$residuals)) # Stationary residuals -> reg y ~ z can be done.
plot( ts(reg2$residuals))

# Example two: No
w <- cumsum( 0.2 + rnorm(n, 0, 2) )

plot(w,z)
reg3 <-  lm( w ~z)
plot(ts(reg3$residuals))
hist(ts(reg3$residuals))






