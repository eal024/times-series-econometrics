

# 1) non-stationary variables that share a common trend are co integrated


# 
set.seed(1)
n <- 1000

mu <- cumsum( 0.1 + rnorm(n, 0, 2)) # RW with a drift
plot(ts(mu))

e1 <- rnorm(n, 0, 3)
z <- mu+e1 # 1
plot(ts(z))

y <- mu*2 # 2
e2 <- rnorm( n, 0 , 3)
plot(ts(y))

# co integration of x -> B(1,-2)
reg1 <- lm( y ~z)  # B = 2
reg2 <- lm( z ~y)  # B = 1/2

summary(reg1) # Significant B
plot(reg1$residuals) # Stationary residuals

plot(ts(y)) # Non-stationary
plot(ts(z)) # Non-stationary

urca::summary( urca::ur.df(y, type = "none"))
# test-statistic is 0.26 < tau1 (-1.95); y H0 non-stationary cant be rejected.

# the cointegration vector is not unique.
z <- 3*mu+e1
y <- -6*mu+e2

reg3 <- lm(y~z)
summary(reg3)

plot( ts(reg3$residuals)) # Stationary residuals

summary(lm(z~y))  # B 1/2







