

# Packages
library(xts)
library(urca)
library(vars)
library(data.table)
library(tidyverse)

# Data preperation
data("PhillipsCurve")
uk <- window( PhillipsCurve, start = 1901)
tid <- seq.Date( from = as.Date("1901-01-01"), length.out = 87, by = "years")
dt <- as.data.table(uk)[ ,date := tid,][]
ukxts <- xts( dt , order.by = tid) # Convert the data to a xts-object

y <- dt[, .(u,dp),][] # Extract unemployment and inflation

y |> mutate( i =1:nrow(y)) |> pivot_longer(-i) |> ggplot( aes( y = value, x = i, fill = name)) + geom_line()

# Look at the data
qplot(u,dp, data = y)
lm( data = y, u ~dp) |> summary()

y2 <- dt[ , .(dp,u),]

qplot(dp,u, data = y2)

## 
y$dp_1 <- lag(y$dp,1)
y$u_1 <- lag(y$u,1)
head(y)

lm( data= na.omit(y), u ~ dp + dp_1 + u_1) |> summary()

VARselect(y[ ,.(u,dp),], lag.max = 8, type = "const") # return information criteria on VAR with different number of lags


# Estimating the VAR ------------------------------------------------------

lm( data= na.omit(y), dp ~  dp_1 + u_1) |> summary()
lm( data= na.omit(y), u ~  dp_1 + u_1) |> summary()

varout1 <- VAR(y = y[ ,.(u,dp),], p = 1, type = "const")
varout2 <- VAR(y = y2, p = 1, type = "const")

summary(varout1) # Look at what is estimated

res_var1 <- residuals(varout1)

plot(ts(res_var1[,1])) # Unemployment
abline(0,0, col = "red")

plot(ts(res_var1[,2])) # inflation
abline(0,0, col = "red")


# plot auto
plot(varout1)
acf(res_var1)

# Testing for correlation in errors -- Portmanteau test
serial.test(varout1) # H0: no correlation

arch.test(varout1) # H0: No arch errors

normality.test(varout1) # H0: is normality
plot(normality.test(varout1))

## Testing for stability of the parameters
plot( stability(varout1, type = "OLS-CUSUM")) # Return one-dim. empirical process of cum sum of OLS residuals 
# If stable: the cumsum should stay around 0

plot(stability(varout1, type="Rec-CUSUM")) # recursive estimation


# Inference ---------------------------------------------------------------

# Does "u" Granger causes "dp"
grangertest(y$u, y$dp, order = 3)
grangertest(y$dp,y$u, order = 3)

irfs_varout1 <-
  irf(
    varout1,
    response = c("dp", "u"),
    n.ahead = 48,
    ortho = T,
    boot = T,
    runs = 500
  )

plot(irfs_varout1)

# Given the ordering av response, we are assuming pd <- u. Shocks in dp are assume not to cotemp. effect u 

# ortho = F, look for no contemporan. effects
irfs_varout1 <-
  irf(
    varout1,
    response = c("dp", "u"),
    n.ahead = 48,
    ortho = F,
    boot = T,
    runs = 500
  )

plot(irfs_varout1)

# Forecasting
pred <- predict(varout1, n.ahead = 10, ci = .95)

print(pred)
plot(pred)

# We can easily calculate the forecast error variance
#This allows us to calculate the proportion of the expected variance in a variable that is due to each of the structural shocks in the model.

fevd(varout1)
fevd(VAR(y = y2, p = 2, type = "const"))

