

# Granger causality: "x->y, if y is predicted better w. x(t-s) includeded"

library(lmtest)
library(urca)
data(ChickEgg)

dat <- ChickEgg # For name convention 

head(dat)
plot.ts(dat)

# Testing stability
ur.df( dat[,1], type = "none" ,lags = 6, selectlags = c("BIC"))
ur.df( dat[,2], type = "none" ,lags = 6, selectlags = c("BIC"))

# Variables seem not stable

d_chick <- diff( dat[,1])
d_egg <- diff( dat[,2])

plot.ts(dat[,1])
plot.ts(d_chick)
plot.ts(d_egg)

# Seems ok
ur.df(d_chick, type = "none", lags = 6, selectlags = c("BIC"))
ur.df(d_egg, type = "none", lags = 6, selectlags = c("BIC"))

grangertest(chicken ~ egg, order = 4, data = dat) ## 4.26: Egg lags are significant = Egg predict Chicken.
grangertest(egg ~ chicken, order = 4, data = dat) ## 0.39: Lag Chicken do not predict Egg

# H0: B=0 
# F > ? (F-sign) reject the H0 of no Granger causality

grangertest(d_chick ~ d_egg) # 
grangertest(d_egg ~ d_chick) # 
# Same conclusion from using the lag variable


