

# Trends and unit Roots (lecture two)

# ARMA models


# 1. MA: Moving average process --------------------------------------------


# y = sigma(q) (Bi)*(e t-i)

# Linear function of previous shocks and current shocks

# Simulating MA(1)
# yt = Be(t-1) + e(t)

# B positive: Shock from last period has positive effect
y <- arima.sim(list(order=c(0,0,1), ma= 0.9), n=50)
plot.ts(y)

# B negaitve: Shock has negative impact.
y2 <- arima.sim(list(order=c(0,0,1), ma= -0.1), n=50)

plot.ts(y2)

# How long does shock persist?
acf(y)
y3 <- arima.sim(list(order=c(0,0,1), ma= 0.1), n=50)
y4 <- arima.sim( list(order=c(0,0,3), ma= c(0.5,0.4, 0.3) ), n=50 )
# Order 3 -- persist several periods
acf(y3)
acf(y4)


# 2. Autoregressive process -----------------------------------------------

# AR(p): y(t) = Sigma(p) B(i)*y(t-i) + e(t)

# The y depend on lags of y 
# Example: Temperature

ts.plot(arima.sim( list(c(1,0,0), ar = 0.1), n = 50) ) # THe shock has a bigger impact
ts.plot(arima.sim( list(c(1,0,0), ar = 0.9), n = 50) ) # The y persist -- over time
ts.plot(arima.sim( list(c(1,0,0), ar = -0.5), n = 50) ) # 



# AR or MA
# hard to guess AR or MA process

# Example: Super market sales -- shoc from a large sale
# AR: Bounce up, and slowly decay
# MA: The sale will go below -- reflecting people "bought ahead"


# AR simulated ------------------------------------------------------------

b <- 0.3  # Beta
y <- rep(0,200) 
e <- rnorm(n = 200, 0, 0.2)
c <- 0 # Constant
y[1] <- 0 # Initial value is 0

for(i in 2:200){ y[i] = c + b*y[i-1] + e[i]}

plot.ts(y)

# Writing simulating function, with option for shock 

sim_ar <- function(obs, beta, y1, shockat, shock){
  
  #
  y3  <- rep(0, obs)
  b   <- beta
  c   <- 0
  e   <- rnorm(n = obs, 0, 0.2)
  y3[1] <- y1
  
  # Loop over the values
  for( i in 2:obs){ 
    # The dependent variable
    y3[i] = c + b*y3[i-1] + e[i] 
    # Shock appear at i = 50
    if(i == shockat){ y3[i] = y3[i] + shock}
  }
  
  y3
}

# Simulated AR(1), with shock and diffenent beta 
plot.ts( sim_ar(100, beta = 0.9, y1 = 0, shockat = 50, shock = 3) )

# Simulation function
plot.ts( arima.sim( list( c(1,0,0), ar = .3), n = 200) )

# The MA(1) prosess -------------------------------------------------------

sim_ma1 <- function( alpha, obs, cons, y1){

  # Model description
  a = alpha
  y = rep(0, obs)
  e = rnorm(n = obs, 0, 0.2)
  c = cons
  y[1] = y1
  
  # This part is the prosess 
  for(i in 2:obs){ y[i] = c + a*e[i-1] + e[i]}
  y
}

##
library(tidyverse)

ma <- c(0.005, 0.5, 0.99)
df <- tibble( ma    = ma,
              data  = map(ma,  ~sim_ma1(alpha = .x, obs = 100,0,0 ) ), 
              index = list( seq(1,100)) 
              ) |> 
  unnest(-ma)

df |>  ggplot( aes(x = index, y = data ) ) + geom_line() + facet_wrap(~ma)

plot.ts(arima.sim( list( order = c(0,0,1), ma = .05), n = 100))


# Box-Jenkins model selection ------------------------------------------

# three stage method to select appropriate model for forecasting time series

# 1) ID stage

# Estimation

# Diagnostic



# Autocorrelation (ACF) / correlogram -----------------------------------

# AR(1), ACF falls geometrical, Pacf: disapear fast
acf(arima.sim( list( order = c(1,0,0), ar = 0.7), n = 100) |> head(10))
# Partial autocorrelation
pacf(arima.sim( list( order = c(1,0,0), ar = 0.7), n = 100) |> head(10))

# AR(1) a < 0, ACF: Every other +/-, Pacf: 1 is negative
acf(arima.sim( list( order = c(1,0,0), ar = -0.7), n = 100) |> head(10))
pacf(arima.sim( list( order = c(1,0,0), ar = -0.7), n = 100) |> head(10))

# AR(2) +/-/+, ACF: Every other +/-, Pacf: 1 + , 2.-
acf(arima.sim( list( order = c(2,0,0), ar = c(0.7,-0.7) ), n = 100) |> head(10))
pacf(arima.sim( list( order = c(2,0,0), ar = c(0.7,-0.7) ), n = 100) |> head(10))



# ACF and PACF ------------------------------------------------------------

# AR(1)
y <- arima.sim( list( order = c(1,0,0), ar = -.7), n = 200)

plot.ts(y)
acf(y)  # AR < 0 differ between +/-, 
pacf(y) # First is large, than near 0

# mA(1)
y1 <- arima.sim( list( order = c(0,0,1), ma = -.7), n = 200)
plot.ts(y1)
acf(y1) # First large ,than near 0
pacf(y1) # Decay slowly toward 0

y2 <- arima.sim( list( order = c(1,0,1), ar = -0.7, ma = -.7), n = 200)

plot.ts(y2)
acf(y2)
pacf(y2)

# Selecting model: Box-Jenkins three stages -------------------------------

# 1) ID-stage:           ID models through ACF and PACF
# 2) Estimate stage:     Estimate each test models
# 3) Diagnostic checking:Ensure residuals are White noise

# ARMA: Estimating with Max likelihood ML
#       AR can be estimated with OLS -- from assumption of NO autcorr. in error
#   ML and OLS generate t-statistics - for inference

# 1) ACF/PACF + Ljung-Box statistic

# Diagnostics on residuals
# The ARMA -residuals need to be white noise (zero mean, constant variance, uncorrelated)
# Compute the ACF and PACF of series residauls
# L-B can be used to test: significance of autocorr. of residals of ARMA-model
# H0: cov(ei, ej) == 0

## Choose a model with a good fitt: 
# R2 not used 
# There is a trade-off between residuals sum of squares and loosing degree of freedom

# AIC/BIC (SBC): Account for such trade-off-> penalizing the inclusion of an excessive number of lags
# All based on residual sum of squares

# Decision rule: Lowest value of the criteria
# As the fitt improves -> AIC and BIC approach -infinity

# Not overfitt (Parsimony)

## 2) Model selection
# Automated alogrithms for procedure to select models



# Example -------------------------------------------------------

## AR(1)
y <- arima.sim( list( order = c(1,0,0), ar = .3), n = 200)

# Plotting rutine
plot.ts(y)
acf(y)
pacf(y)

# Estimating
model1 <- arima(as.xts(y), order = c(1,0,0), include.mean = T, method = "CSS")

model1
library(xts)
summary( lm(as.xts(y) ~ lag(as.xts(y),1)) )

#
library(forecast)

Arima(y, order = c(1,0,0))

## Information criteria

y <- arima.sim( list( order = c(2,0,0), ar = c(.3,-.6)), n = 10^3) # Data

# Four diff. models, for testing 
model.a <- Arima(y, order = c(1,0,0))
model.b <- Arima(y, order = c(2,0,0))
model.c <- Arima(y, order = c(0,0,1))
model.d <- Arima(y, order = c(0,0,2))

vec_aic <- 
  list(model.a,
       model.b,
       model.c,
       model.d) |>  map_dbl( ~.x$aic)

# Tellers model 2 has the best predicition.
vec_aic


##

# Model selection -- automation of the selection --------------------------

model.aa <- auto.arima(y) #

vec_aic
model.aa # Select 2 lags at the optimum.


# Seasonal ----------------------------------------------------------------

# Periodic regularities "seasonality"

# The pure seasonal models of period S, the behavior ACF/PACF will appear lags s,2s,3s osv

## Example of Combined seasonal/non-seasonal models:

# seasonal AM term: y(t) = ay(t-1) + e(t) + B1e(t-1) + B4e(t-4)
# seasonal AR term: y(t) = ay(t-1) + ay(t-4) + e(t) + B1e(t-1)

## How it is don in R:

## Seasonal model
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
bts <- ts(births, frequency = 12, start = c(1946,1))

bts_components <- decompose(bts)

bts_components
plot(bts_components)

## How to find the seasonality manually

acf(bts)
pacf(bts)

model.season <- Arima( bts, order = c(2,1,2), seasonal = c(1,1,1))

model.season

# Model were we dont adjust for season
model1 <- auto.arima(bts)
model2 <- Arima( bts, order = c(2,1,2), seasonal = c(1,1,1))

# 
ts_sa <- seasonal::seas(bts)

plot(ts_sa)

model.season.auto <- auto.arima( ts_sa$data[,1])

model.season.auto

## How use the model to predict?
















