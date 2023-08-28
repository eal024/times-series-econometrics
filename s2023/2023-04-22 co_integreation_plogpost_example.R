

library(tidyverse)
library(lubridate)
library(xts)
library(dynlm)

# GDP and stock return
rangvid_dat <- readxl::read_excel("data/rangvid_data.xls") |> 
  janitor::clean_names() |> 
  mutate( date = ymd( paste0(entry, "-01-01") ) 
          ) |> 
  relocate(date, .before = entry) 


xtsdat <- xts( rangvid_dat[,-c(1:2)], order.by = rangvid_dat$date)


# Plot the data
rangvid_dat |> 
  select(date, p, gdpcur) |> 
  pivot_longer( -date) |> 
  ggplot( aes( y = value, x = date, color = factor(name ) ) ) +
  geom_line() 


# Time series plot
p <- lattice::xyplot( p ~date, data = rangvid_dat, type = "l")
gdp <- lattice::xyplot( gdpcur ~date, data = rangvid_dat, type = "l")
latticeExtra::doubleYScale(p, gdp, style1 = 0, style2 = 3, add.ylab2 = T)

## Regression
rangvid_dat |> 
  ggplot( aes( y = p, x = gdpcur ) ) + 
  geom_point() +
  geom_smooth( method = "lm", color = "darkred",  linetype = 2 ) +
  geom_smooth( method = "loess", se = F  ) 

# Regression result: Shows correlations
summary( lm(data = rangvid_dat, p ~gdpcur))

# 1) Make sure that the data is stationary -> not dependent on the time
# The data should not show trend or seasonality


# co integreation: That the time series are correlated.
# A and B are cointegreated if the difference between them is stationary
  # each series is not stationary --> it shows a trend
  # iF SERIES NOT CO INT. -> the diff is. non stationary. (and thus shows a trend or a seasonality)

# Looking for two non-stationary series -> which the differneece is stationary

fun_plot_line_acf_pacf <- function( y) {
  plot.ts(y)
  acf(y,50)  
  pacf(y)
}

fun_multi_plot <- function(list_y){
  par(mfrow = c( length(list_y), 3))
  walk(list_y, function(y) fun_plot_line_acf_pacf(y) )df4$
}

fun_multi_plot( list_y =  list(y, na.omit(diff(y)) ) )

# The season is still appearing in the diff. data   

# Look at the corration.
cor(xtsdat$p, xtsdat$gdpcur) # Highligh correlated

# cross correlation function
ccf( as.numeric(xtsdat$p), as.numeric(xtsdat$gdpcur), plot = F)

## astsa: Correlatio between value in p and laged value of gdp
astsa::lag2.plot ( as.numeric(xtsdat$p),
                   as.numeric(xtsdat$gdpcur), 10)

## ADF-test for unit root.
# unit-root: if the ts has unit root -> it is not statinoary and need to be diff
# NULL: The unit-root exists
# If the p-value s lower than threshold (0,05) the data is statonary or trend-stationary
# Trend stationary: Series becomes stationary once the trend is removed

# Test can be done:
# removeing nothing
# a constant
# a trend
urca::summary( test <-urca::ur.df( xtsdat$p, lags = 2, selectlags = "AIC", type = "trend"))

# Interpreting: ACF: -3,6  7,1 , 7,3
# test for tau3, phi2 and phi3
# respective: trend, drift and none

# The value is higher than 5 pst. -> The data is stationary
urca::plot(test)

# Other method for testing unit root
urca::summary( urca::ur.ers(xtsdat$p, model = "trend", lag.max = 2) )

Box.test( xtsdat$p, lag = 2, type = "Ljung-Box") # Low p-value means that data is not idd not independently distributed -> there is serial correlation

tseries::kpss.test( xtsdat$p, null = "Trend") # Null assumes a stationary sereies. NULL = trend stationary 
# p-value bewlo 0.1 means non agreemnent with the NULL.

##
urca::summary( test_gdp <-urca::ur.df( xtsdat$p, lags = 2, selectlags = "AIC", type = "trend"))

urca::plot(test_gdp)
# Other:
  # Sample size


## The difference between the variables
diff <- xtsdat$p - xtsdat$gdpcur

# It is only the diff thats matter. Scale does not.
urca::summary( test_diff <- urca::ur.df(diff, selectlags = "AIC", lags = 2, type = "trend"))
urca::plot( test_diff)

urca::summary(urca::ur.ers(diff, 
               type="DF-GLS",
               model = "trend", 
               lag.max=2)) 

# The ur.df test say that that the unit-root cant be rejected.
# co-integred: two non-stationary dataset, and the diff between them is stationary. 

# More ways of testing co integretion.

# If two univariate time-series are co-integrated, 
# then the OLS estimator of the coefficient in the cointegrating regression is consistent.
library(dynlm)

p <- ts(xtsdat$p, start = c(1930,1), frequency = 1)
gdp <- ts(xtsdat$gdpcur, start = c(1930,1), frequency = 1)

dyn_p_gdp <- dynlm(p ~gdp)
par(mfrow = c(2,2))

## Engle Granger -test
# NULL: two or more time series, each I(1) are not cointegredet.

aTSA::coint.test( p, gdp, d = 1, nlag = 2, output = T)
# At lag 2 the data are co integredet at the 1¤ at no trend

# Rgressing Price on GDP

diff <- p - gdp
vecm_eq1 <- dynlm( d(p) ~ L(d(gdp),1:2) + L(d(p),1:2) + L(diff) )
vecm_eq2 <- dynlm( d(gdp) ~ L(d(p),1:2) + L(d(gdp),1:2) + L(diff) )

names(vecm_eq1$coefficients) <- c("Interc", "gdp_l1","gdp_l2", "p1" ,"p2", "ectl1" )
names(vecm_eq2$coefficients) <- c("Interc", "p_l1","p_l2", "gdp1" ,"gdp2", "ectl1" )

vecm_eq1$coefficients

library(lmtest)
library(sandwich)
lmtest::coeftest(vecm_eq1,
                 vcov. = sandwich::NeweyWest(vecm_eq1,prewhite=F,adjust=T)
                 )
# 
lmtest::coeftest(vecm_eq2,
                 vcov. = sandwich::NeweyWest(vecm_eq2,prewhite=F,adjust=T)
)

## Test if the laged values are relevant or not.

# Is laged GDP influecing P?

# Restriced model:
# Model1: Restricted
# Model2: VECM
car::linearHypothesis(vecm_eq1, 
                      hypothesis.matrix=c("gdp_l1", 
                                          "gdp_l2"),
                      vcov. = sandwich)

# THe result: The F value is low, meaning that the lags should not be incl.
car::linearHypothesis(vecm_eq2, 
                      hypothesis.matrix=c("p_l1", 
                                          "p_l2"),
                      vcov. = sandwich)
# P value < 10%, the lag should be included.

## Granger test:
grangertest(p~gdp, order = 2)
# Test of gdp included vs. not included.
#  Model2 without GDP seems better
# should not include gdp

grangertest(gdp~p, order = 2)
# Test of gdp included vs. not included.
#  Model without GDP seems better
# should not include gdp



# Build a VAR: ------------------------------------------------------------



