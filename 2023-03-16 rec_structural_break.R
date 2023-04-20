
# 
library(data.table)
library(lubridate)
library(xts)
library(forecast)

## Structural break and intervention

terror <- vroom::vroom("data/terrorism.csv") |> 
  janitor::clean_names() |> 
  transform( date = as.Date(date, "%d/%m/%Y")
             ) |> 
  as.data.table()

# Transform to xts object
xtstr <- xts(terror[,2:3], terror$date)

head(xtstr)


# Plot the data -- domastic and transnational
plot(xtstr)

# Looking closer at transnational
plot(xtstr$transnational) # There seems to be a break around 1997
# 1997: U.S staten dep. took diplomatic and low enforcement measures to reduce terror


## Looking at acf, and pacf
acf(xtstr$transnational) 
# Divinding the data
acf(xtstr$transnational[ paste0(ymd("1970-01-01"),"/",ymd("1997-10-01"))]) # Before: Seems to fit AR(2)
acf(xtstr$transnational[ paste0(ymd("1997-10-01"),"/",ymd("2010-10-01"))]) # After


## Generate a dummy for before and after 1997
terror[ , z := ifelse( year(date) > 1997, 1, 0 )][] 

xtstr <- xts(terror[,c(2:3)],  order.by = terror$date)


# Two models

# i) trans.(t) = a + dz + e
# ii)trans.(t) = a + dz + btrans(t-1) + btrans(t-2) + e


# Linear
model1 <- lm( terror, formula = transnational ~ z)

terror[ ,':=' (trans_1 = shift(transnational, 1, type = "lag"),
              trans_2 = shift(transnational, 2, type = "lag")
              )][]

lag(xtstr$transnational, k = 2) |> head()

# Fiting the AR(2) model, with dummy.
model2 <- Arima( xtstr$transnational, order = c(2,0,0), xreg = terror$z  )

# Testing the series corr. of residuals
acf(model2$residuals)
hist(model2$residuals)

# COmpare to model1:
acf(model1$residuals) # Seems residuals are corr.

## Q: What feature does the autocorr. function for the entire sample seems to have?
acf(xtstr$transnational) # Looks like potential unit root, as the acf decaus slowly

# That equ. 1 in charact. equation. in AR
# Stochastic trend that in not stationary, and behvaior is primarilu driven by random shocks.
# p = 1, in AR(1)

## Test for unit root: Augm. Dickey-Fuller test unit root test

# H0: That there is a unit root (p = 1: non stationary). Stat. properities change over time
# HA: That is stationary. And Stat. prop. is stable over time 

# If P-value is greater than choosen leve, the H0 fail to be rejected.
df_test <- urca::ur.df(xtstr$transnational, lags = 12, type = "drift", selectlags = "AIC")
urca::summary(df_test) #  P-critical value 5% = -2,88. Value of t = -3,28: Fail to reject the H0
urca::summary( urca::ur.df(xtstr$transnational, lags = 1, type = "drift", selectlags = "AIC") )

# Test two: KPSS
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for stationarity.
tseries::kpss.test(xtstr$transnational, lshort = F)
# lshort: a logical indicating whether the short or long version of the truncation lag parameter is used.

# The larger value kpss is, the stronger the evidance aginsst stationarity
# Low level of p-value, indicate strong evidence against stationarity
# Hypt. of stationarity significance of 5 precent cant be rejected.

# Test cant indicate a unit root.

# Q: Estimate ARMA(1,1):
# trans(t) = a + Btrans(t-1) + dz(t) + oe(t-1) + e(t)

model3 <- Arima( xtstr$transnational, order = c(1,0,1), xreg = terror$z, method = "ML")

summary(model3)

# 
modelsummary::modelsummary(list(model1, model2, model3))

# Model 3 vs. model 2
# - Terrorism is more persistent

dt_pred <- data.table::data.table( model = rep( c("model2", "model3"), times = 5 ),
        value = c(predict(model2, n.ahead = 5, nedata = xtstr$transnational["2010-10-01"], newxreg = 1 )$pred,
                  predict(model3, n.ahead = 5, nedata = xtstr$transnational["2010-10-01"], newxreg = 1 )$pred
                  )
        ) |> 
  melt( id.vars = "model") |> 
  transform( index = rep(1:10, each = 2))

# The model3 gives a higher laged effect than the AR(2) process.
dt_pred |> ggplot( aes( y = value, x = index, fill= model, color = model) ) + geom_line()

# Q: The long run effect of the policiy change
# LR-effect policy in model3
28.341-14.63  # The diff. between the mean and d


# Q: Include dummy for October 1991: The end of Soviet Union

terror[ , z2 := ifelse(date == ymd("1991-10-01"), 1, 0 )][] # dt change
xtstr <- xts( terror[,c(3, 4, 7)], terror$date )


# 1) The simple model
model_simple1 <- lm( terror, formula = transnational ~ z )
model_simple2 <- lm( terror, formula = transnational ~ z + z2)

# The Arima model with the dummy
arima_d1 <- Arima( terror$transnational, order = c(2,0,0 ), xreg = terror[,c(4)] |> as.matrix() , method = "ML")

arima_d2 <- Arima( terror$transnational, order = c(2,0,0 ), xreg = terror[,c(4,7)] |> as.matrix() , method = "ML")

modelsummary::modelsummary(
  list(
    model_simple1,
    model_simple2,
    arima_d1,
    arima_d2
  )
)


# Chow test---------------------------------------------------------------------

model_chow <- transnational ~ trans1 + trans2

terror$trans1 <- quantmod::Lag(terror$transnational, 1)
terror$trans2 <- quantmod::Lag(terror$transnational, 2)

ts_dat <- ts(as.matrix(terror[,c("transnational", "trans1", "trans2" )]), start = c(1970,1), frequency = 4)

qlr <- strucchange::Fstats( model_chow, data = ts_dat[], from  = .15, to = .85)

plot( qlr, alpha = .1) # Plotting where the Chow test F-test is highest

max(qlr$Fstats)

# Breakpoint -- where the struct. seems to happen

qlr$breakpoint

terror[qlr$breakpoint] # In 1997, april
# And this was the introction of anit-terrorism policices
