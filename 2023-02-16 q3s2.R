
#
library(tidyverse)
library(lubridate)
library(xts)
library(dynlm)

## Terrorism, domestic and transnational
df <- readr::read_csv("data/terrorism.csv" ) |> janitor::clean_names() |> mutate( date = dmy(date))

# creating a ts object 
domastic <-  ts( df$domestic,      start = min(df$date), frequency = length( year(df$date) |> unique() ) ) 
transn   <-  ts( df$transnational, start = min(df$date), frequency = length( year(df$date) |> unique() ) )

# Domastic
diff(domastic) |> max(na.rm = T) 
diff(domastic) |> min(na.rm = T)
domastic[ diff(domastic) == max(diff(domastic), na.rm = T )]
domastic[ order(diff(domastic) )[1]-1 ]

# Transnational
# Structural breaks
plot(y = transn, x = df$date, type= "l")
plot.ts(transn[1:108])
plot.ts(transn[100:114])
plot.ts(transn[108:length(transn)])


# 4.2
acf(transn)
acf(transn[1:108]) # Geomtrics reduction. Positive
acf(transn[109:length(transn)]) # Reduces correlation faster. 
pacf(transn[1:108]) # 
pacf(transn[109:length(transn)]) # 

# Modelselection
transn[1:108] # AR(p)
transn[109:length(transn)] # MA(q)

acf(transn[1:108]) # Geomtrics reduction. Positive
acf( diff(transn[1:108])[-1] )       
arima( diff(transn[1:108]), order = c(1,0,0))       

library(forecast)
auto.arima( diff(transn[1:108]))       # Confirms the pathern from ACF
auto.arima( diff(transn[109:length(transn)]))       

# Different models
tsdiag(arima( (transn[1:108]) , order = c(1,0,0)))
tsdiag(auto.arima( diff(transn[1:108])))
tsdiag(auto.arima( diff(transn[109:length(transn)])))
       
## 4.3 Generate a dummy for the decline in transnational terror after 1997       

df$z <- ifelse( df$date > ymd("1997-12-01"),1,0)

# Estimating models
#  i) transn(t) = a + dz(t) + e(t)
# ii) transn(t) = a + d*z(t) + B1*transn(t-1) + B1transn(t-2) + e(t)

lm(df$transnational ~ df$z)

ts(df ) |> head()

model2 <- dynlm( dat = ts(df), formula = transnational ~  z)
model3 <- dynlm( dat = ts(df), formula = transnational ~  z + L(transnational,1:2))

summary(model2)
summary(model3)

# Evaluate the models
acf(model2$residuals) # The residuals seems correlated -- the Se cant be trusted
acf(model3$residuals) # White noise. Can report the se

## 4.4 What features does the autocorrelation fun have -- that can be problematic?
acf(transn)
pacf(transn)

# 4.5 ARMA (1,1)
# model: transn(t) = a + d*z(t) + B1*transn(t-1) + O(t-2) + e(t)

model_ar2  <- arima( transn, order = c(2,0,0))
model_arma <- arima( transn, order = c(1,0,1))
model3_ar1  <- arima( transn, order = c(1,0,0), xreg = ts(df$z))
model3_ar2  <- arima( transn, order = c(2,0,0), xreg = ts(df$z))
stargazer::stargazer(model_ar2,model_arma, type = "text")

acf(model_ar2$residuals)
acf(model_arma$residuals)

# Diagnostics
tsdiag(model_ar2, gof.lag = 30)
tsdiag(model_arma, gof.lag = 30)
tsdiag(model3_ar, gof.lag = 30)
tsdiag(model3_ar1, gof.lag = 30)
tsdiag(model3_ar2, gof.lag = 30)
       
# 4.6 including a sigel dummy Q1991
df$z2 <- ifelse( df$date == ymd("1994-10-01"), 1,0 )
df$z3 <- ifelse( df$date >= ymd("1994-10-01"), 1,0 )
model3_ar2_dummy  <- arima( transn, order = c(2,0,0), xreg = cbind(ts(df$z),ts(df$z2)) )

tsdiag(model3_ar2_dummy)
summary(model3_ar2_dummy)

# dynlm
model4 <- dynlm( dat = ts(df), formula = transnational ~  z+ z2 + L(transnational,1:2))
model5 <- dynlm( dat = ts(df), formula = transnational ~  z+ z2 + z3 + L(transnational,1:2))

summary(model3)
AIC(model3)
summary(model4)  # Including Z2 improves the model.
AIC(model4)
acf(model4$residuals)

summary(model5)
AIC(model5)
AIC(model4)  # The model with one control/dummy is prefered.

## 4.7 Testing the structural break, using the Chow-test
strucchange::sctest(df$transnational ~ df$date, point = 113) # p < 0.05 reject H0. that structural change equal 0

# Grafisk vist structural changes
ddf <- df |> mutate( across( .cols= where(is.numeric) , .fns = function(x) abs(x-lag(x)) )) 
df |> pivot_longer(-date) |> ggplot(aes(y = value, x = date)) +geom_line() + facet_wrap(~name, scales = "free_y")
