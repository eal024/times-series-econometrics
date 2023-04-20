

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(zoo)
library(urca)
library(tseries)
library(tidyverse)

ts.data.df <- read.csv("data/UsMacro_Quarterly.csv") # !!Note if you wanna use macro data you can use quantmod to download from FRED!! 

#Convert into ts.data format
ts.data <- as.ts(ts.data.df$TBillRate, frequency = 4, start = c(1947,1))

plot(ts.data)

acf(ts.data)  # Non stationary --> stable
pacf(ts.data)

acf(diff(ts.data))  # Non stationary --> stable
pacf(diff(ts.data))

# ADF-test. Diff lag lengths and fif. detmerministic terms
ur00 = ur.df(ts.data, type='none', lags = 0)
summary(ur00)
ur10 = ur.df(ts.data, type='drift', lags = 0)
summary(ur10)
ur20 = ur.df(ts.data, type='trend', lags = 0)
summary(ur20)

# Now we allow BIC to decide the number of lags
ur0BIC =ur.df(ts.data, type='none', lags=6, selectlags=c("BIC"))
summary(ur0BIC)
ur1BIC =ur.df(ts.data, type='drift', lags=6, selectlags=c("BIC"))
summary(ur1BIC)
ur2BIC =ur.df(ts.data, type='trend', lags=6, selectlags=c("BIC"))
summary(ur2BIC)

#First differences
#ADF tests with different lag lengths and different deterministic terms 
#(remember to not look at the p-values. We need the tau-value to be less than the critical value)

# Now we allow BIC to decide the number of lags
ur0BIC =ur.df(diff(ts.data), type='none', lags=6, selectlags=c("BIC"))
summary(ur0BIC)
ur1BIC =ur.df(diff(ts.data), type='drift', lags=6, selectlags=c("BIC"))
summary(ur1BIC)
ur2BIC =ur.df(diff(ts.data), type='trend', lags=6, selectlags=c("BIC"))
summary(ur2BIC)

# KPSS tests for both T-bill and diff. Here we can simply look at the p-values. Don't mind the warnings messages.
kpss.test(ts.data)
kpss.test(ts.data, null=c("Trend"))
kpss.test(diff(ts.data))
kpss.test(diff(ts.data), null=c("Trend"))


