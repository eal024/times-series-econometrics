
# From: https://www.econometrics-with-r.org/16-1-vector-autoregressions.html

library(vars)
library(tidyverse)
library(lubridate)
library(dynlm)

# How many lags? 
# Determing optimal length of lag from the information criteria e.g AIC or BIC
# Increasing the estimate error comes with cost: 
#   number of param grows quadratically to the number of variable


## Example: Theory and empirical evidence are helpfull

# growth
# Spread
df <- readr::read_csv("data/Quarterly.csv") |> 
  janitor::clean_names() |> 
  mutate( date = dmy(date)
          )

names(df)
gdp <- ts(df$rgdp, start = min(df$date), end = max(df$date), frequency = 4)

# Growth
growth_gdp <- ts(log(gdp[-1]/gdp[-length(gdp)]), start = c(1957,2), end = c(2012,4), frequency = 4)

# Spreat
# 3-month treasure bill, rate as ts object
tb3 <- ts(df$tbill, start = min(df$date), end = max(df$date), frequency = 4)

# 10-years treasure bound, rate as ts object
r10 <- ts(df$r10, start = min(df$date), end = max(df$date), frequency = 4)

tspread <- r10 - tb3



var_eq1 <- dynlm( growth_gdp ~ L(growth_gdp, 1:2) + L(tspread, 1:2))
var_eq2 <- dynlm( tspread ~ L(growth_gdp, 1:2) + L(tspread, 1:2))

coeftest(var_eq1, vcov. = sandwich )
coeftest(var_eq2, vcov. = sandwich )



