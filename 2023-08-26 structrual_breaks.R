
#
library(tidyverse)
library(forecast)

# data
data <- read.csv("data/Crest.csv", header = T, ";") |> 
  as_tibble() |> 
  mutate( date = dmy(date))


forecast::autoplot(ts(data$Crest_share, start = c(1953,1), frequency = 12))

# Intervention at:
intDate <- as.Date("1955-08-04")


## Test models before and after intervention
dat_b   <- data |> filter( date <= intDate)
dat_aft <- data |> filter( date > intDate)

# MA
auto.arima( y = ts(dat_b$Crest_share) )
auto.arima( y = ts(dat_aft$Crest_share) )

# AR
Arima( y = ts(dat_b$Crest_share), c(1,0,0) )
Arima( y = ts(dat_aft$Crest_share), c(1,0,0) )

# Adequacy of the model
armaCrest <- Arima( y = ts(data$Crest_share), c(1,0,0) )

tsdiag( armaCrest , gof.lag = 30)

# Before and after
pre.arma <- Arima( y = ts(dat_b$Crest_share), c(1,0,0) )
post.arma <- Arima( y = ts(dat_aft$Crest_share), c(1,0,0) )

tsdiag( pre.arma , gof.lag = 30)
tsdiag( post.arma , gof.lag = 30)


## Forecasting when not incorperate the intervention.
f.arma <- forecast( pre.arma, 60)

f.arma.error <- ts(dat_aft$Crest_share)[1:60] - f.arma$mean
plot( f.arma)
plot( f.arma.error) # The error for the forecast 


# Model with incorporate the intervetion. --------------------------------------

# The model
arma.crest2 <- Arima( data$Crest_share, order = c(1,0,0), xreg = data$step.ADA)

# No intervetion
arma.crest1 <- Arima( data$Crest_share, order = c(1,0,0))

df <- tibble( 
  date = data$date,
  obs = data$Crest_share,
  fitted.no.intervention = arma.crest1$fitted,
  fitted.w.intervention = arma.crest2$fitted
  )



df |> 
  pivot_longer(-date) |> 
  mutate( name= factor(name)) |> 
  ggplot( aes( y = value, x = date, fill =  name, color = name)) + 
  geom_line() +
  xlim( c("1954-12-01", "1956-01-01") |> ymd()) +
  theme( legend.position = "bottom")


broom::glance(arma.crest1)
broom::glance(arma.crest2)

















