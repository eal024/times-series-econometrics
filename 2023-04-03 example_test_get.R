
# GETs pacakges

library(xts)
library(gets)
library(zoo)
library(tidyverse)
library(lubridate)


dat <- read.csv("data/Quarterly.csv") |> 
  janitor::clean_names() |> 
  as_tibble() 

gdp <- na.omit(diff(diff(log(dat$rgdp)))) 

# Transform the data
dat1 <- dat |> 
  mutate( date  = dmy(date),
           inf  = 100*( log(cpi) - lag(log(cpi) )),
          dinf  =       inf - lag(inf) ,
          irate = ffr,
            gdp = c(rep(NA_real_,2), gdp),
          dunemp = unemp - lag(unemp)
          ) |> 
  select( date, ffr, cpi, unemp, rgdp, inf, dinf, irate, gdp, dunemp) |> 
  na.omit() |> 
  filter( date < ymd("2006-02-01"))

# Look at the data

# Set up the TS model -----------------------------------------------------

# 3 variabler
# 8 lags0

lag_matrix <- matrix(NA, ncol = 8*3, nrow = nrow(dat1))

for( i in 1:8){
  lag_matrix[, i] <- quantmod::Lag(dat1$dunemp,   k = i)
  lag_matrix[,8+ i] <- quantmod::Lag(dat1$gdp,    k = i)
  lag_matrix[,16 +i] <- quantmod::Lag(dat1$irate, k = i)
}

df <- as_tibble(lag_matrix)

# Give names to the lags
#  Lager navn til lags
fun_name <- function(var, lags){ map_chr(1:lags, function(x) paste0(var,"_",x))}
names(df) <- map( c("dunemp", "gdp", "irate"), \(x) fun_name(x,8) ) |> unlist()

# Joint 
df$date <- dat1$date

dat2 <- dat1 |> left_join( df, join_by(date))

# Create the xts data
names(xregs2)
names(dat2[c(11:9)])
names(dat2)
dat_xts <- xts(dat2[c(7,10:18,19:26,27:34)] , order.by = dat2$date)

dat_xregs2 <- xts( dat_xts[ ,c(2:ncol(dat_xts))])
head(dat_xregs2)
# Create GUM inflation model

# Create the first model
model2 <- arx( dat_xts$dinf, mc = T, ar = 1:8, mxreg = dat_xregs2)

#model2
#infModel1
# Finding the minimum model
get_min_model <- getsm( model2, t.pval = 0.1)
get_min_model
# Variables
coef_names <- names( coef( get_min_model))
var <- coef_names[!str_detect(coef_names, "ar")]

end_data <- xts( data2[, var], order.by = data2$date)

Arima( y = data.xts$dinf, xreg = end_data, order = c(4,0,0))
auto.arima( y = data.xts$dinf, xreg = end_data)


# Structural breaks -------------------------------------------------------









