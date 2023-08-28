
##
library(urca)
library(tseries)
library(xts)
library(zoo)
library(lubridate)

# data
rangvid_data <- readxl::read_excel("data/rangvid_data.xls") |> 
  janitor::clean_names() |> 
  mutate( date = ymd( paste0(entry, "-01-01")) ) |> 
  relocate(date , .before = entry)

# Convert to xts
xts <- xts(rangvid_data[,-c(1:2,9)], order.by = rangvid_data$date)

# Price
plot(xts$p)
lines(xts$gdpcur)

# Non stationary
acf(xts$p)
acf(xts$gdpcur)

# Test with unit root
summary( ur.df( (xts$p), selectlags = c("BIC")))
summary( ur.df(xts$gdpcur, selectlags = c("BIC")))


# Simple regression lm P and GDPCUR, than check the residuals
summary( reg_model <- lm(p ~gdpcur, data = xts) ) # Corration between vars

plot.ts(reg_model$residuals)
acf(reg_model$residuals, 70) # Slowly oscillatiry decaying behaviour

summary( EG0 <-  ur.df( reg_model$residuals, selectlags = "AIC") )
# failure to reject null of unit root: There is not enough evidance to support that the data is stastionary-

po.test( xts[, c("p", "gdpcur")] ) # 
# H0: not co-integrated
# p > 0.05: We can not reject that series are not co-integereted

















