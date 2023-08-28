

# DLM
library(dynlm)
library(AER)
library(zoo)

data("FrozenJuice")

length(FrozenJuice[,1])/12
dates <- seq.Date( from = ymd("1950-01-01"), length.out = 51*12, by = "months")

# Percentage change in relative orange juice prices, and days of freezing degree
dat <- as_tibble( FrozenJuice) |> 
  mutate( ret = 100*log((price/ppi)/lag(price/ppi)),
          date = dates
          ) |> 
  relocate( date , .before = price) |> 
  na.omit()

# Number of freezing days increases the change in relative orange juice prices
summary(lm( data = dat, ret ~fdd))
summary(model1 <- dynlm( ret ~fdd, data = dat))
#we can see a positive relationship between change in price of orange juice and freezing days in the past
#lets see what happens when we incluce lags of FDD

acf(model1$residuals) # Looking like withe noise

# include lags of fdd

model2 <- dynlm( ret ~L(fdd,0:18), data =  read.zoo(dat))

# looks pretty much like white noise so we can report OLS standard errors
acf(as.numeric(model2$residuals))

summary(model2)
# the coefficients of this regression are estimates of the dynamic causal effects on
# orange juice price changes for the first 18 months following an unit increase
# in the number of FDDs.
#For example a single freezing day is estimated to increase prices
# by 0.5% over the month in which the freezing day occurs
# he subequent effect on
# price in later months of a freezing degree day is less.

# intermediate multipliers (cummulative sum of short run multipliers).

summary(model3 <- dynlm( ret ~L(d(fdd),0:17) + L(fdd, 18), data =  read.zoo(dat)))

acf(model3$residuals |> as.numeric())

#we can see that the coefficients here are just the cummulative sum of the coefficients in (2.3)

cumsum(model2$coefficients[-1])
coef(model3)

# After 6 month, the cummulative effect of a freezing degree day is to increase price by 0.899 degree. 

plot(cumsum(model2$coefficients[-1]), type = "l")


## Interpretation 





