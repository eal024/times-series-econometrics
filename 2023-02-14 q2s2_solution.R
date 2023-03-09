

# Distribution lag models DLM

# Change in the orange juice prices by cold weather

library(AER)
library(dynlm)
library(tidyverse)
# Data
data("FrozenJuice")

df <- as_tibble(FrozenJuice)

# Create a new dataset. percetage change in realtive price
df1 <- df |> 
  mutate( ret   = log(price/ppi),
          ret   = 100*(ret - lag(ret) ),
          ) 

# The ts pacakges
fj <- ts.union(fdd = FrozenJuice[,3],
         ret = 100*diff(log(FrozenJuice[,1]/FrozenJuice[,2]) )
         )


## Test if the variable is stable 
fm_dyn  <- dynlm(ret ~ fdd, dat = fj)
fm_dyn2 <- dynlm(data = df1, ret ~fdd)

acf(fm_dyn$residuals) # Look like white noise -- we can report the OLS se
acf(fm_dyn2$residuals)

# Reporting the SE, with OLS
summary(fm_dyn)

# Positive relation between nr of days freezing and change in price.
# When its colder, the price goes up

## Including lags
fm_dyn_wlag <- dynlm( ret ~ L(fdd,0:18), data = fj)

acf(fm_dyn_wlag$residuals) # Also with white noise.

# Can report OLS SE
summary(fm_dyn_wlag)

# The result:
# The coeffiseints are estimates of the dynamic casual effects on the juice price change
# for the first 18 months by a 1 increase in freecing days in mnd (fdd)
# -- A single freezing day is estimated to increase price by 0.5% over the mnd.the FDD days accur.
# The subseq. effct on price in later month of a freezing day is less.
# This can be seen from the laged variables -small and no sign.

## Report te intermeidate multipliers
fm2 <- dynlm( ret ~L(d(fdd), 0:17) + L(fdd,18), dat = fj)

acf(fm2$residuals) # Seems like white noise
# Report OLS

summary(fm2)

# The coeff. is the cummulative sum of model fm_dyn_wlag

summary(fm_dyn_wlag)
coef <- map_dbl(1:18,function(x) fm_dyn_wlag$coefficients[x])

result_intermediate_multipliers <- 
  tibble( i = 1:length(coef[-1]), c = coef[-1] ) |> 
  mutate( cumsum = cumsum(c),
          alt = fm2$coefficients[2:18]
          )

# Confirm the result
summary(fm2)
fm2$coefficients[-1]

# Interpreting the result:
# After 7 month the cumulative effect of a freezing day is 0.9%
# Later, the effect reduces.
plot(result_intermediate_multipliers$alt, type = "l",
     xlab = "Lag in month",
     ylab = "Intermediate multiplier"
     )











