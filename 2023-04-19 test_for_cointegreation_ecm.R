

# Example co integreation
# Does macroeconomical variables affect the stock return?

# Test for cointegration.
# 

# 
library(tidyverse)
library(lubridate)
library(xts)

rangvid_dat <- readxl::read_excel("data/rangvid_data.xls")

head(rangvid_dat)

# Transform data
rangvid_dat_1 <- rangvid_dat |> 
  mutate( date = ymd( paste0(ENTRY, "-01-01"))
          )

# Transform to xts 
xtsdata <- xts( rangvid_dat_1[ , -c(1,9)], order.by = rangvid_dat_1$date)


# Price
plot(xtsdata$P) # Non stationary
plot(xtsdata$GDPCUR) # Also non stationary

acf(xtsdata$P) # ACF shows non stationary
acf(xtsdata$GDPCUR) # ACF shows non stationary for both

# Confirm this by the ADF
urca::summary( urca::ur.df( xtsdata$P, selectlags = c("BIC")) ) # Cant reject NULL of unit root
urca::summary( urca::ur.df( xtsdata$GDPCUR, selectlags = c("BIC")) ) # Cant reject NULL of unit root

# Rgression P and GDP - Check non-stationary of residuals
reg1 <- lm( P ~ GDPCUR, data = xtsdata)

summary(reg1) # Significant

plot.ts(reg1$residuals) # Pathern

acf(reg1$residuals, plot = F) 
acf(reg1$residuals, 70) # Slow oscillatory decaying 
# The autocorraltion does not go to 0 (insignificant)

urca::summary( urca::ur.df( reg1$residuals, selectlags = c("BIC")) ) # Cant reject NULL of unit root
# The null of non-stationary can not be rejected.

urca::summary( urca::ur.df( reg1$residuals, selectlags = c("AIC")) ) # 
# Higher value than 5 pct -- the null can be rejcted


eg.test <- tseries::po.test(xtsdata[,c("P", "GDPCUR")]) # H0: not co-integrated
eg.test
# H0: not co-integrated
# eg.test # Since the p is larger than 0.05 we cannot reject the null that the series are not co-integrated. This is what we found in the manual test as well



# More of co integration -------------------------------------------------

# 4. steps procedure
# 1. Pre-test the variables for their order of integration
# 2. Estimate the LR equilibrium relation: y = B + B1Z + e
  # if co integrated: OLS -> ok
  # Test for non-stationari

# The ECM (error correction model)
# Why ECM.
# 
set.seed(1)
n <- 1000
mu <- cumsum(0.1+rnorm(n, 0, 2)) #random walk with drift

z<- ts(mu+rnorm(1000,0,1))  
y<- ts(mu*3+rnorm(1000,0,2.5))

# Test 
tseries::po.test( data.frame(y,z))
# null of non co-integration can be rejected.
# we have a co integreatied relation

ts_df <- zoo( data.frame(y,z) )

ts_df$dz <- diff( ts_df$z, na.pad = T)
ts_df$dy <- diff( ts_df$y, na.pad = T)


ts_df$dz_1 <- stats::lag( ts_df$dz, -1, na.pad = T)
ts_df$dy_1 <- stats::lag( ts_df$dy, -1, na.pad = T)

ts_df$z_1 <- stats::lag( ts_df$z, -1, na.pad = T)
ts_df$y_1 <- stats::lag( ts_df$y, -1, na.pad = T)

# Understand the long run relationship
reg <- lm( data = ts_df, y ~ 0 + z)
coef( reg )  ## Since we simulated the data, we know that 3 is the true value

# residuals
ecm <- ts_df$y_1 - reg$coefficients[1]*ts_df$z_1 # Residuals
plot(ecm)
mean(ecm, na.rm = T)

ts_df$ecm <- ecm

## Keep only observation
ts_df_1 <- na.omit(ts_df) |> head()

# 
reg_ecm <- lm(data = ts_df_1, dy ~ ecm + dy_1 + dz_1)

summary(reg_ecm)

# The opposite direction to understand wheter both variables contribute to retoring equili

reg2 <- lm( data = ts_df, z ~ y + 0)
reg2$coefficients[1] # (1/3)

ecm2 <- ts_df$z_1 - reg2$coefficients[1]*ts_df$dy_1

plot(ecm2)
mean(ecm2, na.rm = T)

model_ecm <- lm( data= na.omit(ts_df_1), dz ~ ecm + dy_1 + dz_1 )

summary(model_ecm)

































