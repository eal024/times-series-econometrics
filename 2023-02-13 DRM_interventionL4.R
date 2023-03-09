

#
library(tidyverse)
library(forecast)

# Data
dat <- vroom::vroom("data/Crest.csv") |>
  mutate( date    = as.Date(date, "%d-%m-%Y"))

# The intervention date
intDate <- as.Date("1955-08-04") 

# Weekly data
dat$crest <-  ts(dat$Crest_share, frequency = 52, start = c(1953,1)) 

plot(dat$crest) # Can see that there is a "jump" in 1955

pre  <- dat[ dat$date <= intDate, ] # Filter the pre intervention data
post <- dat[ dat$date > intDate, ] # The post

# Plotting the pre and post crest
plot.ts( pre$crest)
plot.ts( post$crest)

## Estimation AR(1)
model_arma <- Arima( dat$crest, c(1,0,0))

# Testint the model
tsdiag( model_arma, gof.lag = 30)

# Estimating Pre and Post intervention (separately)
model_arma_pre <- Arima(pre$crest, c(1,0,0))
tsdiag( model_arma_pre, gof.lag = 30)

model_arma_post <- Arima(post$crest, c(1,0,0))
tsdiag( model_arma_post, gof.lag = 30)

## Forecasts based on the model for the pre-intervention
# How does the model fitt -- when we know the true data?
ARMA_crest_f <- forecast(model_arma_pre, 60)

plot(ARMA_crest_f)
error <- ts(post$crest)[1:60] - ARMA_crest_f$mean

# Plot the error:
plot(ARMA_crest_f)
plot(error)  # As we can see -- the error is diff from 0

# A model that capture the intervention

model_arma_incl_z <- Arima(dat$crest, order =c(1,0,0), xreg = dat$step.ADA) # xreg = 1; if intervention 

# 
model_arma
model_arma_incl_z

## Creating a plot for the two models
# 
df <- tibble( 
        crest    = dat$crest,
        no_int   = model_arma$fitted,
        with_int = model_arma_incl_z$fitted,
        index    = 1:length(crest) 
        )


graf <- ggplot(df, aes(x = index, crest)) +   geom_line( alpha = .5)

p1 <- graf +  geom_line( data= df, aes(x = index, y = no_int  ), inherit.aes = F, color = "blue", linetype = 2, alpha = .8)  + xlim(120,150)

p2 <- graf + geom_line( data= df, aes(x = index, y = with_int), inherit.aes = F, color = "orange", linetype = 2, alpha= .8)  + xlim(120,150)

p3 <- p1 + geom_line( data= df, aes(x = index, y = with_int), inherit.aes = F, color = "orange", linetype = 2)  + xlim(120,150)

# Looking at the result
p3

library(patchwork)

p1+p2













