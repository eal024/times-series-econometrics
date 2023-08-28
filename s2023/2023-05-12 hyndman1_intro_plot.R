
# Work through https://otexts.com/fpp2/time-plots.html

library(fpp2)
library(tseries)
library(tidyverse)
library(zoo)
library(lubridate)

# ts objects

vec <- c(123,39,52,110)
y <- ts( vec, start = 2012) # If the dta is yearly
y_mnd <- ts( vec, start = 2012, frequency = 12) # If the data is yearly

# Graph
head(melsyd) #Data
str(melsyd)
# The Economy class observations
forecast::autoplot( melsyd[,3]) + labs( y = names(data.frame(melsyd) )[3] )

# period  1989 and 1992 -- interesting fall in the data
# Some flutuations -- increase/decreaese
# some periods with missing data



# # Example two -----------------------------------------------------------

autoplot(a10) + labs(y = "$", x = "year", title = "Monthly sales of antidiabetic drugs in Australia.")

# Convert to df
dat_ex <- tibble(date = as.Date( time(a10), format = "%Y-%m-%d"),
                 x = a10)

# Patterns
dat_ex |> lm( format = x ~ date)
autoplot(a10) + geom_smooth( method = "lm")

# Trends
decompose(a10)
plot(decompose(a10))

# Seasonal plots ----------------------------------------------------------

# Year yaer as a line
ggseasonplot( a10, year.labels = T, year.labels.left = T) +
  labs( y = "$",
        title = "seaon plot"
        )

# Monthly diff
ggseasonplot( a10, polar = T) +
  labs( y = "$",
        title = "monthly diff pay"
  )

plot( a10)
abline( lm(a10 ~ time(a10)), col =  "red")

# Alternativs
boxplot( a10 ~ cycle(a10),xlab = "mnd", ylab = "$" )

# Subseries
dat_ex |>
  mutate( mnd = month(date) |> as.factor() ) |> 
  ggplot( aes(y = x,x = mnd, fill = mnd) ) +
  geom_boxplot()



# Scatter plots -----------------------------------------------------------

# example
dat_el <- as_tibble(elecdemand) |> janitor::clean_names()

summary( model1 <- dat_el |>  lm( formula = demand ~ temperature))

qplot(data = dat_el, temperature, demand) +
  geom_smooth( method = "lm")

# Correlation
cor(x = dat_el$demand, y = dat_el$temperature)
model1


## Scatterplot matrices
autoplot(visnights[ ,1:5])
autoplot(visnights[ ,1:5], facet = T)


hist(visnights[,1])
lines( density( visnights[,1]), col = "red")
plot( density( visnights[,1]), col = "red")
GGally::ggpairs( as_tibble(visnights[ ,1:5]))


# Lagplot
window(ausbeer, start = 1992) # Window: extract only portion of the data
gglagplot( window(ausbeer, start = 1992)) # 
# Lag 8 and lag 4 highly correlated.



# Autocorrelation ---------------------------------------------------------

# Definiation
r <- S[(y_t-mean(y) )(y_t_k-mean(y) )]/S[y - hat(y)]^2

beer2 <- window(ausbeer, start = 1992)

plot(beer2)
acf(beer2)
ggAcf(beer2) + labs( y = "Her", x = "der")
acf(beer2, plot = F)

# r4 is higher than for the other lags. This is due to the seasonal pattern in the data: 

## Trend
acf(elec |> window(start = 1980), lag = 50)
# Slow decrease in ACF -- as the laags increases is due to a trend. Scallioped shape --> season.


































