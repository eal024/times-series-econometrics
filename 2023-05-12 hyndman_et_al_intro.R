
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
















