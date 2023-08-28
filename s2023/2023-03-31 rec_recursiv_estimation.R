

## Recursive regression
library(forecast)
library(tidyverse)

data <- vroom::vroom("data/Crest.csv") |> 
  mutate( date     = lubridate::dmy(date),
          int_date = ifelse(date == lubridate::ymd("1955-08-04"), 1, 0) 
          ) 

#
dat <- ts(data$Crest_share, frequency = 52, start = c(1953,1))

summary(crest_est <- Arima(dat, order = c(1,0,0)))

# Matrix 6 variables.
# For storing data
estimate_store <- matrix(ncol = 6, nrow = nrow(data)-20)

n_rep <- nrow(data)-20

for(i in 1:n_rep){
  model <- Arima(dat[i:(i+20)], order = c(1,0,0))
  
  estimate_store[i, ] <-
    c(model$coef[1],
      confint(model)[1, ],
      model$coef[2],
      confint(model)[1, ]
      )  
}

head(estimate_store)

crest_est$coef[1]
confint(crest_est)
1:n_rep
Arima(dat[250:(250+20)], order = c(1,0,0))

plot.ts(estimate_store[,2] |> head())

plot.ts( estimate_store[,2], col= "blue", ylim = c(-1,1.5), xlab = "time")
lines( estimate_store[,3], col= "blue")
lines( estimate_store[,1], col= "red")
lines( rep(0, length(dat)), col= "red")

plot(estimate_store[,1], x = 1:256, "ln")
as_tibble(data) |> mutate( i = 1:nrow(data)) |> View()

tibble( coef = estimate_store[,1], i = 1:256) |> View()

