
# Evaluation forecsast accuraracy
library(fpp2)
library(tseries)
library(tidyverse)
library(zoo)
library(lubridate)

# For evaluation of the accuracy using forecast models
# Need to test the model on new data

# Training and test sets --------------------------------------------------

# Functions for subsetting
# window(): usefull for extractiong a portion of the data
ausbeer
window(ausbeer, start = 1995)
subset( ausbeer, start = length(ausbeer)-4*5)

ausbeer
subset(ausbeer, quarter = 1)
subset(ausbeer, quarter = 1) |> tail()
# Tail and head


# Error: ------------------------------------------------------------------
# A forecast “error” is the difference between an observed value and its forecast. 

# Forecast error vs. residauls:

# FE: Based on test data
# res: From training model

# Scale dependent: Cant compare different units!

# RMSE or MAE for measure error


## Example.
beer2 <- window(ausbeer, start = 1992, end = c(2007,4) )
beer3 <- window(ausbeer, start = 2008)

# Models
beerfit1 <- meanf(beer2, h = 10)
beerfit2 <- rwf( beer2, h = 10)
beerfit3 <- snaive( beer2, h = 10)

# Plotting the different forecastings
autoplot(window(ausbeer , start = 1992) ) +
  autolayer(beerfit1, series = "Mean", PI = F) + 
  autolayer(beerfit2, series = "Naive", PI = F) +
  autolayer(beerfit3, series = "Seasonal naive", PI = F) +
  guides( colour = guide_legend(title = "Forecast")) -> model_fit 

# The season naive predicts best -- lowest RMSE 
accuracy(beerfit1, beer3)
accuracy(beerfit2, x = beer3)
accuracy(beerfit3, x = beer3)

accuracy(beerfit1, beer3) |> as_tibble() |> mutate( set = c("train", "test"))

fun_accuracy_df <- function(modelfit, data, navn = NULL){ 
  frame <- accuracy(modelfit, data) |>
    as_tibble() |> 
    mutate( set = c("train", "test")) |> 
    relocate( set, .before = ME)
    
    if(is.null(navn)){frame}else{ frame$navn <- navn; select(frame, navn, everything())}
  }


map2_df( list(beerfit1,beerfit2, beerfit3),
         c("mean", "naive", "seaonalnaive"),
         function(x,y) fun_accuracy_df(modelfit =  x, data = beer3, navn = y) )

model_fit



# Time series cross validations -------------------------------------------

# Simple validations: 
beer2 <- window(ausbeer, start = 1992, end = c(2007,4) )
beer3 <- window(ausbeer, start = 2008)

# A more advanced: is time series cross-validation.
# or Evaluation on a rolling forecasting origin
 
e <- tsCV(goog200 , rwf, drift = T, h = 1)

e^2 |> mean(na.rm = T) |> sqrt()


## Example time sereis cross-validationl

gafa_stock  <- tsibbledata::gafa_stock 

google_2015 <- gafa_stock |>
  filter( Symbol == "GOOG", lubridate::year(Date) >= 2015)

# Start length training set = 3, increase the size with 1 for each
google_2015_tr <-  google_2015 |> 
  tsibble::stretch_tsibble( .init = 3,
                            .step = 1 ) |>
  relocate( Date, Symbol, .id)


# Id = key indicating
google_2015_tr


# TSCV accurary
nested <- google_2015_tr |> 
  group_by(.id) |> 
  nest()

# Accuracy
nested |> mutate( models  = map(data, function(x) lm(data = x, Close ~1)  )
                  )


## 
library(tsibble)
library(tidymodels)

google_2015 |> 
  stretch_tsibble(.init = 3, .step = 1) |> 
  model( RW(Close ~ drift()))


