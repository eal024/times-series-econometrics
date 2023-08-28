

#
library(tsibble)
library(tsibbledata)
library(fpp2)
library(fpp3)


# Recent production
df <- aus_production |> 
  filter( year(Quarter) >= 1995)


# Training data
df_train <-  df |> filter(year(Quarter) <= 2007)

# Different models
fit <- 
  df_train |>     
  model(
    mean = MEAN(Beer),
    naive = NAIVE(Beer),
    seasonal = SNAIVE(Beer),
    drift = RW(Beer ~ drift())
)


beer_fc <- fit |> forecast( h = 10)

# Graphical presentation
beer_fc |> 
  autoplot( 
    aus_production |>
      filter(year(Quarter) >= 1992),
    level = NULL
    )

# Seasonal model preform best.

# Relevant periods from the df, to compute measures of the models 
accuracy( beer_fc, df)

