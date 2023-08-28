


library(tidyverse)
library(fpp3)



# Example -----------------------------------------------------------------

# Data
# GDP per capita
gdppc <- global_economy |> 
  janitor::clean_names( ) |> 
  mutate( gdp_per_capita = gdp/population)

# Plot for a given country
gdppc |> 
  filter( country == "Norway") |> 
  autoplot( gdp_per_capita)


# Linear trend model
fit <- gdppc |> 
  model(
    fable::TSLM( gdp_per_capita ~trend() )
    )

fit[[2]][[1]] |> coef()

# Alternative model method
gdppc |> 
  filter( country == "Afghanistan") |> 
  mutate( t = 1:n()) |> 
  lm( formula = gdp_per_capita ~ t) |> 
  summary()

# Forecast table: fable
fit |> 
  head(1) |> 
  forecast( h = 3)  

# Graphing the forecast for Norway
fit |> 
  filter( country == "Norway") |> 
  forecast( h = "3 years") |> 
  autoplot( gdppc)




# Some models -------------------------------------------------------------









