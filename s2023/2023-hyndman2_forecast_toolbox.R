
library(tidyverse)
library(tseries)
library(fpp2)
library(lubridate)

# Example data
df <- 
  readxl::read_excel("../TS_prosjekt/data/2023-02-17 uforetrygd (mars).xlsx", sheet = 1) |>
  janitor::clean_names() |> 
  select( periode, mottakere, aap, regnskap_faste )

y <- ts(df$aap, start = year(min(df$periode)), frequency = 12)  
y_utgifter_fast <- ts(df$regnskap_faste, start = year(min(df$periode)), frequency = 12)

# Simple forecasting methods -------------------------------------------

# Average method
# hat(y) = mean(y)
mean(y)
meanf(y, h = 1)
meanf(y, h = 3)

# Naive methods: Last observations is all forecasts value
y[length(y)]
naive(y)

#Because a naïve forecast is optimal when data follow a random walk (see Section 8.1), these are also called random walk forecasts


# Seasonal naive methods
# Value equal same value fast observe value from the same season,

y # Last value is Feb. next forecast is march.
# mar 2022 is 2121 
# April 2022 is 2044
snaive(y, h = 1)
snaive(y, h = 2)

# Drift methods
lm( y ~ 0 + I(1:length(y)) )
y[length(y)]
rwf(y, drift = T, h = 1)

# A line from first to last
c(y[1],y[length(y)])

(2076-285)/length(y)
2076+11*1
rwf(y, drift = T, h = 1)


# Plotting the methods
h_test <- 30
df_graph <- tibble( method = c("mean", "naive", "season_naive", "drift") |> 
                      as.factor(),
        date = list( 1:(length(y) + h_test)),
        data = list(
          pred = c(y ,meanf(y,  h = h_test)$mean),
          pred = c(y,naive(y,  h = h_test)$mean),
          pred = c(y,snaive(y, h = h_test)$mean),
          pred = c(y,rwf(y, h = h_test, drift = T)$mean)
          )
        ) |> 
  unnest( cols = c(date, data)) |> 
  mutate( type = ifelse( date < length(y), "true", "pred"))


df_graph |> 
  ggplot(aes( y = data, x = date, fill = method, color= type) ) +
  geom_line() +
  facet_wrap(~ method)




