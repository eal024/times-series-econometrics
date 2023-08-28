

# 
library(forecast)
library(tidyverse)

# Data
dat <- vroom::vroom(here::here("data/Crest.csv")) |>
  mutate( date    = as.Date(date, "%d-%m-%Y"))

# The intervention date
intDate <- as.Date("1955-08-04") 

# Weekly data
dat$crest <-  ts(dat$Crest_share, frequency = 52, start = c(1953,1)) 