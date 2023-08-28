

library(tidyverse)

# data
data <- read.csv("data/Crest.csv", header = T, ";") |> 
  as_tibble() |> 
  mutate( date = dmy(date))


data
