

## 
library(fpp3)
library(tsibble)

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison |> 
  mutate( qrt = yearquarter(Date)) |> 
  select(-Date) |> 
  as_tsibble( key = c(State, Gender, Legal, Indigenous ),
              index = qrt
              )





