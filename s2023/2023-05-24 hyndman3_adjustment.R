

## Variation from number of days 

# example diff. number in each months

# Data: Milk production
milk
monthdays(milk) 
milk/monthdays(milk) 

cbind( monthly = milk,
      dailyaverage = milk/monthdays(milk)
      ) |> 
  autoplot( facet = T)



## Population or per capita adjustment
## inflation adjustment
## Power or log transformatino
  # Box-Cox transformation

