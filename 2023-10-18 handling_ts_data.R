

library(tidyverse)
library(zoo)

dat <- seq.Date(from = as.Date("2008-10-01"), length.out = 100, by = "month")

## Dates and aggregated data

# Year-montg
as.yearmon(dat)

# Year qart
yearqtr(dat)

# Sampling frquency

# Aggregation
# High -> lower resultion

library(xts) # Extending the zoo packages

maunaloa <-  datasets::co2

head(as.zoo(maunaloa))
apply.quarterly( x = as.zoo(maunaloa), FUN = mean) |> head()
apply.yearly( x = as.zoo(maunaloa), FUN = mean) |> head()

tzoo_data <- as.zoo(maunaloa)

xts::endpoints(tzoo_data, on = "year")

# Create a index from every third day
endpoints( tzoo_data, on = "month", k = 3)

pr <- endpoints( tzoo_data, on = "month", k = 3)

# max for each period
period.apply(x = tzoo_data,
             INDEX = pr,
             FUN = max
             ) |> 
  head()

# Look at the data
tzoo_data |> head(9)


# Missing values ----------------------------------------------------------






