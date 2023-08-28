
library(tidyverse)

data("co2")
maunaloa <- co2

# Data, co2 over time
head(maunaloa)

# ggplot auto function
autoplot(maunaloa)

# Start and end date
tsp(co2)

head(maunaloa, 1)
tail(maunaloa, 1)

# Cycle of the time series
cycle(co2)

## date
t <- "2022-08-01"

class(t)
as.Date(t) |> class()

library(lubridate)
as_date(t)

parse_date_time("April 22, 2022", orders = "%B %d, %Y")

?strptime







