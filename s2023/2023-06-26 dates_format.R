

#
library(tidyverse)
library(lubridate)

# parsing dates
d <-  "April 22, 2022"

parse_date_time(d, orders =  "%B %d, %Y")

?strptime

# several diff times formats
dates_vec <- c(d, "12/20/2022")

parse_date_time( dates_vec, orders = c("%B %d, %Y",
                                  "%m/%d/%Y"
                                  )
                 )
