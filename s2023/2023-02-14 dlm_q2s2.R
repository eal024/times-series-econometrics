

# packages
library(AER)
library(tidyverse)
library(lubridate)

data("FrozenJuice")
dat <- FrozenJuice

Sys.setlocale("LC_CTYPE")
skimr::skim(dat) # Price organe juice, Price index, "ffd freezing degree days within month

# po: Percent change in relative orange junice prices
# ffs: number of freezing days within month
date_seqv <- seq.Date(from = ymd("1950-01-01"), to = ymd("2000-12-01"), by = "month")

# The last price-adjuster
index_last <- dat[,2][length(dat[,2])]

df <- tibble( date   = date_seqv,
              p      = as.numeric(dat[,1]),
              index  = as.numeric(dat[,2]),
              ffd    = as.numeric(dat[,3]),
              adjP   = p*(index/index_last),
              lagffd = lag(ffd) 
              ) |>
  # Percentage change orange juice, fixst prices
  mutate( np  = (adjP/lag(adjP))-1,
          op  = (p/lag(p))-1,
          )

summary( lm(dat = df, op ~ ffd) )

df_zoo <- cbind( zoo::read.zoo(df),
                 stats::lag(df_zoo$ffd, seq(from = -12, to = -1 , by = 1) )
                 ) 

# Change some names
names(df_zoo) <- map_chr(names(df_zoo), function(x) str_replace(x,"-","_") )


library(dynlm)
summary( dynlm( data = df_zoo, op ~ ffd ) )
summary( dynlm( data = df_zoo, op ~ L(ffd, 0:18)) )

## The intermediate multiplier










