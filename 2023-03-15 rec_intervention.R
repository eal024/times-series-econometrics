

## Intervention

library(forecast)
library(tidyverse)
library(lubridate)

# Data
df <- vroom::vroom("data/Crest.csv") |> 
  mutate( date    = dmy(date),
          intdate = as.Date("1955-08-04") # Intervention date
          )

# ts object
df$tsdat <- ts(df$Crest_share, frequency = 52, start = c(1953,1))

# Plotting the data. There seems to be a clear break in 1955
ts.plot(df$tsdat)

# Finding the break
ggplot( df,
        aes(y = Crest_share,
            x = date)
        ) + 
  geom_line( ) +
  geom_vline( xintercept = ymd("1955-08-03"), color = "red")


## One solution in to plot pre and past intervention periode
df_sub <- df |> subset( date < intdate)

df_post <- df[ df$date >= df$intdate, ]

# Modeling

# Assume that we do not model the intervention 
model1 <- Arima( df$tsdat, c(1,0,0))

model1

modelpre  <- Arima( df_sub$tsdat, c(1,0,0))
modelpost <- Arima( df_post$tsdat, c(1,0,0))

# The models differ    
modelsummary::modelsummary(
  list(
    model1 = model1,
    pre = modelpre,
    post = modelpost
    )
  )


# Forecasting -------------------------------------------------------------

pre_forecast  <- forecast(modelpre, h = 60)

error_exl_int <- ts(df_post$tsdat)[1:60] - pre_forecast$mean

# Forecasting last 60 observation
plot(pre_forecast)
plot(error) # Gives a large error

# Creating model that capture the intervention
# The intervention date is inclued as a dummy 0, or 1 (after the intervention)
model_inc_interv <- Arima( df$tsdat, order = c(1,0,0), xreg = df$step.ADA)

incl_forecast  <- forecast(model_inc_interv, h = 60)
error_inc_inter <- ts(df_post$tsdat)[1:60] - pre_forecast$mean


# Organizing the fitted model data
df_fitted <- bind_rows(   tibble(date = df$date, kat = "model without intervention",  value  = model1$fitted ),
                          tibble(date = df$date, kat = "model included intervention", value = model_inc_interv$fitted )
                        )



df_fitted |>
  ggplot( aes( x = date, y = value, fill = kat, color = kat)
          ) +
  geom_line( ) + 
  geom_line( data = tibble( date = df$date, value = df$tsdat),
             aes(y = value, x = date),
             inherit.aes = F,
             linetype = 2,
             alpha = 0.8
  ) +
  facet_wrap(~ kat) +
  theme( legend.position = "none")

#
df_fitted |> left_join( df |> select(date, obs = tsdat), join_by(date) ) |> 
  mutate( error = value-obs, .by = kat) |> 
  ggplot( aes( x = date, y = error, fill = kat, color = kat) ) +
  geom_col(  position = position_dodge2()) 



# Intervetion example 2  --------------------------------------------------





