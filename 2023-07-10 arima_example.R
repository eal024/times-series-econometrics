
library(tidyverse)
library(fpp3)

# Data
dat <- us_employment |> 
  janitor::clean_names() |> 
  filter( (title == "Leisure and Hospitality"),
          year(month) > 2000
  ) |> 
  mutate( employed = employed/1000) |> 
  select(month, employed)

# The data is non-stationary, with strong seasonality and
# non linearity trend.
dat |> autoplot( employed)


# Strong non-stationarity 
dat |> 
  gg_tsdisplay( employed,
                plot_type =  "partial", lag = 36
  )


# With seasonal diff
dat |> 
  gg_tsdisplay( difference(employed,12),
                plot_type =  "partial", lag = 36
  )

# With seasonal diff and first diff
dat |> 
  gg_tsdisplay( difference(employed,12) |> difference(),
                plot_type =  "partial", lag = 36
  )


# Seaonal and 1.diff numbers
dat1 <- dat |> 
  mutate( diff_employed = difference(employed, 12) |> difference()
  )

# Fitted models
# unitroot_nsdiffs(): Determine D: Number of seasonal differences
# unitroot_ndiffs(): Determine d: Number of ordinary differences to use 

fit <- dat |>
  model(
    arima012011 = ARIMA(employed ~pdq(0,1,2) + PDQ(0,1,1) ),
    arima210011 = ARIMA(employed ~pdq(2,1,0) + PDQ(0,1,1) ),
    auto = ARIMA(employed, stepwise = F, approx = F )
  )


fit |> 
  pivot_longer( everything(),
                names_to = "Model name",
                values_to = "orders"
  )

# AICc
glance(fit) |> arrange(AICc) |> select(.model:BIC)


augment(fit) |> 
  filter(.model == "auto") |> 
  features( .innov, ljung_box, lag = 24, dof = 4)

# Degree of free = 4: Matches the number of paramter in the model
# Large p-value confirm that ressiduals are equal white noise


# Forecast the model

forecast(fit, h = 36) |> 
  filter(.model == "auto") |> 
  autoplot(dat)

# model
fit
fit$auto[[1]][[1]]$par
fit$auto[[1]][[1]]$model

# Reproduction of the fpp3 --- function 
ts_dat_diff <- na.omit(difference(difference(dat$employed, lag = 12)) )

#
Arima( y = ts_dat_diff, 
       order = c(2,0,0),
       seasonal = list( order = c(1,0,1), 
                        periode = 12
                        ),
       include.mean = F
)
       

# When model is selected -- time to forecast ------------------------------

forecast(fit, h = 36) |>
  filter( .model == "auto")|>
  autoplot( dat) + 
  labs( title = "us empl", y = "number of people (mill)")


# -------------------------------------------------------------------------



# training data 1991 to 2006
dat2 <- us_employment |> 
  janitor::clean_names() |>
  filter( title == "Leisure and Hospitality"
          ) |> 
  mutate( employed = employed/1000) |> 
  select(month, employed) 

# Splitting data into test and training
train <- dat2 |> 
  filter(between(
    as.Date(month), ymd("1991-06-01"),ymd("2006-05-01")
     )
    )

test <- dat2 |> 
  filter(between(
    as.Date(month), ymd("2006-06-01"),ymd("2008-07-01")
  )
  )


# Fitting models
fit <- train |>
  model(
    arima012011 = ARIMA(employed ~pdq(0,1,2) + PDQ(0,1,1) ),
    arima210011 = ARIMA(employed ~pdq(2,1,0) + PDQ(0,1,1) ),
    auto = ARIMA(employed, stepwise = F, approx = F ),
    auto2 = ARIMA(employed, stepwise = F, approx = F )
  )


fit |> 
  pivot_longer( everything(),
                names_to = "Model name",
                values_to = "orders"
  )

# AICc
glance(fit) |> arrange(AICc) |> select(.model:BIC)


## With forecast

forecast(fit, h = 36) |>
  filter( .model == "auto")|>
  autoplot() +
  geom_line( data = dat2 |>
               filter( 
                 between(
                   as.Date(month), ymd("2002-06-01"),ymd("2009-07-01")
                   )
                 ),
             aes( x = month, y = employed),
             inherit.aes = F,
             linetype = 2,
             color= "red"
             ) + 
  labs( title = "us empl", y = "number of people (mill)")


  










