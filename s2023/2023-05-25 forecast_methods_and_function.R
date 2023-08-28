


# Return object forecast
forecast()

# ETS algorithm are returned as defulat
forecast( ausbeer, h = 4)

# Arima example
x <- arima.sim( n = 100, model = list( ar = c(0.5)) )
model1 <- auto.arima( x)

forecast(model1, h = 5)

### Some other methods
briks <- aus_production |> filter_index("1970 Q1"  ~ "2004 Q4" ) |> select(Bricks)

aus_production |>
  filter( between(
    lubridate::yq(Quarter), ymd("1970-01-01"), ymd("2024-12-01") ) 
    )


# 
model_mean <- briks |> model(MEAN(Bricks))
model_mean$`MEAN(Bricks)`[[1]] |> coef()

# Frome the fpp2 packages
meanf( briks$Bricks )


model_lm <- briks |> lm( formula = Bricks ~1 ) |> summary()

forecast(model_mean, h = 10)















