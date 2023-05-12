
library(tidyverse)

# AR(2)
sim_data <- arima.sim(list(order = c(2,0,0), ar = c(0.3, 0.4) ), n = 200) + 10 

acf(sim_data)
pacf(sim_data)
arima( x = sim_data, order = c(2,0,0), include.mean = T, method = "CSS") # ARIMA result
# CSS: condtional sum of squares

y <- as.numeric(sim_data)

dat <- tibble( y = y , y_1 = lag(y,1),  y_2 = lag(y,2) )

summary( lm( y ~ 0 + ., data = dat ))

#
arima( x = sim_data, order = c(2,0,0), include.mean = T, method = "CSS") # ARIMA result
ar.ols( sim_data, order = 2, demean = F, intercept = T)




