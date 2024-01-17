
# Simulating data, for investigation

# AR
n <- 100
y  <- arima.sim( n = n, list(ar = 0.5)) + 2 # AR(1)
z  <- arima.sim( n = n, list(ar = -0.5)) + 2 # AR(1)
yy <- arima.sim( n = n, list(ar = c(0.5, 0.25))) + 2 # AR(2)
yz <- arima.sim( n = n, list(ar = c(0.5, -.5))) + 2 # AR(2)


plot.ts(y) # AR + 0.5, increased value
plot.ts(z) # AR downward values

acf(y)
acf(z) # Seems like no serial correlation. 


# Visulazie
plot.ts(yy)
acf(yy)


plot.ts(yz)
acf(yz)

## ggplot and facet
library(tidyverse)

df <- tibble( 
    order = rep(c("0.5", "-0.5", "0.5, 0.25", "0.5, -0.25"), each = n), 
    data = c( y, z, yy, yz)
    )  |> 
    mutate( index = 1:n(), .by = order)

# Line plot
ggplot(df, aes(y = data, x = index) ) + geom_line() +  facet_wrap(~order)

# ACF in a ggplot-----------------
df1 <- df |> group_nest(order) 

# The data
df1$acf <- df1$data |> map(\(x) acf(x, plot = F, lag.max = 10)[[1]]  )


