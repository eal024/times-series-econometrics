
library(tidyverse)

# Generate data without a real realation (spurious regression)
#set.seed(1234)
set.seed(1234)
n <- 300
e.1 <- rnorm(n,0,1)
e.2 <- rnorm(n,0,1)
x <- arima.sim(n=n,list(order=c(0,1,0)),innov = e.1)
y <- arima.sim(n=n,list(order=c(0,1,0)),innov = e.2)

plot(x,type = "l")
# ggplot(tibble(x, i = 1:length(x)), aes(y = x, x = i)) + geom_line() + geom_smooth( method = "lm")

# Ser det ut til å være en sammenheng mellom y og x?
df <- tibble( y = y, x = x)

qplot(y = y, x = x, data = df, geom = c("line", "smooth"))
lm( data = df, y~x ) |> summary()

## We know that there should not be a relation. 
# How to test?

# Test the corr between diff
lm(diff(y) ~diff(x)) |> summary()
