
# Testing structural breaks with a Chow test

#
set.seed(333)

x <- seq(from = 1, to = 5, length = 100)
y <- numeric(100)

y[1:50] <- 2*x[1:50]  # Trend 2*x (1 to 50)
y[51:100] <- rep(2*x[51],50)  # Repeat last value 50 to 100

# Error
z <- rnorm(100, 0, .15)

y <- y + z

plot(x,y)

library(strucchange)

sctest(y ~ x, type = "Chow", point = 3) #F>3 p-value < 0.05 = Rejection => finds a structural break


