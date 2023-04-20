

# Chow test for test of structraul breaks

#
set.seed(333)

x <- seq(1, 5, length = 100)
y <- numeric(100)

y[1:50] <- 2*x[1:50]
y[51:100] <- rep(2*x[51],50)

plot( x = 1:100, y= y, "ln")

# Adding with noise
z <- rnorm(100, 0, .15)

y <- y + z


plot(x,y)

#
strucchange::sctest(y ~ x, type = "Chow", point = 3)






