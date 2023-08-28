


# https://www.statology.org/kpss-test-in-r/

set.seed(100)

dat <- rnorm(100)

plot(dat, type = "l")



library(tseries)

kpss.test(dat, null = "Trend")