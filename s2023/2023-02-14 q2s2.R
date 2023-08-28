
# S2. 2 Spurious regressions

# Generate Random walk
set.seed(123)
obs <- 300
rw1 <- c(cumsum(rnorm(n = obs, mean = 0, sd = 1)))  # y(t) = y(t-1) + e  e~N(0,1)
rw2 <- c(cumsum(rnorm(n = obs, mean = 0, sd = 1)))  # y(t) = y(t-1) + e  e~N(0,1)

# Plotting the data
plot( rw1, type = "l")
plot( rw2, type = "l")

# Regress the two relation: There seems to be a relation, but we know it is spurious
model_spurious <- lm( formula = rw1 ~ rw2)
summary(model_spurious)

#
library(tidyverse)

tibble( rw1 = rw1, rw2 = rw2 ) |> 
  ggplot( aes(y = rw1, x = rw2) ) +
  geom_point( ) +
  geom_abline( intercept = coef(model_spurious)[1],
               slope = coef(model_spurious)[2],
               color = "red",
               size = 1.5
               )

## 2.3 Testing for a spurious regression
# Testing the relation between the difference.
# If there is no relation, there is an indication of no relation.
summary( lm( formula = diff(rw1) ~ diff(rw2)) )






