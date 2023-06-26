
#
library(fpp2)


# The intervals which we expect y(t) to lie with a specified prop.
# 95% hat(yt+h|T) +- 1.96*sigma

# Different calculation dep. of method

naive(goog200) # sigma = sigma*(h)^0.5

autoplot(naive(goog200, h = 30))  


# Bootstrap se
naive(goog200, bootstrap = T) 

## See transforming se from transformed data



