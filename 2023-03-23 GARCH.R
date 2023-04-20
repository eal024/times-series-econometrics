
library(fGarch)
library(tidyverse)

spec <- garchSpec( model = list(ar = 0.2, alpha = c(0.2), beta = 0))
ysim1 <- garchSim(spec, n = 400)

spec <- garchSpec( model = list(ar = 0.2, alpha = c(0.8), beta = 0))
ysim2 <- garchSim(spec, n = 400)

spec <- garchSpec( model = list(ar = 0.8, alpha = c(0.2), beta = 0))
ysim3 <- garchSim(spec, n = 400)

spec <- garchSpec( model = list(ar = 0.8, alpha = c(0.8), beta = 0))
ysim4 <- garchSim(spec, n = 400)

plot(ysim1)
plot(ysim2)
plot(ysim3)
plot(ysim4)

df <- tibble( sim = c(1:4),
        ar = c(0.2, 0.2, 0.8, 0.8),
        alpha = rep( c(.2,.8),2),
        data = list(ysim1,ysim2,ysim3,ysim4)
        ) 

df |> 
  unnest(cols = data)
