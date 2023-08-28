

library(forecast)

# Data
dat <- read.csv("data/Crest.csv", header = T, sep = ";") 

# wrangling
dat$date <- as.Date(dat$date, format = "%d-%m-%Y")

# Intervetion: Set the interv. date
int_date <- as.Date("1955-08-04")

# ts obj
dat$crest <- ts(dat$Crest_share, frequency = 52, start = c(1953, 1))

# Ploting the data
plot(dat$crest)

# AR(1)
crest_est <- Arima(dat$crest, order = c(1,0,0))

summary(crest_est) 

# From the plot - there is a larger change in the series round 1955-1956

# Estimate model first half than second.
crest_early <- Arima(dat$crest[1:138], order = c(1,0,0))
crest_late  <- Arima(dat$crest[139:278], order = c(1,0,0))

summary(crest_early)
summary(crest_late)

##

# Rolling window estimation -----------------------------------------------

# Matrix for storing data
est_store <- matrix(ncol = 6, nrow = nrow(dat)-20)

number_models <- nrow(dat)-20

for(i in 1:number_models){
  
  est <- Arima(dat$crest[i:(i+20)], order = c(1,0,0)) # Running the model over i`th windows
  
  # Saving
  est_store[i,] <- c(est$coef[1], confint(est)[1,], est$coef[2], confint(est)[1,] )
}

est_store

plot.ts( est_store[,2], col= "blue", ylim = c(-1,1.15), ylab = "AR(1) coeff. estimate")
lines(est_store[,3], col = "blue")
lines(est_store[,1], col = "red")
lines(rep(0, times = length(est_store[,1])), col = "green")

# Clearly something drastic is happening ... but we of couse knew that. Notice that the estimate even change sign. 






