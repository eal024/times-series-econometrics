
rm(list=ls())

require(forecast)

setwd("C:/Users/HP/Desktop/Work/CBS forelæsninger/Slides/2020/5. Model Selection/R section")

data <- read.csv("data/Crest.csv", header = T, sep = ";")

data$date <- as.Date(data$date, format = "%d-%m-%Y") # designating the date as the right R format
intDate<-as.Date("1955-08-04") #  sets the intervention Date  
colnames(data)[1] <- "date" # Setting the column name

data$Crest <-ts(data$Crest, frequency = 52, start = c(1953,1)) # Designate as a time series object

plot(data$Crest)

crest.est <- Arima(data$Crest, order = c(1,0,0)) # AR(1) estimation

summary(crest.est)

crest.early <- Arima(data$Crest[1:138], order = c(1,0,0)) # AR(1) estimatin for first half of the sample
crest.late <- Arima(data$Crest[139:278], order = c(1,0,0)) # AR(1) estimatin for first half of the sample

summary(crest.early); summary(crest.late) # It is clear that there is a large change in the AR1 coeffient estimate over time


### We now make a rolling window estimation ###

estimate.store <- matrix(ncol = 6, nrow = nrow(data)-20) # Creating a matrix object to store our estimates

for (i in 1:(nrow(data)-20)) {
  
  estimation.store <- Arima(data$Crest[i:(i+20)], order = c(1,0,0)) # Running the estimation in the i'th windows
  estimate.store[i,] <- c(estimation.store$coef[1], confint(estimation.store)[1,], estimation.store$coef[2], confint(estimation.store)[1,]) # Saving the estimaties and the confidence interval 
  
}

plot.ts(estimate.store[,2], col = "blue", ylim=c(-0.8, 1.1), xlab = "time", ylab = "AR(1) coefficient estimate")
lines(estimate.store[,3], col = "blue")
lines(estimate.store[,1], col = "red")
