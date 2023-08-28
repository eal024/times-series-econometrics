

data <- read.csv("data/Quarterly.csv",sep=",",header=TRUE)

library(xts)
library(gets)
library(zoo) 

# Setting up time series properties for "original" sample period 
data$date_format <- as.Date(data$Date, "%d/%m/%Y")
data.xts <- xts(data[c(2, 9,14,16)], data$date_format)
data.xts$inf <- na.omit(100*(diff(log(data.xts$CPI))))
data.xts$d.inf <- na.omit(diff(data.xts$inf))
data.xts$irate <- data.xts$FFR
data.xts$gdp <- na.omit(diff(diff(log(data.xts$RGDP)))) 
data.xts$d.unemp <- na.omit(diff(data.xts$Unemp))
data.xts <- na.omit(data.xts["/2006-01-01"])

tail(data.xts); head(data.xts) # To check if end date is in 2006
#----------------------------------------------------------------------------------------------------------------
# First set up a TS model

# As far as I could tell there is no clever way to get the gets-method to include lags of covariates
# So instead I use a loop to create lagged values of unemployment. I create 8 lags

lag.matrix <- matrix(NA, ncol = 24, nrow = nrow(data.xts))



for (i in 1:8) {
  lag.matrix[,i] <-  stats::lag(data.xts$d.unemp, k=i)
  lag.matrix[,8+i] <- stats::lag(data.xts$gdp, k=i)
  lag.matrix[,16+i] <- stats::lag(data.xts$irate, k=i)
}

data.xts <- cbind.xts(data.xts, lag.matrix)

for (i in 1:8) {
  colnames(data.xts)[9+i] <- paste('unemp', sep ="", i)
  colnames(data.xts)[17+i] <- paste('gdp', sep ="", i)
  colnames(data.xts)[25+i]<- paste('irate', sep ="", i)
}


# To check if lags have been succefully created and added to data  
head(data.xts, 10)

# Set covariates to be equal to unemployment, irate, gdp and their lags
xregs2 <- xts(data.xts[,c(9:33)])
names(xregs2)
head(xregs2) # check if correct covariates have been selected

# Create GUM inflation model
infModel1 <- arx(data.xts$d.inf, mc=TRUE, ar=1:8, mxreg=xregs2)
infModel1 # To see output



# Finding the minimum model
getsminfModel1 <-getsm(infModel1, t.pval=0.1) 
getsminfModel1

#Define new matrix of covariates with regressors chosen from the gets-procedure
xregs3 <- xts(data.xts[,c(9,10, 12,13,14, 30,32)])
head(xregs3)
