# To investigate the usefulness og the sis function we try with another data set
# We use the US inflation data set and firstly load packages and set standard TS structure

rm(list=ls())
library(xts);
library(gets)
library(zoo) 

data <- read.csv("data/Quarterly.csv",sep=",",header=TRUE)



# Setting up time series properties for "original" sample period 
data$date_format <- as.Date(data$Date, "%d/%m/%Y")
data.xts <- xts(data[c(2, 9,14,16)], data$date_format)
data.xts$inf <- na.omit(100*(diff(log(data.xts$CPI))))
data.xts$d.inf <- na.omit(diff(data.xts$inf))
data.xts$irate <- data.xts$FFR
data.xts$gdp <- na.omit(diff((diff(log(data.xts$RGDP))))) 
data.xts$d.unemp <- na.omit(diff(data.xts$Unemp))
data.xts <- na.omit(data.xts["/2006-01-01"])
tail(data.xts); head(data.xts) # To check if end date is in 2006

#----------------------------------------------------------------------------------------------------------------
# First set up a TS model

# As far as I could tell there is no clever way to get the gets-method to include lags of covariates
# So instead I use a loop to create lagged values of unemployment. I create 8 lags

lag.matrix <- matrix(NA, ncol = 24, nrow = nrow(data.xts))

for (i in 1:8) {
  lag.matrix[,i] <- lag(data.xts$d.unemp, k=i)
  lag.matrix[,8+i] <- lag(data.xts$gdp, k=i)
  lag.matrix[,16+i] <- lag(data.xts$irate, k=i)
}


data.xts <- cbind.xts(data.xts, lag.matrix)

for (i in 1:8) {
  colnames(data.xts)[9+i] <- paste('unemp', sep ="", i)
  colnames(data.xts)[17+i] <- paste('gdp', sep ="", i)
  colnames(data.xts)[25+i]<- paste('irate', sep ="", i)
}


# To check if lags have been succefully created and added to data  
head(data.xts)

# Set covariates to be equal to unemployment, irate, gdp and their lags
xregs2 <- xts(data.xts[,c(9:33)])
head(xregs2) # check if correct covariates have been selected


xregs2_train <- xregs2[1:150,]  #cutting out the first 150 - training set
xregs2_test <- xregs2[151:183,] # test set for xregs

y_train <- data.xts$d.inf[1:150] # training set inflation
y_test <- data.xts$d.inf[151:183] # test set inflation



# Create GUM inflation model
infModel1 <- arx(y_train$d.inf, mc=TRUE, ar=1:8, mxreg=xregs2_train)
infModel1 # To see output



# Use the gets-package to see if we get same choice of model
getsminfModel1 <-getsm(infModel1, t.pval=0.1) 
getsminfModel1

#Define new matrix of covariates with regressors chosen from the gets-procedure
xregs3 <- xts(xregs2_train[,c(1,2, 13, 16, 22, 24)])
xregs3_test <- xts(xregs2_test[,c(1,2, 13, 16, 22, 24)]) # test set for xregs

head(xregs3); tail(xregs3)

infModel.mail <- Arima(y_train, order =c(3,0,0), xreg=xregs3) # estimating model that we have optimized using GETS (easier to predict if we create the model anew)

pred <- predict(infModel.mail, newxreg = xregs3_test)$pred # Predicting with the new explanatory variables # You can use $se if you want the standard errors of the estimates

plot.ts(as.numeric(y_test), col = "blue", ylim = c(-2,2))
lines(as.numeric(pred), col = "red")

MSE <- mean((as.numeric(pred)-as.numeric(y_test))^2)
MSE




