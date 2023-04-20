
# Packages
library(xts)
library(gets)
library(zoo)

# Data
dat <- read.csv("data/Quarterly.csv", sep = ",", header = T) |>
  janitor::clean_names() |> 
  transform( date = as.Date( date, "%d/%m/%Y")
             )

# xts object
datxts <- xts( dat[c(2,9,14,16)], dat$date) 
names(datxts)

# Inflation and diff. inflation
log_cpi  <- log(datxts$cpi)
datxts$inf <- na.omit(100*diff(log_cpi))
datxts$dinf <- na.omit( diff(datxts$inf))

# Interest rate
datxts$irate <- datxts$ffr

# GDP
log_rgdp <- log(datxts$rgdp)
datxts$gdp <- na.omit( diff( diff( log_rgdp )))

# Unemployement
datxts$dunemp <- na.omit( diff(datxts$unemp))


# TS model ----------------------------------------------------------------

# Setting the TS-model
# Creating matrix to store laged covariats
lag_matrix <- matrix(NA, ncol = 24, nrow = nrow(datxts) )

# This is how laged variables are created
datxts$unemp |> head()
lag(datxts$unemp, k = 1) |> head()
lag(datxts$unemp, k = 2) |> head()

lag(datxts$unemp, k = 1) |> head()
c(NA_real_, datxts$unemp[c(1:(length(datxts$dunemp)-1) )]) |> head()
lag(datxts$unemp, k = 2) |> head()
c(rep(NA_real_,2), datxts$unemp[c(1:(length(datxts$dunemp)-2) )]) |> head()

for( i in 1:8){
  lag_matrix[, i]     <- lag(datxts$dunemp, k = i)
  lag_matrix[,8 +i ] <- lag(datxts$gdp, k = i)
  lag_matrix[,16+i]  <- lag(datxts$irate, k = i)
}

lag_matrix |> head()

datxts <- cbind.xts( datxts, lag_matrix)

for(i in 1:8){
  colnames(datxts)[9+i ] <- paste("unemp", sep = "", i)
  colnames(datxts)[17+i ] <- paste("gdp", sep = "", i)
  colnames(datxts)[25+i] <- paste("irate", sep = "", i)
}


head(datxts)

# covariats
xreg <- xts( datxts[, c(9:33)] )

head(xreg)


xreg_train <- xreg[1:150,] # Cutting first 150
xreg_test <- xreg[151:183,] # test

y_train <- datxts$dinf[1:150]
y_test <- datxts$dinf[151:183]


## Creat a GUM:General unrestriced model 
infModel1 <- arx(y_train$dinf, mc = T, ar = 1:8, mxreg = xreg_train)

# Use the gets-packges to see model selection
getsminModel1 <- getsm(infModel1, t.pval = 0.1)

## Define new matrix of covariats with regressiors
summary(getsminModel1)

var_names <- getsminModel1$mean.results |> row.names()

var_names[!grepl("ar", var_names)]
names(xreg_train)[c(1,2,13,16,22,24)]

xregs3 <- xts(xreg_train[,names(xreg_train) %in% var_names])
# xregs3 <- xts(xreg_train[,c(1,2,13,16,22,24)])
xregs3_test <- xts(xreg_test[,c(1,2,13,16,22,24)])

head(xregs3)
tail(xregs3)

# Look at how the variabl
head(
  xreg_test[,c(1,2)]
  )

# Estimating the model from using GETS
infModel1_mail <- Arima( y_train, order = c(3,0,0), xreg = xregs3)

pred <- predict(infModel1_mail, newxreg = xregs3_test )$pred


plot.ts(as.numeric(y_test), col = "blue", ylim = c(-2,2))
lines(as.numeric(pred), col = "red")

mse <- mean(( as.numeric(pred)-as.numeric(y_test))^2 )

mse


# Testing for structural breaks or outliers -------------------------------

# isat function are used

isat( datxts[,"dinf"], mc = T, ar = 1:2, mreg = xregs3, t.pval = 0.01, plot =T)

# On GUM
sis2 <- isat(datxts[, "dinf"], mc = T, mxreg = xreg, t.pval = 0.01, plot=T)

sis2$ISfinalmodels
















