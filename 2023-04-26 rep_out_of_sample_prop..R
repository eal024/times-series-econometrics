


# Out of sample properites
library(xts)
library(gets)
library(zoo) 
library(lubridate)
library(forecast)

data <- read.csv("data/Quarterly.csv",sep=",",header=TRUE) |>
  janitor::clean_names() |> 
  as_tibble() |> 
  mutate( date = dmy(date)
          )

# Setting up time series properties for "original" sample period 
data.xts <- xts( data[c(2, 9,14,16)], data$date)
data.xts$inf <- na.omit(100*(diff(log(data.xts$cpi))))
data.xts$d.inf <- na.omit(diff(data.xts$inf))
data.xts$irate <- data.xts$ffr
data.xts$gdp <- na.omit(diff((diff(log(data.xts$rgdp))))) 
data.xts$d.unemp <- na.omit(diff(data.xts$unemp))
data.xts <- na.omit(data.xts["/2006-01-01"])


tail(data.xts); head(data.xts) # To check if end date is in 2006

#----------------------------------------------------------------------------------------------------------------
# First set up a TS model

# As far as I could tell there is no clever way to get the gets-method to include lags of covariates
# So instead I use a loop to create lagged values of unemployment. I create 8 lags

fun_create_lags <- function( var, lags, name = "lag" ){
  
  var_list <- list(var) # Convert the data into a list
  lag_list <- map2(var_list, 1:lags, function(x,l){ quantmod::Lag(x, k= l) })
  
  # Return data.frame with the lags
  data_fun <- as.data.frame(lag_list) |> # Convert to a data.frame
    janitor::clean_names()
  
  names(data_fun) <- paste0(name,"_",1:lags)
  data_fun
}

fun_create_many_lags <- function( list_vars, number_lags){
  
  #
  pmap( list(
    x = list_vars,
    y = names(list_vars)
  ) ,
  # Creates lags
  function(x,y) fun_create_lags(var = x, lags = number_lags, name = y) 
  ) |> 
    # Binding list to data.frame
    reduce( 
      bind_cols
    ) 
}


data.lags <- fun_create_many_lags( list_vars = list( unemp = data.xts$unemp, gdp =  data.xts$rgdp, irate = data.xts$irate),
                      number_lags = 8
                      )

data.xts1 <- as.data.frame(data.xts) |> bind_cols( as.data.frame(data.lags) )

data.xts2 <- xts(data.xts1, ymd(rownames(data.xts1)) ) 

# To check if lags have been succefully created and added to data  
head(data.xts2)

# Set covariates to be equal to unemployment, irate, gdp and their lags
xregs2 <- xts(data.xts2[,c(9:33)])

xregs2_train <- xregs2[1:150,]  #cutting out the first 150 - training set
xregs2_test <- xregs2[151:183,] # test set for xregs

y_train <- data.xts2$d.inf[1:150] # training set inflation
y_test <- data.xts2$d.inf[151:183] # test set inflation



# Create GUM inflation model
infModel1 <- arx(y_train$d.inf, mc=TRUE, ar=1:8, mxreg=xregs2_train)
infModel1 # To see output



# Use the gets-package to see if we get same choice of model
getsminfModel1 <-getsm(infModel1, t.pval=0.1) 
getsminfModel1


#Define new matrix of covariates with regressors chosen from the gets-procedure
coef_names_gets <- names( coef( getsminfModel1))
var_gets <- coef_names_gum[!str_detect(coef_names_gets, "ar|mconst")]
xregs3 <- xts( xregs2_train[ , var_gets], order.by = index(xregs2_train) )
xregs3_test <- xts( xregs2_test[ , var_gets], order.by = index(xregs2_test) )


infModel_1 <- Arima(y_train, order =c(4,0,0), xreg= xregs3) # estimating model that we have optimized using GETS (easier to predict if we create the model anew)

pred <- predict( infModel_1, newxreg = xregs3_test)$pred # Predicting with the new explanatory variables # You can use $se if you want the standard errors of the estimates

plot.ts(as.numeric(y_test), col = "blue", ylim = c(-2,2))
lines(as.numeric(pred), col = "red")

MSE <- mean((as.numeric(pred)-as.numeric(y_test))^2)
MSE



