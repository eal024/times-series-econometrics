

# Create lags for many variables. Return a data.frame with all the variables 
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

# y the dependent variable
# df the independent X variable, with a date variable date -- works as a index for xts object
  # The lags need to be includeded in the data
# How you split the data
# t.pval: The critiria for the gets-model
test_out_of_sample <- function( y, df , train_test_split, t.pval, include.mean){
  
  # Order the df (date needs to be first)
  df <- select(df, date, everything()) 
  data_xts <- xts( df[, c(2:ncol(df))], order.by = df$date) # Create a xts-object
  
  # # Training set
  len_train <- round(train_test_split*nrow(df),0) # Number of observation in test-set
  xreg_train<- data_xts[1:len_train, ]
  y_train   <- y[1:len_train]

  # Testing set
  xreg_test<- data_xts[(len_train+1):nrow(df), ]
  y_test   <- y[(len_train+1):nrow(df)]
  
  # The models (based on the gets packages)
  # Create GUM of the training set
  model_gum <- arx( y_train, mc = TRUE, ar=1:8, mxreg = xreg_train)

  # Use the gets-package to see if we get same choice of model
  model_gets <-getsm(model_gum, t.pval= t.pval)

  #Define new matrix of covariates with regressors chosen from the gets-procedure
  coef_names_gets <- names( coef( model_gets))
  # Drop the ar and the mconst
  var_gets <- coef_names_gets[! str_detect(coef_names_gets, "ar|mconst")]
  
  # The new set of vars in the training set as a xts object
  xregs_gets_train <- xts( xreg_train[ , var_gets], order.by = index(xreg_train) )

  # # The new set of vars in the test set as a xts object
  xregs_gets_test <- xts( xreg_test[ , var_gets], order.by = index(xreg_test) )

  # # ARMA-model based on the gets
  # # Count how many AR- the gets how. OBS!! If for ex. ar1, ar4, ar8 -> return ar1, ar2, ar3
  ar_order <- length(as.numeric(str_extract( coef_names_gets[str_detect(coef_names_gets, "^ar")], "[0-9]")))
  
  ardl_train <- Arima(y_train, order =c(ar_order,0,0), include.mean = include.mean , xreg= xregs_gets_train) # estimating model that we have optimized using GETS (easier to predict if we create the model anew)

  pred <- predict( ardl_train, newxreg = xregs_gets_test)$pred # Predicting with the new explanatory variables # You can use $se if you want the standard errors of the estimates
  # 
  pred
  
  # MSE is an importan paramter: Write about it and understand
  
}

df_example <- as.data.frame(xregs2) |> mutate( date = index(xregs2))
pred <- test_out_of_sample(y = data.xts2$d.inf, df = df_example, train_test_split = 0.7, t.pval = 0.1, include.mean = T)

## Test how the function works:
pred <- test_out_of_sample(y = data.xts2$d.inf, df = df_example, train_test_split = 0.7, t.pval = 0.01, include.mean = T)


plot.ts(as.numeric(y_test), col = "blue", ylim = c(-2,2))
lines(as.numeric(pred), col = "red")
lines(as.numeric(pred), col = "red")
lines(as.numeric(pred), col = "green")
lines(as.numeric(pred), col = "black")

## How test the model?
# Where to start?
# how splitt the training data?
# choice of restriction?


