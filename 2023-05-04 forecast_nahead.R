

#
library(quantmod)
library(forecast)

getSymbols("AAPL", from = "2015/01/01")

ts <- AAPL$AAPL.Adjusted

plot(ts)
plot(ts_d <- diff(ts))

fs <- auto.arima(ts_d)

summary(fs)
plot( forecast(fs), 100)


n.pred <- 50
preds <- rep(NA, n.pred)
test <- vector()

for(i in 1:n.pred){
  
  ts.temp <- ts_d[1:( length(ts_d)-n.pred-1+i)]
  test[i] <- ts_d[( length(ts_d)-50-1+i)]
  model.re <- auto.arima(ts.temp)
  preds[i] <- predict(model.re, 1)$pred
}


ts_d[( length(ts_d)-50-1+1)]
test

plot.ts(test)
lines(preds, col = "red")

