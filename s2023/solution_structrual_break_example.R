data <- read.csv("data/Crest.csv", header = T, sep = ";")

data$date <- as.Date(data$date, format = "%d-%m-%Y") # designating the date as the right R format
intDate<-as.Date("1955-08-04") #  sets the intervention Date  
colnames(data)[1] <- "date" # Setting the column name
#  Plots market shares  

data$Crest <-ts(data$Crest, frequency = 52, start = c(1953,1)) # Designated as a time series object

plot(data$Crest)

# Sample for pre-intervention period 

pre <- data[data$date<=intDate,]
plot.ts(pre$Crest_share)
# Sample for post-intervention period

post<- data[data$date>=intDate,]
plot.ts(post$Crest_share)


#Estimating a simple AR(1) model
armaCrest <- Arima(data$Crest_share, c(1,0,0))
armaCrest


# Adequacy of the Model

tsdiag(armaCrest, gof.lag = 30)


#Estimating an AR(1) model before intervention
pre.armaCrest2 <- Arima(pre$Crest_share, c(1,0,0))
pre.armaCrest2
tsdiag(pre.armaCrest2, gof.lag = 30)

#Estimating an AR(1) model after intervention
post.armaCrest2 <- Arima(post$Crest_share, c(1,0,0))
post.armaCrest2
tsdiag(post.armaCrest2, gof.lag = 30)



# Forecasts based on the model for the pre-intervention period for Crest

armaCrest.f <- forecast(pre.armaCrest2, 60)
armaCrest.f.err <- ts(post$Crest_share)[1:60] - armaCrest.f$mean

plot(armaCrest.f) # Forecast from ARIMA
plot(armaCrest.f.err)



# Building a model that captures the intervention 

armaCrest2<-Arima(data$Crest, order=c(1,0,0), xreg = data$step.ADA)

armaCrest2


#### Plotting the two models
library(data.table)
df <- data.frame(Org.data=data$Crest_share, wo_int=armaCrest$fitted, w_int=armaCrest2$fitted, x=1:length(data$Crest_share)) |> as.data.table()
df.m <- data.table::melt(df,id.vars="x")

p <- ggplot(df.m, aes(x=x, y=value, color=variable)) + geom_line()
p

p2 <- ggplot(df.m, aes(x=x, y=value, color=variable)) + geom_line() + xlim(120,150)
p2

broom::glance(arma.crest)
broom::glance(armaCrest2)

