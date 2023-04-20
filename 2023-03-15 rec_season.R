
library(forecast)

#
df

## Season and structrual breaks

# Season
library(seasonal)

# An example is a data set of the number of births per month in New York city, from January 1946 to December 1959 (originally collected by Newton)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

tsbirths <- ts(births, frequency = 12, start = c(1946,1))

# Plotting the observed data
plot(tsbirths)

ggplot2::autoplot(tsbirths)

# Efficient way of decomposing the data
ggplot2::autoplot(decompose(tsbirths))

# Finding the season effect manually
acf(tsbirths)

# Spesifying the model
model1 <- Arima( tsbirths, order = c(2,1,2), seasonal = c(1,1,1))

acf(model1$residuals) # Seems like white noise
hist(model1$residuals)

# Automatical finding the model
auto.arima( tsbirths)

# Adjusting for season
ts_sa <- seas(tsbirths)

plot(ts_sa)

ts_sa$data |> head()
tsbirths |> head()

# Model adjusted for season
model_sa <- auto.arima( ts_sa$data[,1])

boxplot(tsbirths ~ cycle(tsbirths) )
boxplot(diff(tsbirths) ~ cycle(diff(tsbirths)) ) # Since there is a trend in the data, using the diff gives a clearer pathern of season.


