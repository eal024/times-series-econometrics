

# Generate some time series data
set.seed(123)

# Define the ARIMA model parameters
ar_order <- 2
ma_order <- 0
sd_noise <- 1
n <- 100

# Generate the ARIMA time series data
ts_data <- arima.sim(list(ar = c(0.6, 0.3)), n = n, sd = sd_noise)


# Fit an ARIMA model using auto.arima()
arima_fit <- auto.arima(ts_data)

# Extract the ARMA model coefficients
arma_coef <- coef(arima_fit)

# Construct a vector of ARMA coefficients
ar_coef <- arma_coef[grep("^ar", names(arma_coef))]
ma_coef <- arma_coef[grep("^ma", names(arma_coef))]
arma_vec <- c(arma_coef["intercept"], ar_coef, ma_coef)

# Print the vector of ARMA coefficients
print(arma_vec)
