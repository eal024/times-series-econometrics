

## Residuals digagnostics
  # Intro:
  # hat(yt)|t-1: Fitted value based on previous observations
  # et = yt - hat(yt)

# E(e) = 0, E(ejei) = 0
# And E(eiei) = constant
# Normaldistributed

# Example Google 2000

# 
df <- tibble( day = 1:length(goog200),
        stock = as.numeric(goog200)
        )

# Mean model
mean(goog200)
df |> lm( formula = stock ~ 1) |> summary()


# The naive model
goog200[length(goog200)]
naive(goog200)
model_naive <- naive(goog200)

df |> 
  mutate( lag = dplyr::lag(stock),
          res = stock-lag)

# The resiudals from the model
res <- residuals(model_naive)

autoplot(res_naive)
ggplot() +
  geom_histogram(data = tibble(res= res_naive), aes(x = res))

# Histogram of residuals: Seems normaly distributed
gghistogram(res_naive)

# Autocorrealtions plot

# Lack of significant residuals -- seems like the model is good.
ggAcf(res)


# ## Formal test of the residuals: Portmanteau test -----------------------

# Formal test of autocorrelations
# Portmanteau test: we test whether the first h autocorrelations are significantly different from what would be expected from a white noise process. 

# Ljung-Box test
# Large value of Q suggest that the autocorr. do not come from a white noise
# Chi-distribution l degree of freedom.

Box.test(res, lag = 10)
Box.test(res, lag = 10, type = "Lj") # Signi. result p-values large

#
checkresiduals(naive(goog200))














