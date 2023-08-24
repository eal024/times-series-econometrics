

# ARIMA

# The data
dat <- global_economy |> 
  filter( Code == "CAF") 

dat |> 
  autoplot( Exports ) |> 
  labs( 
    title = "CAR exports", y = "% of gdp")



dat |> gg_tsdisplay( difference(Exports), plot_type =  "partial")


# Alterantive
dat1 <- dat |> 
  mutate( d_export = c(NA_real_,diff(Exports))
          )


# ACF plot
na.omit( dat1$d_export) |> acf()
na.omit( dat1$d_export) |> pacf()


# The model
fit_models <- dat |> 
  model(
    arima210  = ARIMA( Exports ~ pdq(2,1,0)),
    arima013  = ARIMA( Exports ~ pdq(2,1,0)),
    stepwise  = ARIMA( Exports),
    search    = ARIMA( Exports,  stepwise = F)
  )


# Look how well the model performs 
fit_model_long <- fit_models |>
  pivot_longer(!Country,
               names_to = "Model",
               values_to = "Orders"
               ) 


# Fitting models
fit_model_long |> 
  glance() |> 
  arrange(AICc) |> 
  select(.model:BIC)


# Looking at the residuals: ok
fit_models |> 
  select(search) |> 
  gg_tsresiduals()



# Portmanteau tetst (K = 3)
augment(fit_models) |> 
  filter( .model == "search") |> 
  features(.innov, ljung_box, lag = 10, dof = 3)

# The p-value low -> indicating low degree of ACF  


# Plotting the forecast
fit_models |> 
  forecast(h = 5) |> 
  filter( .model == "search") |> 
  autoplot( dat)













