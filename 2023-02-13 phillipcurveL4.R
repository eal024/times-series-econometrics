

# Example of a ADL-model.
# PC: Explaining unemployment rate from inflation

#
library(forecast)
library(urca)
library(strucchange)
library(lubridate)

data(PhillipsCurve)
uk <- window(PhillipsCurve, start = 1948)

# log price
ts.plot(uk[,1])

# Inflation
ts.plot(diff(uk[,1]))
ts.plot(diff(diff(uk[,1])))

# Data.frame way
df <- tibble( date = seq.Date( from = ymd("1857-01-01"), to = ymd("1987-01-01"), by = "year"),
        um    = PhillipsCurve[, 3] |> as.numeric(), # Unemployment
        logp  = PhillipsCurve[, 1] |> as.numeric(), # Log price
        wa    = PhillipsCurve[, 2] |> as.numeric()
        ) 

# 
u <- uk[,3] # unemployment
p <- uk[,1] # log price
w <- uk[,2] # log wage

# Inflation
infl <- diff(p)
dinf <- diff(infl)
du <- diff(u)
dw <- diff(w)
ddw <- diff(dw) 

# Data frame
df1 <- df |>
  mutate( 
    inf = logp - lag(logp,1), # Inflation 
    dinf = inf - lag(inf, 1), # chagen inflation 
    dum   = um - lag(um,1),
    dwa = wa - lag(wa,1)
          ) |> 
  filter( year(date) > 1947 )

# Plotting the data
# Is the data stationary?
df1 |> 
  pivot_longer(-date) |> 
  ggplot( aes(x = date, y = value)) +
  geom_line( ) + 
  facet_wrap(~name, scales = "free_y") +
  labs( title = "time-series data of inflation, unempl. and wage",
        subtitle = "Diff.(inf), diff unempl. and diff wage seem stationary")

# Scatter plot of different version of PH -- to see if we can see the inverse relationship that theory tellsus exists

plot(as.numeric(u)[-1],as.numeric(infl)) # The minus ones are where we cut the series because it is longer than the others where we have taken a differnece
plot(as.numeric(du),as.numeric(w)[-1])
plot(as.numeric(du)[-length(as.numeric(du))],as.numeric(dinfl))

summary(lm( formula = u[-1] ~ infl) ) # Cant see a relation
summary(lm(formula = df1$wa ~ df1$dum) ) # increased unemployment, increases the wage -- not as theory

df1 |> ggplot( aes(x = um, y =inf)) + geom_point() + geom_smooth( method = "lm", se = T)


plot(x = df1$um,  y = df1$inf) # unemployemnt -> decreased inflation .
abline( lm(df1$um ~ df1$inf) )
plot(y = df1$wa,  df1$dum) # increased unemployment, reduces the wage
abline( lm(df1$wa~df1$dum) )
summary(lm(df1$wa~df1$dum))

plot(df1$dum,df1$dinf) #
abline(df1$dum,df1$dinf)
# Hard to find any pathern from ploting

# Look for autocorrelation
acf(du)
acf(dw)
acf(diff(dw))
acf(dinf)

# All the series seem stationary (ex. dw)
summary( ur.df(dw, lags = 6, selectlags = "BIC"))
summary( ur.df(ddw, lags = 6, selectlags = "BIC"))
summary( ur.df(du, lags = 6, selectlags = "BIC"))
summary( ur.df(dinf, lags = 6, selectlags = "BIC"))

# All sereis identified are stationary -> can work with them


# Selection of model ------------------------------------------------------

imodel0 <- auto.arima(dinf)
imodel0

# Modeling Inflation as ADL - function of unemployment
imodel1 <- auto.arima(dinf , xreg = du[-1])
imodel1

df1 |> lm( formula = dinf ~  dum)

imodel1 <- auto.arima(dinf , xreg = du[-1])

wmodel0 <- auto.arima(ddw)
wmodel1 <- auto.arima(ddw, xreg = du[-1])

# Comparing model inflation and ADL inflation of unemployemnt
rbind(accuracy(imodel0), accuracy(imodel1)) # The ADL preform better #The bottom one is with unemployment included. This work better on most of the accuracy measures

#Comparing accuracy of the wage inflation models. The bottom one is with unemployment included. This work better on most of the accuracy measure
rbind(accuracy(wmodel0), accuracy(wmodel1)) # Same conclution


















