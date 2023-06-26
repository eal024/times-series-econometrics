
library(fpp2)
library(tidyverse)

## Example
## US consumption expenditure

autoplot( uschange[,c("Consumption", "Income")]) +
  ylab( "% change") +
  xlab("Year")

## How does consumption changes from income?
df <- uschange |> as_tibble()

model1 <- df |> lm( formula = Consumption ~ Income )

# Consumptions and income 
summary(model1)

#
model2 <- fixest::feols( data = df, Consumption  ~ Income)


## multiple variables
uschange |> as_tibble() |> GGally::ggpairs()

# Model2: consumtion from income production and unempl.
model3 <- fixest::feols(data = df,
                        Consumption  ~ Income + Production + Unemployment + Savings
                )


# Displaying the result
modelsummary::modelsummary(list(model2, model3))



# Fit the data ------------------------------------------------------------

dates <- seq.Date( from = as.Date("1970-01-01"),
          to = as.Date("2016-10-01"), 
          by = "3 months")

dat <- tibble( 
  date = dates[1:(length(dates)-1)],
  consum = df$Consumption,
  fitted2 = fitted(model2),
  fitted3 = fitted(model3)
        )

# How does the model fit the data?
ggplot( data = dat |>
          pivot_longer(-date) |> 
          mutate( name = factor(name)), 
        aes(y = value,
            x = date, 
            fill = name,
            color = name) 
        ) +
  geom_line()


# How the models fitts the data
dat |>
  ggplot( aes(y = consum, x = fitted2)) +
  geom_point() +
  geom_abline( intercept = 0, slope = 1)

## Standard error of the regression
T_K_1 <- (nrow(df)-length(coef(model2)))-1

sum( (df$Consumption- fitted(model2))^2)/T_K_1

summary(model2)

# Checking residuals ------------------------------------------------------

checkresiduals(model2)

## Check each variable against the residals
dat2 <-tibble( index = 1:length(fitted(model3)),
        res = resid(model3),
        income = df$Income,
        saving = df$Savings
        )

# 
dat2 |> 
  pivot_longer(-c(index,res) ) |> 
  ggplot( aes(y = res, x = value, fill = factor(name),
              color = factor(name)) 
          ) +
  geom_point() +
  geom_smooth( method = "lm") +
  facet_grid( ~name, scales = "free")


# Alternativ
df$res <- resid(model3)
df$fitted <- fitted(model3)
df |> fixest::feols( res ~ Income)


# Homoscedastic -----------------------------------------------------------

df |> 
  ggplot(aes( y = res,
              x = fitted
              ) ) +
  geom_point() +
  labs( title = "fitted vs. residuals",
        subtitle = "No pattern. Seems like the residuals are homo")



# outliers--------------------------------------------------------------

model2_exl <- df |>
  arrange( desc(abs(res)) ) |> 
  mutate( index = 1:nrow(df) ) |> 
  filter( index > 5) |> 
  fixest::feols( Consumption  ~ Income)

# Detect outliers
df |> 
  ggplot( aes( y = Consumption,
               x = Income) ) +
  geom_point() +
  geom_abline( intercept = coef(model2)[1],
               slope = coef(model2)[2],
               color = "red"
               ) +
  geom_abline( intercept = coef(model2_exl)[1],
               slope = coef(model2_exl)[2],
               color = "blue"
  )


# Spurious regression -----------------------------------------------------

# None stationary data variable of time -- may lead to spurious regression

# Rice production in Guinea
guinearice
length(guinearice)
# Air trafic
ausair2 <-window(ausair, end = 2011)
length(ausair2)












  
















