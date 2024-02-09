

library(tidyverse)


time <- 1:10  # periods

y <- c(1, rep(0, 9)) 
z <- y
w <- y

# AR
for( i in 2:10 ){
    # AR 1  + 0.7
    y[i]  <- y[i-1]*0.7 + rnorm(n = 1, mean = 0, sd = 0)
    # AR (1) -0.7
    z[i]  <- z[i-1]*-(0.7) + rnorm(n = 1, mean = 0, sd = 0)
    # AR (1) 0.7 - 0.5
    w[i]  <- if(i == 2){ 
        w[i-1]*(0.7) + rnorm(n = 1, mean = 0, sd = 0)
        } else{
            w[i-1]*(0.7) + w[i-2]*(-0.5) +rnorm(n = 1, mean = 0, sd = 0)
        }
}



# AR theoretical dynamics 
df_theory <- tibble( 
    time = time,
    type = "theory", 
    `AR(1):0.7` = y,
    `AR(1):-0.7` = z, 
    `AR(2):0.7-0.5` = w
    )

# Longer
df_theory_longer <- df_theory |> 
    pivot_longer(
        -c(time,type)
    )  |> 
    mutate(
        name = factor(name)
    ) 


## Empirical data
ey <-  arima.sim( n = n, list(ar = 0.7)) + 0 # AR(1)
ez <-  arima.sim( n = n, list(ar = -0.7)) + 0 # AR(1)
ew <-  arima.sim( n = n, list(ar = c(0.7, -0.5))) + 0 # AR(2)

tbl_emprical_acf <- 
    list(
        ey = ey, 
        ez = ez,
        ew = ew
        ) |> 
    map(
        \(x) acf(x, plot = F)$acf |> as_tibble()  
    ) |> 
    bind_cols()

names(tbl_emprical_acf) <- c("AR(1):0.7", "AR(1):-0.7", "AR(2):0.7-0.5")


tbl_emprical_acf$time <- 1:21

df_empirical_longer <- tbl_emprical_acf |> 
    pivot_longer(
        -time
    ) |>
    mutate( 
        name = factor(name),
        type = "emprical"
    )

## Binding together
df <- bind_rows(
    df_theory_longer, df_empirical_longer
    ) |> 
    filter( time < 11)



df |> 
    ggplot(
        aes(x = time, y = value, fill = name)
    ) +
    geom_col() + 
    facet_grid( name ~type)



