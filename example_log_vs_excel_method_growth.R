

100*diff(log(FrozenJuice[,1]/FrozenJuice[,2]) )

pr <- 101:150

df <- tibble( i = 1:length(pr), p = pr, index = seq(from = 1, to = 1.1, length.out = length(pr)) )

# With logarithm
df1 <- df |> 
  mutate( 
    ln_p = log(p),
    p_dev = ln_p - lag(ln_p)
          ) |> 
  head()

# Using log
head( diff( log(df1$p) ) )


df1 |>  mutate( padj_dev = p/lag(p)-1 )



