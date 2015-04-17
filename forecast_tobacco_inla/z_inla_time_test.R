
require(dplyr)

df <- data.frame(
  n_countries = 1:6,
  time = sapply(list(time1, time2, time3, time4, time5, time6), function(x) x["elapsed"])
)

df$mins <- df$time / 60
df$mins_prev <- lag(df$mins)
df$ratio <- df$mins / df$mins_prev

plot(df$n_countries, df$mins)

