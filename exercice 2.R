df <- read.csv("C:/Users/brody/OneDrive/Bureau/DYVINE/SEMESTRE 2/Series temporelles/series temporelles/DailyDelhiClimateTrain.csv")
View(df)
tf=ts (df$meantemp,start = "2013", frequency = 365.25)
meantemp=ts (df$meantemp,frequancy = 365.25, start = 2015)


library(forecast)
autoplot(tf)

str(df)
df$date = as.date(df$date, format = "%Y-%m-%d" )
df$mois = month(df$date)

df$annee = year(df$date)

library(lubridate)
library(dplyr)
df_agg = df %>%
  group_by(mois, annee)
  summarise(avg_temp = mean(tf, na.rm = TRUE))

t2 = ts(df_agg$avg_temp, frequency = 12, start = 2013)
autoplot(t2)

autoplot(decompose(t2))

library(tseries)
adf.test(t2) # RH0, la serie est stationaire

kpss.test(t2) # p-value> 0.05 NRH0 : la serie est stationaire

