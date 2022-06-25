#w=0-6===> dim=0 lundi=1...
#W= yu 1-7 lundi=1  dimanche=7 (1er jour de la semaine)

library(lubridate)
isoweek("2020-02-24")  # semaine 9

isoweek("2022-01-01")
isoweek("2019-12-30")
puumala  <- read_dta("C:/Users/brody/OneDrive/Bureau/DYVINE/SEMESTRE 2/Series temporelles/puumala.dta")
df$date_str=as.Date 


write.csv(df,"df2.csv", row.names = F)

as.Date(2015101, "%Y%U%u")

mydatetime <- as.POSIXct("2018-01-01")