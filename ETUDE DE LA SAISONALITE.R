# ANALYSE DE LA SAISONALITE
library(readxl)
dis1 <- read_excel("C:/Users/brody/OneDrive/Bureau/DYVINE/SEMESTRE 2/Series temporelles/TSA practice1.xlsx")

dis2 = dis1[,-1]
dis2 = as.vector(t(dis2))

View(dis2)

series = ts(dis2, start =c(1928, 1), frequency =12 )
# visualisation de la series temporelles

cycle(series)
library(forecast)
library(ggplot2)
autoplot(series) + theme_bw()
# pour les saisonalite les courbes ne sont pas regulieres contrairement a la tendance


boxplot(series~cycle(series))
# il y'a une saisonalite  car le boxplot n'a pas la meme mediane
# la saisonalité est un phenomene qui sort du lot de facon remarquable
# les mois signifiatif sont de 3,4,5,6

monthplot(series)
ggseasonplot(series)

ggsubseriesplot(series , polar = TRUE)
ggsubseriesplot(series)

# modeliser la saisonalité
t1 = time(series)
library(TSA)
 har1 =harmonic(series, m=1)
 
 model1 = lm(series~har1)
 summary(model1)
 
 har2 =harmonic(series, m=2)
 
 model2 = lm(series~har2)
 summary(model2)
 
# METHODE ANOVA
 
mois=season(series)
mois

res= lm(series~mois)
summary(res)

anova(res) 
 