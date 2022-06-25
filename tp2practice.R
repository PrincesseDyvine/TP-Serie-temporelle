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


boxplot(series~cycle(series))

autoplot(decompose(series)) + theme_bw()

frequency(series)
cycle(series)

info = tsoutliers(series)
info$index # pour detecter les valeurs abberantes
info$replacements # suggestion de correction de valeurs abberantes

# estimation de la tendansce par regression lineaire simple
t=time(series) #definir la variable temps

resultat1= lm(series ~ t)
summary(resultat1) # le resultat est significatif mais de mauvaise qualité par rapport au resultat

resultat1$fitted.values

fitted(resultat1)

resultat1.fit = ts(resultat1$fitted.values, frequency = 12, start =c(1928.1))
#visualisation de la series originale et de la tendance estimée par regression lineaire simple


#plot()
#lines

autoplot(series, series="Original series", size =1.2) + autolayer(resultat1.fit, series="Lineair model", size=1.2)

# normalistion du temps
t1 = t-mean(t)

t2 = t1^2
  
resultat2 = lm(series ~ t1 + t2)
summary(resultat2)


resultat2.fit = ts (resultat2$fitted.values, frequency = 12, start = c(1928,1))

autoplot(series, series="Original series", size =1.2) + autolayer(resultat2.fit, series="Lineair model", size=1.2)

plot(resultat2$residuals, type ="l")
moving = ma(series, order = 12, centre = TRUE)
plot(series)
lines(moving, col= "red")

plot(temp - ma52, type = 'l', ylab = "detrended series")

library(tseries)
kpss.test(series)

adf.test(series)
kpss.test(resid(resultat1))
kpss.test(resid(resultat2))

ma52 = ma(series, order=52, centre = TRUE)
plot(series)
lines(ma52, col = "red")
###
t= time(series)
res = lm(series- t)
t1 = (t-min(t))/(max(t) - min(t)) # normalisation de series
t2 = t1^2

res2 = lm (series - t1 + t2)
summary(res2)

# modeliser la saisonnalite
library(TSA)
har2= harmonic(series,m = 2)
res3 = lm(series~har2)
model1= lm(series~har1)
summary(model1)

plot(series)
lines(ts(fitted(res3),start = 1928, frequency = 12), col = 2, lwd = )

res4 = lm(series ~ t1 + t2 + har2)
summary(res4)

# si la series n'est pas temporaire on elimine la tendance et la saisonalité(yt=mt+st+et)
residu4=resid(res4)
residu1 = resid(res3)
plot(residu4, type = "l")
adf.test(residu4)
kpss.test(residu4)
# la diferrence entre yt et yt-1 correspond a eliminer la tendance et la saisonalité

# fonction d'autocorealtion partielle(et=0.6*et-1) 
#on identifie une corelation(et-1)

# autrement
plot(series)
kpss.test(series)
diff_series = diff(series)
plot(diff_series)
kpss.test(diff_series)
# si la serie diff_series n'est pas stationaire , alors on dfferencie la series une autre fois

diff2 = diff(diff(series))
hist(residu4)
qqnorm(residu4)
qqline(residu4 , col = 2)

library(e1071)
skewness(residu4)
kurtosis(residu4)

# inferieur a 5 donc elle ne suit pas une loi normal


shapiro.test(residu4) # verifie la normalite
jarque.bera.test(residu4)
# inferieur a 5 donc elle ne suit pas une loi normal

cor(residu4[1:527], residu4[2:528])
a = acf(residu4)
a
# lorsque les barres depassent la lignes bleu c'est que c'est la correaltion significatif
# les erreurs sont dependantes

pacf(residu4)

# fonction d'autocorealtion partielle(et=0.6*et-1 +u) 
# on sarrete lorsqu'il ya des bruits blancs(la moyenne de l'erreur=0, variance constante, et ne depend pas dans le temp)
#on identifie une corelation(et-1)

# MARCHE ALEATOIRE: constante au cours du temps X(t)=X(t-1)+Et , ici l'erreur correspond a l'erreur 