#question 1
df=read.csv("DailyDelhiClimateTrain.csv",sep=",",dec = ".") 
View(df)
# question 2
tf=ts (df$meantemp,start = "2013", frequency = 365.25)
library(ggplot2)
library(forecast)

# question 3
autoplot(tf)
library(tseries)
adf.test(tf)
kpss.test(tf)

# > 5% donc la serie n'est stationnaire(p-value 0.6407> 5), on  rejette H0
# H0: la serie est stationnaire 
# H1: 
# la moyenne est constante au cour de l'etude 

kpss.test(tf) # p-value est inferieur a 5% , on rejette H0 et on garde H1( la series n'est pas stationaire)

# question 5
t=time(tf)
res1=lm(tf~t)
summary(res1)
# t est significatif
library(TSA)
har = harmonic(tf, m=5)

res2 = lm(tf~har)
summary(res2)
plot(tf)
lines(res2$fitted.values, type = "l", col=2)

lines(ts(res2$fitted.values,start = 2013,frequency = 365.25), type = "l", col=2, lwd = 2)
mod_comb = lm(tf~ t + har)
summary(mod_comb)


# QUESTION 9
residu = tf - fitted(mod_comb)

adf.test(residu)
kpss.test(residu) # p-value > 5% donc la serie est stationaires(non rejet de H0)

hist(residu)

qqnorm(residu)
qqline(residu , col = "red") # la serie ne suit pas une loi normal


skewness(residu) #asymetrique
kurtosis(residu) #aplatissement(il faut qu'il soit au alentour de 3)

shapiro.test(residu) # elle ne suit pas une loie normale 
#H0: la serie suit une loi normal
#H0: la serie ne suis pas la loi normale(p-value<5%), 
jarque.bera.test(residu)

# Q12
mean(residu)
#homoscédastique= la variance varie au cour du temps

plot(residu) # elle n'est pas variable au cours du temps
plot(residu, type = "l")



# question 13
library(lmtest)
bptest(mod_comb)
# il y a une corelation entre les variable(Et) et (Et-1)

acf(residu) # pour donner les coefficient 2 a 2,pour verifier l'aucorelation des individus
# on mesure la liason lineaire
residu[1:10]
dwtest(mod_comb)

# pour verifier la dependance c'est avec les fonction d'autocorelation
# H0: pas d'autocorelation (le modele ajuste bien les données
#H1: presence d'autocorelation
#i p-value < 0.05 => Rejet de H0

Box.test(residu, type = "Ljung") # rejet de H0
checkresiduals(mod_comb)
mean(residu)

acf(residu)
# acf: renseigne sur l'ordre de la moyenne mobile
# on utilise les premiers lag significatifs pour construire le modele de series temporelles
# dans notre cas = Et-1....Et-8 +Ut(le terme d'erreur)
# avant de faire la regression on doit d'abord faire la corelation(liaison lineaire entre deux variables)

acf(residu)


MA1= arima(residu, order = c(0,0,8))
print(MA1)
coeftest(MA1)
acf(MA1$residuals) 

# si le premier lag ne depasse pas il nya pas d'autocorelation

MA2 =arima(residu,order =c(0,0,7))
print(MA2)
coeftest(MA2)
acf(MA2$reiduals)

# pacf: autocorelation partielle
# acf: autocorelation direct
# il faut verifier les deux

MA3= arima(residu, order = c(0,0,3))
acf(MA3$residuals)

pacf(residu)
# la serie depends d'elle meme retatdée dans le temps(modele autoregressif)

AR1= arima(residu, order = c(1,0,0)) # construction d'un modle AR1 dans R
acf(MA3$residuals)

lrtest(AR1 , ARMA1)

#si les lags dde ACF sont significatifs, on dit que c'est un models MA(q)
#si les lags dde ACF sont significatifs, on dit que c'est un models AR(p)
# ceux qui donne(ARMA(p,q)
# on s'arrete lorsque le modles suit un bruit blan ou meilleur encore un bruit blanc gaussien

# il faut que le premier lag soit significatif pour qu'il est au moins une dependance
ARMA1= arima(residu, order = c(1,0,3))
ARMA1

AIC(MA3, AR1, ARMA1)

r =  auto.arima(residu)# il donne l'ordre optimal que la serie soit stationnaire ou pas
r
# si le premier lag(PACF) est significatif et negatif alors on dit que le modele est MA
#
f = forecast(residu, model=AR1, h = 113)
plot(f)
View(df)
#la partie grise c'est lintervalle de confiance'

# tp ARIMA

test = read.csv("DailyDelhiClimateTest.csv")
t1 = ts(test$meantemp, frequency = 365, start = 2017)

train = Arima(tf, order = c(1,0,0))

autoplot(forecast(train, h = 113))
autolayer(t1)

data.test <- Arima(t1, model = train)
accuracy(data.test)
# on peut ne pas valider tous les modeles

