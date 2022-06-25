df=read.csv("DailyDelhiClimateTrain.csv",sep=",",dec = ".") 
View(df)

tf=ts (df$meantemp,start = "2013", frequency = 365.25)

# QUESTION 3
library(tseries)
adf.test(tf)
# conclusion ADF test: p-value NRH0 >5% ==> NRH0
# la serie n'est pas stationaire

# QUESTION 4
acf(tf) # tous les lags sont significatifs, 
# on veut verifier si la serie est stationaire
# la decroissance des coefficients de correlation dans acf est lente, donc la serie n'est pas stationaire

plot(tf)


#QUESTION 5
# differencition = Xt - Xt-1
diff1 = diff(tf)
plot(diff1)

# QUESTION 6
library(tseries)
adf(diff1)
# la differencié est stationaire
# l'ordre de la differentiation est =1, donc c'est grace au coefficients 1  qu'on a changé la stationarité de la serie
# diff2 = diff(diff(tf)): d=2
# on a utilisé la differenciation pour l rendre stationnaire

# QUESTION 7
pacf(diff1)
# le model est un MA d'ordre 5

# QUESTION 8
acf(diff1) # le models est autoregrssif

# p = 3 ; p=5 ; p=2(car 1 differenciation a suffit)
# ARMA(3,5)+ la differenciation ==> ARIMA p=3

# QUESTION 9
model1 = Arima(tf, order = c(3,1,5)) # ARIMA(3,1,5)
summary(model1)

# QUESTION 10
acf(model1$residuals)
# ne sont pas significatifs

# il faut etre sur qu'on elimine la dependance

# QUESTION 11

model2 = Arima(tf, order = c(3,1,5)) # ARIMA(3,1,5)
summary(model2)

acf(model2$residuals)
AIC(model1,model2)
# AIC(model2) est plus faible que le model 1
BIC(model1,model2)
#NRH0: on prend le modele le plus simple


model3 = Arima(tf, order = c(3,1,5)) # ARIMA(3,1,5)
summary(model3)

acf(model3$residuals)
pacf(model3$residuals)

AIC(model2,model3)
# AIC(model2) est plus faible que le model 3
BIC(model2,model3)
# BIC(model3) est plis faible que le model2
lrtest(model2,model3)
# p-value> 5% NRH0
# conclusion : prendre le modele le plus simple : ARIMA51,1,1,)


model4 = Arima(tf, order = c(3,1,5)) # ARIMA(1,1,0)
summary(model4)

acf(model4$residuals)
pacf(model4$residuals)
AIC(model3,model4)
# model3 est plus performant

# RMSE(model3) = 1,61 < RMSE

# QUESTION 12

#pour avoir une serie stationnaire il faut utilise ACF et PACF, lAIC  ET BIC  DOIT ETRE Faible
# si la serie n'est pas stationnaire nos avons deux possiblité soit l'estimation(tendance,saisonalité) ou la differenciation(Yt- Yt-1)

# pour la prevision on utile beaucoup la differenciation

# EXERCICE 2

