getwd() #récupérer le chemin d'accès du dossier courant
setwd("C:/D/Enseignement/esb21-22/serietemporelle")
#importation du jeu de données
library(readxl)
df <- read_excel("exemple1.xlsx")
#Résumé du jeu de données
summary(df)
df$sex = as.factor(df$sex)
#la variable cd4
#la variable cd4 est une quantitative
boxplot(df$cd4)
hist(df$cd4)
summary(df$cd4)

mean(df$cd4)
mean(df$cd4, na.rm = TRUE)

#représenter le nuage de nuage de points entre cd4 et age
attach(df)
plot(age, cd4)
cor(age, cd4, use = "complete.obs")
#régression simple
res = lm(cd4 ~ age , data= df)
summary(res)


