#############################################################################
################                                          ###################
################          PROJET DE MODELE LINEAIRE       ###################
################                                          ###################
################       Date limite 14/02/2022             ###################
################                                          ###################
################      Mor Diouf Et Mame Thiam Mbengue     ###################
################                                          ###################
#############################################################################


######################        EXERCICE2       ###############################

##### Packages utilisés
library(ggplot2)
library(plotly)
library(ggplot)
library(leaps)
library(corrplot)
library(lmtest)
library(FactoMineR)

### 1) Importons les données 

mult.ozone <- read.table("C:/Users/TerminalPc/Desktop/Stat-Eco/stat maths/Stat Gning/Projet-Reg-Mor Diouf & Mame Thiam Mbengue/ozone.txt",header=T,sep=" ")
View(mult.ozone)

# Familiarisons nous avec nos donnees 

#Dimension du dataFrame
dim(mult.ozone)

attach(mult.ozone)

#Statistique descriptive des donnees
summary(mult.ozone)

#le nombre de variables
names(mult.ozone)

#les types de nos variables
str(mult.ozone)

#Coefficient de correlation entre maxo3 et T12
cor(mult.ozone$maxO3,mult.ozone$T12)

#Une Modele avec Constante de toutes les variables
reg.multiple <- lm(maxO3~.,data=mult.ozone)
summary(reg.multiple)

# 1-Sélection des Variables En utilisant le Critère R2

# La fonction "regsubsets" nous permet de voir les variables qui sont significatives

choix<-regsubsets(maxO3~.,int=T,nbest=1,nvmax=13,method="exhaustive",really.big=T,data=mult.ozone)

summary(choix)

plot(choix,scale="r2")
# En considerant ce graphique , le meilleur modele au sens de R2 est donc
#       maxO3i = ??0 + ??1T12i + ??2Vx9i + ??3Ne9i + ??4maxO3vi + ??i

reg.multiple <- lm(maxO3~T12+Vx9+Ne9+maxO3v,data=mult.ozone)
summary(reg.multiple)


# 2-Analyse de Résidus du modele choisi

residus <- reg.multiple$residuals
par(mfrow <- c(2,2))

# Vérifions la Linéairité du modéle.
plot(reg.multiple,1)
# La "ligne rouge" affichée est un ajustement du nuage de points qui 
# utilise une méthode non-linéaire avancée, appelée régression locale

# Conclusion : Verifions si p-valeur < 0.05
library(lmtest)
raintest(reg.multiple)
# p-value = 0.9944 > 0.05 donc le modele est Lineaire

# Le QQ-plot nous donne la normalite
plot(reg.multiple,2)
abline(0,1, col="red")
# Verifions si p-valeur > 0.05
shapiro.test(residuals(reg.multiple))
# p-value = 0.01587 donc on rejette l'hypothese de Normalite.

# On vérifie si les Variances des Résidus sont égales.
plot(reg.multiple,3)
#Conclusion : Verifions si p-valeur > 0.05
library(lmtest)
bptest(reg.multiple)
# p-value = 0.07223 > 0.05 donc  V(e1) = . . . = V(en).

#Detection des valeurs anormales avec la distance de COOK. On voit que 
#les valeurs associees aux individus 20010725,20010731 et 20010824 sont anormales
plot(reg.multiple,4)
# On peut retirer les valeurs qui sont anormales dans les donnees
reg2 <- lm(maxO3~T12+Vx9+Ne9+maxO3v, subset = -c(20010725,20010731,20010824),data=mult.ozone)
summary(reg2)


mulrstud=rstudent(reg.multiple)
rstud
plot(rstud)

# 3-Étudions la colinéarité des variables explicatives du modèle choisi.

# On convertit les variables qualitatives en variables factor
pluie_fac = factor(mult.ozone$pluie, c("Pluie","Sec"))
levels(pluie_fac)=c(1,0)
#On change le type de la variable pluie en numérique
pluie <- as.numeric(pluie_fac)

vent_fac = factor(mult.ozone$vent, c("Est","Ouest","Nord","Sud"))
levels(vent_fac)=c(1,2,3,4)
#On change le type de la variable vent en numérique
vent <- as.numeric(vent_fac)

#On supprime les colonnes vent et pluie
mult.ozone=subset(mult.ozone, select=-c(vent,pluie))
View(mult.ozone)

#Ajoutons dans le data Frame les variables pluie et vent converties et de type numérique
mult.ozone=cbind(mult.ozone, pluie)
mult.ozone=cbind(mult.ozone, vent)
View(mult.ozone)
str(mult.ozone)

#Si le carré du coefficient de corrélation est supérieur au R^2, on peut soupçonner de la colinéarité.
cor(mult.ozone)^2 > summary(reg.multiple)$r.squared

# TRUE : montre que les deux variables sont colineaire
# FALSE : montre que les variables ne sont pas colineaire
##### CONCLUSION:
# On voit que les variables ne sont pas colineaires dans ce modèle


# Selection de modele avec critere R2
RegBest(y=mult.ozone[,1], x=mult.ozone[,-1], nbest = 1)



