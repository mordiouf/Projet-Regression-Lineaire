#############################################################################
################                                          ###################
################          PROJET DE MODELE LINEAIRE       ###################
################                                          ###################
################       Date limite 14/02/2022             ###################
################                                          ###################
################      Mor Diouf Et Mame Thiam Mbengue     ###################
################                                          ###################
#############################################################################

############################# Exercicec 1:  #################################

library(ggplot2)
library(plotly)
library(ggplotly)


### 1) Importons les données 
ozone <- read.table("C:/Users/TerminalPc/Desktop/Stat-Eco/stat maths/Stat Gning/Tp-Reg/ozone.txt",header=T,sep=" ")
View(ozone)
###le View nous permet de verifier visuellement si une regression linéaire est pertinent dans la mesure ou il s'agit de regarder si le nuage des points s'étire le longue d'une droite.   

# Familiarisons nous avec nos donnees 
#Dimension du dataFrame
dim(ozone)
attach(ozone)
#Statistique descriptive des donnees
summary(ozone)
#le nombre de variables
names(ozone)
#les types de nos variables
str(ozone)
ggplot(ozone) + geom_point(aes(x=T12, y=maxO3))
#Coefficient de correlation entre maxo3 et T12
cor(ozone$maxO3,ozone$T12)
boxplot(maxO3 ~ T12,
xlab = "T12", ylab = "maxO3",
col = "cornflowerblue", COL = "red", pch = 20,border = "cornflowerblue")
aggregate(maxO3 ~ T12, data = ozone, mean)
#En termes de représentation graphique, un diagramme de type boîte à moustaches permettra d'apprécier la distribution des données dans chaque groupe. La commande boxplot() repose sur l'usage de la même formule que aggregate() permettant de décrire la relation entre les deux variables.


### 2) Tracons le nuage des points et superposons la droite de regression
plot(ozone$T12, ozone$maxO3,pch = 15,main = "Nuage des points",col = "blue")
nuage = ggplot(ozone, aes(T12,maxO3 )) +  
  geom_point(col="red") +
  ggtitle("Nuage des points et la droite de regression ")+
  geom_smooth(method = "lm",se=TRUE)
nuage 
      

regression <- lm(maxO3~T12,data=ozone )
summary(regression) 


### Modele sans constante
regression.sc <- lm(maxO3~T12-1,data=ozone )
summary(regression.sc)



### 3) Vérifions par une calcul explicite que les valeurs des estimateurs béta chapeau zé
xi = ozone$T12
xi
x_barre = mean(xi)
x_barre
yi = ozone$maxO3
y_barre = mean(yi)
b_1 = (sum((xi-x_barre)*(yi-y_barre))/sum((xi-x_barre)**2))
b_1
b_0 = y_barre-b_1*x_barre
b_0




### 4) Estce-que le jeu de données contient des valeurs aberrantes retracons le nuages des points en marquants les observations aberrantes
#regression = lm(ozone$T12, ozone$maxO3)
regression <- lm(maxO3~T12,data=ozone )
plot(rstudent(regression) , col = "green",pch=19,ylab = "Résiduals",xlab = "filled Values")
abline (h=c(-2,2),col = "red")
regression
lines(lowess(rstudent(regression)))
identify (rstudent(regression))
###
### Analyse des résidus constitue une phase primordiale de la regression linéaire,ainsi on contate dans le graphique que 95 à 96% des résidus se trouvent dans l'intervalle [-2,2]c'est le cas ici puis que 4individus sur 112 sont en dehors de cet intervalle. Les individus à l'exterieur de intervalle sont des individus extremes.     

#On peut enlever les points aberrants
new.ozone = ozone[-c(17,25,34,79),]
dim(new.ozone)

### 5) Vérifions avec QQplot approprié l'hypothése gaussienne 
plot(regression,which = 2,sub="" ,main=" Le QQplot approprié l'hypothése gaussienne ", cex.id = 1.5,pch = 20, col= "red",xlab = "")
abline(0,1)

### 6) Comparons par un graphique les résidus estimés aux résidus standardisés

plot(predict(regression),rstandard(regression),ylab = "Résiduals",xlab = "filled Values",pch = 20, main = "Répresentation des différents résidus",col ="green")
identify(predict(regression),rstandard(regression))
plot(regression)
regression
outlierTest(regression)
#plot (rstandard(regression)~fitted(regression),main = " Les résidus",col = "green", ylab = "Résidual",xlab ="Résidu standard ti")
#plot (rstandard(regression)~fitted(regression),main = " Les résidus",col = "blue", ylab = "Résidu_estimés",xlab= "Résidu studentisés t*i")
plot (rstandard(regression)~fitted(regression),main = " Les résidus",col = "blue", pch = "+",ylab = " Résidu_estimés " ,xlab = "Résidu standard ti|Résidu studentisés t*i") 
#plot(rstandard(regression)~fitted(regression),pch="*",data = ozone ,xlab = " Résidu estimés",ylab = "résidu")

### 7) Est-ce-que le jeu de données contient des points lévriers et marquons les points levrier dans lnuage des points(T12,maxO3)   
#Les points leviers se sont les points qui sont trop influente dans nos jeu de donnees

n <- length(ozone$maxO3)
levier <- hatvalues(regression)
p <- regression$rank
seuil = 2*p/n
seuil2 = 3*p/n
ID <- (1:n)[levier > seuil]
plot(1:n, levier, xlab = 'index', ylab = 'MaxO3',pch=25,
     text(ID,levier[ID],ID,col = "red",pos = 2,cex=1.5))
abline(seuil,0,lty=3)
abline(seuil2,0,lty=4)
###on prend  un exemple de points levrier 101 qui répresnt le plus grand points levrier dans le graphique.Les points associés aux 11 plus grandes valeurs de la distance de cook sont numérotés,  ainsi que leurs résidus studentisés, la droite en trait plein est la droite ajusté. 

### 8) Analysons la distance cook des obervations

plot(maxO3~T12,data=ozone , pch=20)
regression=lm(maxO3~T12,data=ozone) 
cutoff = 4/length(maxO3-length(coef(regression)))
cutoff
par(mfrow=c(2,2))
plot(regression)
plot(regression, which = 4 ,col = "green",pch = 20, 
     cook.levels = cutoff,cex.id = 1.5)
abline(h = cutoff, lty = 1,col = "red")
###
library(car)
influencePlot(regression,main = "INFLUENCEPLOT",sub = "la taille des cercles est proportionnelle à la distance de cook",cex.sub = 1.5, col = "green")
influenceIndexPlot(regression) 

### 9)Ajoutons à la figure du nuage des points et à la droite de regression les intervalles de prédiction  et de confiance en tout points xi observé
ggplot(ozone, aes(y=maxO3, x=T12))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("maxO3")+
  xlab("T12") +
  theme_classic()+
  annotate("text", x = 18, y = 150, label = "maxO3 = -27.419 + 5.468 * T12\n (pval<2.2e-16)")


int_pred <- predict(regression, interval="prediction")
my_reg <-cbind(ozone, int_pred)
head(my_reg)

ggplot(my_reg, aes(y=maxO3, x=T12))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  geom_line(aes(y=lwr), color = "blue", linetype = "dashed")+
  geom_line(aes(y=upr), color = "blue", linetype = "dashed")+    
  ylab("maxO3")+
  xlab("T12") +
  theme_classic()+
  annotate("text", x = 18, y = 150, label = "maxO3 = -27.419 + 5.468 * T12\n (pval<2.2e-16)")






