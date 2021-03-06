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

##### Packages utilis�s
library(ggplot2)
library(plotly)
library(car)

### 1) Importons les donn�es 
ozone <- read.table("C:/Users/TerminalPc/Desktop/Stat-Eco/stat maths/Stat Gning/Tp-Reg/ozone.txt",header=T,sep=" ")
View(ozone)
### le View nous permet de verifier visuellement si une regression lin�aire
# est pertinent dans la mesure o� il s'agit de regarder si le nuage des points s'�tire le longue d'une droite.   

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

#Coefficient de correlation entre maxo3 et T12
cor(ozone$maxO3,ozone$T12)

#En termes de repr�sentation graphique, un diagramme de type bo�te 
#� moustaches permettra d'appr�cier la distribution des donn�es dans 
#chaque groupe. La commande boxplot() repose sur l'usage de la m�me formule 
#que aggregate permettant de d�crire la relation entre les deux variables.

boxplot(maxO3 ~ T12,data = ozone,
        xlab = "T12", ylab = "maxO3",
        col = "cornflowerblue", pch = 20,border = "cornflowerblue")

aggregate(maxO3 ~ T12, data = ozone, mean)

### 2) Tracons le nuage des points et superposons la droite de regression

## Nous considerons le modele maxO3 = ??0 + ??1 ??? T 12 + erreur 
regression <- lm(maxO3~T12,data=ozone)
summary(regression) 
# Utilisation de ggplot
nuage=ggplot(ozone, aes(T12,maxO3 )) +  
  geom_point(col="red") +
  geom_abline(intercept = coef(regression)[1],
              slope = coef(regression)[2])+
  ggtitle("Nuage des points et la droite de regression ")+
  geom_smooth(method = "lm",fullrange=TRUE,se=FALSE)
nuage
######### Conclusion Graphique ##########
# On constate que le nuage des points s'�tire le longue de la droite.
# Les points sont align�s et corr�les donc on donne comme conclusion  
# on rejette H0 en faveur de H1

########### Conclusion avec la commande summary #########
#La conclusion du test H0 : ??k = 0 contre H1 : ??k!=0 pour chaque k ??? {0,1} 

# On fait le test de significativite des Parametres.
# Le test de Student permet d'�valuer l'influence de T12 sur maxO3 .
# La commande 'summary' montre que le p-value = 2e-16< 0.001 
# Donc le rejet de H0 est hautement significatif.
# Par consequent l'influence de T12 sur maxO3 est "hautement significative"

### Modele sans constante 
regression.sc <- lm(maxO3~T12-1,data=ozone )
summary(regression.sc)

# On Trace sur le meme graphique la droite de regression du modele sans constante
nuage=ggplot(ozone, aes(T12-1,maxO3 )) +  
  geom_point(col="red") +
  geom_abline(slope = coef(regression.sc)[1],col="green")+
  ggtitle("Nuage des points et la droite de regression ")+
  geom_smooth(method = "lm",fullrange=TRUE,se=FALSE)
nuage

######## COMPARONS #######
# On voit que la droite de regression du modele sans constante est 
#  trop �loign� des points tandisque celle du mod�le avec constante 
#  passe sur le maximum de points.
########  Donc le mod�le avec constante est le meilleur.


### 3) V�rifions par une calcul explicite que les valeurs des estimateurs b�ta chapeau z�
xi = ozone$T12
x_barre = mean(xi)
x_barre
yi = ozone$maxO3
y_barre = mean(yi)
b_1 = (sum((xi-x_barre)*(yi-y_barre))/sum((xi-x_barre)**2))
b_1
b_0 = y_barre-b_1*x_barre
b_0

### 4) Estce-que le jeu de donn�es contient des valeurs aberrantes
#retracons le nuages des points en marquants les observations aberrantes

regression <- lm(maxO3~T12,data=ozone )
plot(rstudent(regression) , col = "green",pch=19,ylab = "R�siduals",xlab = "filled Values")
abline (h=c(-2,2),col = "red")
regression
lines(lowess(rstudent(regression)))
# On peut identifier les points aberrants en cliquant sur les points
#  puis sur 'Finish' pour voir la valeur de chaque point.
identify (rstudent(regression))

# Analyse des r�sidus constitue une phase primordiale de la regression
# lin�aire,ainsi on contate dans le graphique 96% des r�sidus se trouvent
# dans l'intervalle [-2,2]c'est le cas ici puis que 4individus sur 112 sont
# en dehors de cet intervalle. Les individus � l'exterieur de l'intervalle sont des individus extremes.     

#On peut enlever les points aberrants
new.ozone = ozone[-c(17,25,34,79),]
dim(new.ozone)

### 5) V�rifions avec QQplot appropri� l'hypoth�se gaussienne 

plot(regression,which = 2,sub="" ,main=" Le QQplot appropri� l'hypoth�se gaussienne ", cex.id = 1.5,pch = 20, col= "red",xlab = "")
abline(0,1, col="blue")
# Verifions si p-valeur > 0.05
shapiro.test(residuals(regression))
# p-value = 0.792 > 0.05 donc on accepte l'hypothese de Normalite.


### 6) Comparons par un graphique les r�sidus estim�s aux r�sidus standardis�s

#Graphique Residus Estimes
plot(residuals(regression))
abline(h=c(-2,0,2), lty=c(2,1,2))

# Graphique Residus standardises
plot(rstandard(regression))
abline(h=c(-2,0,2), lty=c(2,1,2))

# Graphique Residus standardises
plot(rstudent(regression))
abline(h=c(-2,0,2), lty=c(2,1,2))

plot (rstandard(regression)~fitted(regression),main = " Les r�sidus",
      col = "blue", pch = "+",ylab = " R�sidu_estim�s " ,xlab = "R�sidu standard ti|R�sidu studentis�s t*i") 

############  Comparaison ###################
#On voit que 95% des r�sidus sont dans l'intervalle [-2,2].
# ici, 4 individus sur 112 sont en dehors de cet intervalle, 
# C'est le cas du graphique des r�sidus standardis�s et les r�sidus studentis�s.
# Les 4 point sont appel�s points Extr�mes.
# On voit aussi qu'il y'a des points qui sont presque sur la ligne , 
#il peuvent avoir des influences n�gatives sur notre modele
# Pour les r�sidus Estim�s; la plupart des points sont a l'ext�rieur de l'intervalle [-2,2]


### 7) Est-ce-que le jeu de donn�es contient des points l�vriers et marquons les points levrier dans lnuage des points(T12,maxO3)   

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
###on prend  un exemple de points levrier 101 qui r�presente le plus grand 
#points levrier dans le graphique.Les points associ�s aux 11 plus grandes
#valeurs de la distance de cook sont num�rot�s,  ainsi que leurs r�sidus studentis�s
#la droite en trait plein est la droite ajust�. 

### 8) Analysons la distance cook des obervations

plot(maxO3~T12,data=ozone , pch=20)
regression=lm(maxO3~T12,data=ozone) 
cutoff = 4/length(maxO3-length(coef(regression)))
cutoff
plot(regression, which = 4 ,col = "green",pch = 20, 
     cook.levels = cutoff,cex.id = 1.5)
abline(h = cutoff, lty = 1,col = "red")

#Nous avons v�rifi� s'il y'a des points aberrants qui pourraient influencer
# nos r�sultats .Pour d�terminer ces points nous avons utilis� la distance
# de COOK . On voit que 3 points se d�marquent des autres(20010727,20010729,20010824)
###### On les r�tire pour voir la significativite de nos variables.
reg2 <- lm(maxO3~T12, subset = -c(20010727,20010729,20010824),data=ozone)
summary(reg2)
# Comme la suppression de ces points n'a pas chang� l'expression du mod�le
# nous avons pr�f�re de les garder car leurs influence n'est pas 
# significative.Puisque les postulats sont v�rifi�s, nous avons garder
# le mod�le maxO3 en fonction de T12


### 9)Ajoutons � la figure du nuage des points et � la droite de regression 
# les intervalles de pr�diction  et de confiance en tout points xi observ�

#Visualisation de l'intervalle de confiance avec geom_smooth
ggplot(ozone, aes(y=maxO3, x=T12))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("maxO3")+
  xlab("T12") +
  theme_classic()+
  annotate("text", x = 18, y = 150, label = "maxO3 = -27.419 + 5.468 * T12\n (pval<2.2e-16)")

# Il est encore possible d' ajouter l'intervalle de pr�diction sur le plot.
# Pour cela, il est n�cessaire, au pr�alable, de stocker les valeurs des
# bornes inf�rieures et sup�rieures de cet intervalle, calcul�es sur les
# fitted. Puis de les ajouter au tableau de donn�es.
int_pred <- predict(regression, interval="prediction")
my_reg <-cbind(ozone, int_pred)
head(my_reg)

#Visualisation de l'intervalle de prediction.
ggplot(my_reg, aes(y=maxO3, x=T12))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  # ajout de la borne inf de l'intervalle de pr�diction
  geom_line(aes(y=lwr), color = "blue", linetype = "dashed")+
  # ajout de la borne sup de l'intervalle de pr�diction
  geom_line(aes(y=upr), color = "blue", linetype = "dashed")+    
  ylab("maxO3")+
  xlab("T12") +
  theme_classic()+
  annotate("text", x = 18, y = 150, label = "maxO3 = -27.419 + 5.468 * T12\n (pval<2.2e-16)")
######## Comparons les deux intervalles. 
######## Interpr�tons la forme des bandes de ces intervalles.

# L'intervalle de pr�diction est plus large que l'intervalle de
# confiance � cause de l'incertitude suppl�mentaire li�e � la pr�diction
# d'une valeur individuelle. L'intervalle de pr�diction d�pend aussi de
# la qualit� du mod�le et de son ad�quation dans la r�gion .
#  L'intervalle de confiance admet une forme hyperbolique.

####### Interpr�tons la relation entre intervalle de confiance/pr�diction
#       avec les observations atypiques.

# L'intervalle de Confiance nous montre que la moyenne du maxO3 est susceptible 
# de se situer dans l'intervalle � 95% de certitude, mais l'intervalle 
# de confiance � 95% ne signifie pas que 95% des observations futures 
# seront � l'int�rieur de cet intervalle. 

# L'intervalle de pr�diction  est susceptible de contenir une observation 
# individuelle future � partir des valeurs des pr�dicteurs en entr�e, 
# qui sont pris en compte dans notre mod�le.
################# Conclusion  ####################
# Nous pouvons �tre s�rs � 95% que cet intervalle comprendra le maxO3
# d'une prochaine Temp�rature donn�e � midi (T12) avec des niveaux donn�s
# des pr�dicteurs en entr�e.


