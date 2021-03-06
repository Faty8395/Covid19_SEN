## Importation de la base
setwd("C:/Users/EL HADJI NDAO/Desktop/Projet_S�ries_Temporelles")
read.csv2("Covid19_SEN.csv", sep = ";", header = TRUE)
base = read.csv2("Covid19_SEN.csv", sep=";", header = TRUE)
View(base)
   
## Dimension de la base
dim(base)
## Type de colonnes
str(base)
   
## Transformer les colonnes
# date est consid�r�e comme factor
base$ID <- as.factor(base$ID)
base$date <- as.Date(base$date)
base$TestRealise <- as.numeric(base$TestRealise)
base$NbreCasPositif <- as.numeric(base$NbreCasPositif)
base$TestNegatif <- as.numeric(base$TestNegatif)
base$CasImportes <- as.numeric(base$CasImportes)
base$CasContacts <- as.numeric(base$CasContacts)
base$CasCom <- as.numeric(base$CasCom)
base$NbreGueris <- as.numeric(base$NbreGueris)
base$NbreDeces <- as.numeric(base$NbreDeces)
   
## v�rification des colonnes : type de colonnes
str(base)
   ## D�crire la mani�re avec laquelle la date est exprim�e
as.Date(x = "02/03/2020", format = "%d/%m/%Y")

## Comme on va visualiser une s�rie temporelle, on doit trier en fonction de la date
base$date=as.Date(base$date,"%d/%m/%Y")
base=base[order(base$date),]

## Visualisation graphique la s�rie des cas au covid 19
plot(base$date,base$NbreCasPositif,type="l")
plot(base$date,base$CasImportes,type="l")
plot(base$date,base$CasContacts,type="l")
plot(base$date,base$CasCom,type="l")
plot(base$date,base$NbreGueris,type="l")
plot(base$date,base$NbreDeces,type="l")

## Si on travaille sur la s�rie hebdomadaire : cr�er la s�rie temporelle NbreCasPositif au Covid 19 au S�n�gal
hebdo=ts(base$NbreCasPositif,start=c(2020,3), frequency=52)
hebdo
plot.ts(hebdo,xlab="date",ylab="NbreCasPositif")
## Logarithme de la s�rie nbre de cas positif : pour stabiliser la s�rie
plot(log(hebdo),xlab="date",ylab="NbreCasPositif")

## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
## Etudier la liaison entre la variable (Y) nombre de cas positif
# au covid 19 et la variable (x) test r�alis� : �chantillon test� au covid 19 par jour
# Donn�es de l'exemple : le nombre cas positif au covid 19 par jour au S�n�gal
# L'�chantillon les 33 communiques au covid 19 = individus
# Un appercu des donn�es
summary(base)

# Les corr�lations : relations entre NreCasPositif et TestRealise
# Le nuage de points entre les deux variables : visualisation graphique
# Test : la corr�lation de Pearson (param�trique)
# La corr�lation de Sparman (non param�trique)
# V�rifier la normalit�
par(mfrow = c(3,4))
barplot(table(base$NbreCasPositif))
barplot(table(base$TestRealise, 14:21)-1)

## Corr�lation de Pearson
cor.test(base$NbreCasPositif,base$TestRealise)

# NB pas de loi normale, alors corr�lation de Spearman
cor.test(base$NbreCasPositif,base$TestRealise, method = "Spearman")
plot(base$NbreCasPositif,base$TestRealise)

library(ggplot2)
ggplot(base, aes(x = TestNegatif, y = NbreCasPositif)) + geom_point()
ggplot(base, aes(x = TestRealise, y = NbreCasPositif)) + geom_point(colour="dodgerblue", alpha=.8)

## liaison entre les deux variables en fonction des cas
ggplot(base, aes(x = TestRealise, y = NbreCasPositif)) + geom_point(alpha=.8, aes(colour = NbreCasPositif, size = CasImportes))
ggplot(base, aes(x = TestRealise, y = NbreCasPositif)) + geom_point(alpha=.8, aes(colour = NbreCasPositif, size = CasContacts))
ggplot(base, aes(x = TestRealise, y = NbreCasPositif)) + geom_point(alpha=.8, aes(colour = NbreCasPositif, size = CasCom))
ggplot(base, aes(x = TestRealise, y = NbreCasPositif)) + geom_point(alpha=.8, aes(colour = NbreCasPositif, size = NbreGueris))
ggplot(base, aes(x = TestRealise, y = NbreCasPositif)) + geom_point(alpha=.8, aes(colour = NbreCasPositif, size = NbreDeces))

## Un appercu graphique des donn�es : visualiser graphique la variable
qplot(base, NbreCasPositif, fill=I("dodgerblue"))
qplot(base,TestRealise, fill=I("dodgerblue"))

## Pour avoir le nuage de points pour chaque variable
p_1 <- qplot(base, seq_along(NbreCasPositif), NbreCasPositif) + xlab("Index")
p_2 <- qplot(base, seq_along(TestRealise), TestRealise) + xlab("Index")
library(gridExtra)
grid.arrange(p_1, p_2)

## Esrimation de param�tres : r�gression lin�aire simple
reg <-lm(NbreCasPositif~TestRealise, base)
## La lecture des sorties
summary(reg)
## Extraction
names(reg)
## R�sidus 
qplot(seq_along(reg$residuals), reg$residuals) + xlab("") + ylab("Residuals")
## Ordonner les r�sidus en fonction du nombre de cas Positif
Id <- order(base$NbreCasPositif)
qplot(seq_along(reg$residuals[Id]), reg$residuals[Id])+xlab("")+ylab("Rsiduals")






