library(ltm)
library(lavaan)
library(markdown)
library(foreign)
library(semPlot)
library(faraway)
library(survival)
library(psych)
library(GPArotation)


---
# qcidata <- read.csv2("QCIdata.csv", header = TRUE)
--- 

library(sys)
gebruiker = Sys.info()["user"]
data_dir = paste("C:\\Users\\",gebruiker,"\\Stichting Hogeschool Utrecht\\FCA-DA-P - Analytics\\Analyse kwaliteitscultuur\\data",sep="")
qcidata <- read.csv2(paste(gebruiker,"\\QCIdata_volledig.csv",sep=""), header = TRUE)
head(qcidata)
summary(qcidata)




# replace X door NA
qcidata[qcidata == 6]<-NA
qcidata[qcidata == 7]<-NA
qcidata[qcidata == 8]<-NA

# Cronbach's alpha bij selecteren van vragen uit de gehele dataset
# hier: de vragen mbt Individueel eigenaarschap (doeltreffendheid)

vraag3=qcidata$Vraag3
vraag4=qcidata$Vraag4
vraag5=qcidata$Vraag5
vraag6=qcidata$Vraag6

alpha1=data.frame(vraag3,vraag4,vraag5,vraag6)
alpha(alpha1)
reliability(alpha1)

# Het verwijderen van vraag3 resulteert in een verhoging qua betrouwbaarheid >0,05
# Vraag: Dus vervolgstap?

alpha1a=data.frame(vraag4,vraag5,vraag6)
alpha(alpha1a)

# Kijk wat er met resultaat vraag4 gebeurt. Ook deze geeft verschil > 0,05. 
# Vraag: Ook verwijderen?

#... maar onderstaande werkte niet... Waarom? Moet NA toch vervangen worden door gemiddelde
# van de kolom?

alpha1_matrix<-data.matrix(qcidata)
reliability(cov(alpha1_matrix))
cov(alpha1_matrix)

#Individueel eigenaarschap (affectiviteit)
vraag7=qcidata$Vraag7
vraag8=qcidata$Vraag8
vraag9=qcidata$Vraag9

alpha2=data.frame(vraag7,vraag8,vraag9)
alpha(alpha2)

# Individueel eigenaarschap (cognitie)
vraag10=qcidata$Vraag10
vraag11=qcidata$Vraag11
vraag12=qcidata$Vraag12
vraag13=qcidata$Vraag13

alpha3=data.frame(vraag10,vraag11,vraag12,vraag13)
alpha(alpha3)

#covariance / reliability
# Vraag:waarom werkt onderste niet? zie onderzoek OVO

alpha1_matrix<-data.matrix(alpha1)
reliability(alpha1_matrix)
reliability(alpha1)

reliability(alpha2)
reliability(alpha3)

cov(var(alpha1,na.rm = TRUE))
cov(var(alpha1_matrix,na.rm = TRUE))

reliability(cov(var(alpha1_matrix,na.rm = TRUE)))

            