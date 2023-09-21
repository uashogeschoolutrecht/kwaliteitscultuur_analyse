library(ltm)
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
gebruiker <- Sys.info()["user"]
data_dir <- paste("C:\\Users\\",gebruiker,"\\Stichting Hogeschool Utrecht\\FCA-DA-P - Analytics\\Analyse kwaliteitscultuur\\data",sep="")
qcidata <- read.csv2(paste(data_dir,"\\QCIdata_volledig.csv",sep=""), header = TRUE)
head(qcidata)
summary(qcidata)

library(lavaan)

model <-'
mpg ~ hp + gear + cyl + disp + carb + am + wt
hp ~ cyl + disp + carb
'

fit <- cfa(model, data = mtcars)


summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

library(semPlot)
semPaths(fit, 'std', layout = 'circle')

semPaths(fit,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)

ggcorr(mtcars[-c(5, 7, 8)], nbreaks = 6, label = T, low = "red3", high = "green3", 
       label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))
mtcars[-c(5, 7, 8)]
mtcars
nlsy



fit <- lm(hp ~ cyl + disp + carb , data=mtcars)
summary(fit) # show results
