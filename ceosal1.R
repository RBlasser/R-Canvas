library(foreign)
library(car)
library(lmtest)
install.packages("plyr")
library(plyr)
library(ColorPalette)
library(ggplot2)

ceosal1 = read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/ceosal1.dta")

attach(ceosal1)

########################################################################## Pregunta (a)

#Media, mediana y Desviación Estándar
mean(salary)
median(salary)
sd(salary)

summary(salary)



#Cuantos salarios por Sector
#IndustryQ= 67
IndustyQ = length(which(Industry==1))
                   
#FinanceQ= 46
FinanceQ = length(which(Finance==1
                   
#ConsumerQ= 6
ConsumerQ = length(which(consprod==1)
                    
#Utilityq= 36
UtilityQ = length(which(utility==1)

#Total Observaciones= 209
sum(IndustyQ, FinanceQ, ConsumerQ, UtilityQ, na.rm = FALSE))


###################################################################### Pregunta (b)
attach(ceosal1)
plot(roe,salary)
max(ceosal1$roe)

library(ColorPalette)
plot(roe, salary, main = "Salario vs ROE", ylim = c(0,4000), xlim = c(min(roe),60), col = "blue", frame.plot = FALSE, pch = 20)
abline(lm(salary~roe), col = "red")



###################################################################### Pregunta (c)
#Boxplot 1 comparando Salarios por Sector
Industry <- (salary[indus == "1"])
Finance <- (salary[finance == "1"])
Consumer <- (salary[consprod == "1"])
Utility <- (salary[utility == "1"])

#El limite superior del eje Y fue reducido para poder apreciar mejor la distrubución salarial por sector
boxplot(Industry,Finance, Consumer, Utility, las = 3, col = c("red", "sienna", "palevioletred1", "royalblue2"), names = c("Industry", "Finance", "Consumer", "Utility"), ylim=c(0,3000), main = "Distribución Salarial por Sector", ylab= "Salarios")

#Promedio por Sector
IndustryMean <- mean(Industry)
FinanceMean <- mean(Finance)
ConsumerMean <- mean(Consumer)
UtilityMean <- mean(Utility)

boxplot(IndustryMean, FinanceMean, ConsumerMean, UtilityMean, ylab = "Salarios", main = "Salario Promedio por Sector", las = 3, col = c("red", "sienna", "palevioletred1", "royalblue2"), names = c("Industry", "Finance", "Consumer", "Utility"))


#Boxplot 2 comparando Salarios por Sector 
ceosal1$sector[indus == 1] ="IndustryII"
ceosal1$sector[finance == 1] = "FinanceII"
ceosal1$sector[consprod == 1] = "ConsumerII"
ceosal1$sector[utility == 1] = "UtilityII"

boxplot(salary~sector, data = ceosal1, main = "CEO's salary by Economic Sector", ylab="Salary", col = c("blue","green", "red", "gray"), ylim=c(0,4000))


###################################################################### Pregunta (d)
model1 = lm(salary~roe, data = ceosal1)
summary(model1)


#98.7 % de la varianza de salarios no es explicado por el modelo
#Un incremento de 1 punto porcentual del ROE aumenta el salario del CEO en $18,500.00

model2 = lm(log(salary)~log(sales), data = ceosal1)
summary(model2)

#Un incremento de 1% de las ventas, aumenta el salario del CEO en 0.0257%

###################################################################### Pregunta (e)


bptest(model2) #Test Breusch-Pagan para Heterocedasticidad
#p-value = 0.6792  Por tanto, no rechazamos la hipotesis de homocedasticidad (hipotesis nula)

###################################################################### Pregunta (f)
model3 = lm(log(salary)~log(sales) + roe + I(roe^2), data = ceosal1)
summary(model3)

#Este modelo permite un efecto decreciente del ROE sobre el logaritmo del salario.
#Ver signo negativo de ROE al cuadrado, Solo ROE al cuadrado no es estadisticamente diferente de cero

###################################################################### Pregunta (g)
model4 = lm(log(salary)~log(sales) + roe + finance + consprod + utility, data = ceosal1)
summary(model4)

#Se omite transportes para evitar multicolinealidad perfecta
#Salario de CEO Utility es 28.3 % menos que el de transporte (aproximadamente)
#La diferencia estadisticamente significativa al 5 %
#Todas las variables son estadisticamente significativas al 10 % (ver> p-values)
#Fianance no es estadisticamente significativa al 5 %



