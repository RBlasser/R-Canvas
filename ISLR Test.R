install.packages("ISLR")
library(ISLR)
library(plyr)
install.packages("eeptools")
library(eeptools)
library(lmtest)
library(foreign)
library(car)

Default = data.frame(Default)


###Pregunta (a)
# Tasa de No Pago. Es mayor entre estudiantes?
attach(Default)

#Tasa No Pago (total)
tasaNp <- length(which(default=="No"))
sum(tasaNp)

#Tasa Si Pago (total)
tasaSP <- length(which(default=="Yes"))
sum(tasaSP)

#Tasa No Pago + Tasa Si Pago
sum(tasaNp, tasaSP)

#Numero de Estudiantes (total)
studentSI <- length(which(student == "Yes"))
sum(studentSI)

#Numero de No Estudiantes (total)
studentNO <- length(which(student == "No"))
sum(studentNO)

#Estudiantes + No Estudiantes
sum(studentSI,studentNO)


pie(table(default, student), main = "Proporcion de Estudiantes que no pagan")

table(default, student)

names(Default)

plot(Default$student)

df <- data.frame(table(Default$default, Default$student))

df=Default
table(default, student)

#Estudiantes que No Pagaron
subdf1=subset(df, student=="Yes" & default =="Yes")
sprintf("%.2f %%", 100 * length(which(subdf1$default=="Yes", arr.ind = TRUE))/(sum(studentNO,studentSI)))

#Estudiantes que Si Pagaron
subdf2=subset(df, student=="Yes" & default =="No")
sprintf("%.2f %%", 100 * length(which(subdf2$student=="Yes", arr.ind = TRUE))/(sum(tasaSP,tasaNp)))

#No Estudiantes que No Pagaron
subdf3=subset(df, student=="No" & default =="Yes")
sprintf("%.2f %%", 100 * length(which(subdf3$default=="Yes", arr.ind = TRUE))/(sum(tasaSP,tasaNp)))

#No Estudiantes que Si Pagaron
subdf4=subset(df, student=="No" & default =="No")
sprintf("%.2f %%", 100 * length(which(subdf4$default=="No", arr.ind = TRUE))/(sum(tasaSP,tasaNp)))


#Tasa Total de No Pago
Tasa_No_Pago_Total <- length(which(Default$default=="Yes", arr.ind = TRUE))/(sum(tasaNp,tasaSP))
sprintf("%.2f %%", 100 * Tasa_No_Pago_Total)
Tasa_Estudiantes_No_Pago <- (length(which(subdf1$student=="Yes", arr.ind = TRUE)))/(sum(studentSI,studentNO))

#RESPUESTA (A)> Tasa No Pago: 3.33%   Tasa No Pago Estudiantes: 1.27%


################################################

Default$no_pago[default=="Yes"] = 1
Default$no_pago[default=="No"] = 0
model = glm(default~balance, family=binomial(link = logit), data = Default)
summary(model)

#RESPUESTA (B)
attach(Default)
boxplot(balance, default, names = c("Balance","Default"), main= "Balance vs Default")

#RESPUESTA (C)
attach(Default)
boxplot(income, default)

#RESPUESTA (D)
attach(Default)
boxplot(balance, student)

#RESPUESTA (E)
attach(Default)
Default$no_pago[default=="Yes"] = 1
Default$no_pago[default=="No"] = 0
Default$si_student[default=="Yes"] = 1
Default$si_student[default=="No"] = 0

linprob <- lm(no_pago ~ balance, data = Default)
summary(linprob)
coeftest(linprob, vcov = hccm)
## Min: -0.23 Max: 0.99

#RESPUESTA (F)

logitprob <- glm(no_pago ~ balance, family = binomial(link = logit), data = Default)
summary(logitprob)
xpred1 = list(balance = c(1000,2000))
predict(logitprob, xpred1, type="response")

#RESPUESTA (G)
probitprob <- glm(no_pago ~ balance, family = binomial(link = probit), data = Default)
summary(probitprob)
xpred1 = list(balance = c(1000,2000))
predict(probitprob, xpred1, type="response")

#RESPUESTA (H)
ModelLogitH<- glm(no_pago ~ balance + income + si_student, family = binomial(link = logit), data = Default)
summary(ModelLogitH)
coeftest(ModelLogitH, vcov = hccm)
xpred4= list(balance = c(1500,1500), income=c(40000,40000), si_student = c(1,0))
predict(ModelLogitH, xpred4, type = "response")
summary(predict(ModelLogitH, xpred4, type = "response"))
