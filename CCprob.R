library(ISLR)
library(plyr)
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

subdf=subset(df, student=="Yes" & default =="Yes")

length(which(subdf$student=="Yes", arr.ind = TRUE))

StudentsYESPAY <- length(which(subdf$student=="Yes", arr.ind = TRUE))

Repuesta_A<- (length(which(subdf$student=="Yes", arr.ind = TRUE)))/(sum(studentSI,studentNO))
Repuesta_A




