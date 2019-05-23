library(foreign)
library(car)

csv.data <- read.csv("C:/Users/rb/Desktop/GlobalMinds/UChile/Econometria/R/DB1 for Monte Carlo.csv")

n <- 10000

summary(csv.data)

f <- function(x) x^2

plot(runif(n), runif(n), col = "blue", pch = 20)
curve(f, 0,1, n =100, col = "white", add = TRUE)

IngresosVar <- var(csv.data$Ingresos)
sqrt(IngresosVar)
sd(Ingresos)

attach(csv.data)

set.seed(3000)

distnom <- dnorm(Ingresos, mean = Ingresos, sd = Ingresos)


hist(rnorm(10000, 32000, 4594.683), main = "Distribucion de Ingresos", xlab = "Ingresos")


dDistNorm <- dnorm(Ingresos, 32000, 4594.683)


###########

set.seed(3000)
xseq2 <- seq(-3, 3, .005)
densities2 <- dnorm(xseq2,0,1)
cumulative2 <- pnorm(xseq2, 0, 1)