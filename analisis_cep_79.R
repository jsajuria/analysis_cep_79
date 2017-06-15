## Análisis áde edad y preferencia de voto - Encuesta CEP No. 79 Abril/Mayo 2017

# Los datos deben bajarlos de cepchile.cl

setwd("~/Downloads/encuesta_cep79_abr_may2017")

library(zeligverse)
library(foreign)

cep <- read.spss("cep79.sav", to.data.frame = T) # Le cambié el nombre al archivo.

# Re-codificación de las variables. Hay que tomar la pregunta abierta de preferencia de voto y generar bariables binarias por candidato/a
cep$pinera <- ifelse(cep$MEP_1 == "Sebastián Piñera", 1, 0)
cep$sanchez <- ifelse(cep$MEP_1 == "Beatriz Sanchez", 1, 0)
cep$guillier <- ifelse(cep$MEP_1 == "Alejandro Guiller",1,0)
cep$nsnc <- ifelse(cep$MEP_1 == "No sabe", 1, ifelse(cep$MEP_1 == "No contesta",1,0)) # Esta es la variable de no sabe/no contesta
#Edad
cep$edad <- 2017-as.numeric(as.character((cep$DS_P2A_ANO)))
#Probabilidad de votar
cep$turnout <- ifelse(as.numeric(cep$ELE_4) == 1, 1, 0)


# Regresiones
# Cada modelo est´á estimado primero con la funci´ón base y luego con Zelig, para generar los gráficos con incertidumbre.
mod.pinera <- glm(pinera ~ edad, cep, family=binomial)
summary(mod.pinera)
mod.pinera2 <- zelig(pinera ~ edad, model = "logit", data = cep)
summary(mod.pinera2)
x.out.pinera <- setx(mod.pinera2, edad = c(19:93))
s.out.pinera <- sim(mod.pinera2, x = x.out.pinera)
plot(s.out.pinera, main = "Piñera")


mod.sanchez <- glm(sanchez ~ edad, cep, family=binomial)
summary(mod.sanchez)
mod.sanchez2 <- zelig(sanchez ~ edad, model = "logit", data = cep)
x.out.sanchez <- setx(mod.sanchez2, edad = c(19:93))
s.out.sanchez <- sim(mod.sanchez2, x = x.out.sanchez)
plot(s.out.sanchez, main = "Sanchez")

mod.guillier <- glm(guillier ~ edad, cep, family=binomial)
summary(mod.guillier)
mod.guillier2 <- zelig(guillier ~ edad, model = "logit", data = cep)
x.out.guillier <- setx(mod.guillier2, edad = c(19:93))
s.out.guillier <- sim(mod.guillier2, x = x.out.guillier)
plot(s.out.guillier, main = "Guillier")

mod.nsnc <- glm(nsnc ~ edad, cep, family=binomial)
summary(mod.nsnc)
mod.nsnc2 <- zelig(nsnc ~ edad, model = "logit", data = cep)
x.out.nsnc <- setx(mod.nsnc2, edad = c(19:93))
s.out.nsnc <- sim(mod.nsnc2, x = x.out.nsnc)
plot(s.out.nsnc, main = "No sabe / No contesta")

mod.turnout <- glm(turnout ~ edad, cep, family=binomial)
summary(mod.turnout)
mod.turnout2 <- zelig(turnout ~ edad, model = "logit", data = cep)
x.out.turnout <- setx(mod.turnout2, edad = c(19:93))
s.out.turnout <- sim(mod.turnout2, x = x.out.turnout)
plot(s.out.turnout, main = "Prob. de votar")

## Gráfico con incertidumbre estimada usando simulaciones pseudo-bayesianas

set.seed(1234) #fija seed para replicabilidad

# Generar el perfil de la ósimulación
x.profile1 <- cbind(1,
                    seq(19, 93, 1))

# Simulaciones por cada regresión usando distribuciones normales multivariables
BETA.pinera <- mvrnorm(10000, coef(mod.pinera), vcov(mod.pinera))
BETA.sanchez <- mvrnorm(10000, coef(mod.sanchez), vcov(mod.sanchez))
BETA.guillier <- mvrnorm(10000, coef(mod.guillier), vcov(mod.guillier))

# Estima las probabilidades
pp1 <- plogis(x.profile1 %*% t(BETA.pinera))    
pp2 <- plogis(x.profile1 %*% t(BETA.sanchez)) 
pp3 <- plogis(x.profile1 %*% t(BETA.guillier)) 


## Generar los datos para cada línea e intervalo de confianza
#Piñera
upper1 <- rep(NA, 74)
lower1 <- rep(NA, 74)
middle1 <- rep(NA, 74)
for (j in 1:74){
  upper1[j] <- sort(pp1[j,])[9750]
  lower1[j] <- sort(pp1[j,])[250]
  middle1[j] <- sort(pp1[j,])[5000]
}

#Sanchez
upper2 <- rep(NA, 74)
lower2 <- rep(NA, 74)
middle2 <- rep(NA, 74)
for (j in 1:74){
  upper2[j] <- sort(pp2[j,])[9750]
  lower2[j] <- sort(pp2[j,])[250]
  middle2[j] <- sort(pp2[j,])[5000]
}
  
#Guillier
  upper3 <- rep(NA, 74)
  lower3 <- rep(NA, 74)
  middle3 <- rep(NA, 74)
  for (j in 1:74){
    upper3[j] <- sort(pp3[j,])[9750]
    lower3[j] <- sort(pp3[j,])[250]
    middle3[j] <- sort(pp3[j,])[5000]
  }

# Finalmente, aquí están las tres líneas en un mismo gráfico
  
par(family="CMU Serif", mfrow=c(1,1))
# Piñera
plot(seq(20,93,1), middle1, type="l", lwd=3, ylab="Probabilidad de preferencia", xlab = "Edad", col = "#ABDDDE", bty="n", ylim = c(0,0.5), xlim = c(20,93), main="Cambio en las probabilidades por edad", xaxt="n")
points(seq(20,93,1), lower1, type="l", lty=2, col=  "#ABDDDE", lwd=1)
points(seq(20,93,1), upper1, type="l", lty=2, col=  "#ABDDDE", lwd=1)

# Sanchez
points(seq(20,93,1), middle2, type = "l", lty=1,col="#972D15", lwd=3)
points(seq(20,93,1), lower2, type = "l", lty=2,col="#972D15", lwd=1)
points(seq(20,93,1), upper2, type = "l", lty=2,col="#972D15", lwd=1)

# Guillier
points(seq(20,93,1), middle3, type = "l", lty=1,col="#FAD510", lwd=3)
points(seq(20,93,1), lower3, type = "l", lty=2,col="#FAD510", lwd=1)
points(seq(20,93,1), upper3, type = "l", lty=2,col="#FAD510", lwd=1)

axis(1, at=seq(20,93,10))
legend(20,0.5,c("Piñera","Sanchez", "Guillier"), 
       col = c("#ABDDDE", "#972D15", "#FAD510"), 
       lty = rep(1,3), bty="n", lwd=rep(3,3))
