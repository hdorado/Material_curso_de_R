
#Sesión 4 Curso R, inferencia estadística
#Hugo Andres Dorado B
#29 octubre del 2016


#Generar una muestra de una distribución.

?sample

#Con replanzamiento

sample(x=c("c","s"), size = 6 ,prob = c(0.5,0.5),replace = T)

#Sin remplazamiento
sample(x=c("c","s"), size = 2 ,prob = c(0.5,0.5),replace = F)

#distribución bernoulli

?rbinom

rbinom(20,size=1,prob = 0.03)

moneda <- rbinom(40,size=1,prob = 0.5)

moneda

mean(moneda)

var(moneda)

0.5*0.5

#Lanzar un dado 51 veces y contar 3

dbinom(20,size = 51,1/6)

pbinom(20,size = 51,1/6)

?ppois

ppois(2,2)

#Simular la llegada de los pacientes por treinta minutos
#En un tiempo total de 6 horas

llegadasd <- rpois(50,2)

barplot(table(llegadasd))

#Distribucion normal

layout(matrix(1:4,2,2,byrow = T))

botellas <- rnorm(100,mean = 200,sd = 15)

hist(rnorm(10,mean = 200,sd = 15),main="n = 10")

hist(rnorm(50,mean = 200,sd = 15),main="n = 50")

hist(rnorm(100,mean = 200,sd = 15),main="n = 100")

hist(rnorm(200,mean = 200,sd = 15),main="n = 200")

?pnorm

pnorm(210,mean = 200, sd=15)-pnorm(190,mean = 200,sd=15)

#Teorema central del limite

hist(rbeta(1000,5,2))

hist(rexp(1000,4))


tlcBeta <- function(n){
    m.muestral <- 0
    for(i in 1:100){    
        m.muestral[i] <- mean(rbeta(n,5,2))
    }
    m.muestral    
}

hist(tlcBeta(100))

tlcexp <- function(n){
    m.muestral <- 0
    for(i in 1:100){    
        m.muestral[i] <- mean(rexp(n,5))
    }
    m.muestral    
}

layout(matrix(1:4,2,2,byrow = T))

hist(tlcexp(20),main="n=20")
hist(tlcexp(50),main="n=50")
hist(tlcexp(100),main="n=100")
hist(tlcexp(200),main="n=200")

#intervalos de confianza

muestra.edades <- rnorm(45,mean=25,sd=6)

#intervalo de confianza para la media

t.test(muestra,conf.level = 0.95)

#Prorciones verficar----proxima clase

m.estudiantes <- rbinom(n=100,1,prob = 0.3)

sum(m.estudiantes)

prop.test(41,100,correct = F)

#Intervarlo para la varianza

muestra.edades.medicina <- rnorm(45,mean=21,sd=6)

var.test(muestra.edades,muestra.edades.medicina)

t.test(x=muestra.edades,y=muestra.edades.medicina,
       alternative = "less")


boxplot(muestra.edades,muestra.edades.medicina)

c1 <- runif(20,0,5)
c2 <- runif(20,3,5)    

t.test(c1,c2,alternative = "less",paired = T)

mean(c1)
mean(c2)

#Analisis de varianza.

edad.ing <- rnorm(n=35,mean=24,sd=4) 
edad.med <- rnorm(n=35,mean=24,sd=4)
edad.soc <- rnorm(n=35,mean=24,sd=4)
edad.adm <- rnorm(n=35,mean=24,sd=4)

estudiantes <-
rbind(
data.frame(edad=edad.ing,facultad="ingeneria"),
data.frame(edad=edad.med,facultad="medicina"),
data.frame(edad=edad.soc,facultad="sociales"),
data.frame(edad=edad.adm,facultad="administracion")
)

aov.estudiantes <- aov(edad~facultad,data=estudiantes)

summary(aov.estudiantes)

#prueba 2

edad.ing <- rnorm(n=35,mean=24,sd=4) 
edad.med <- rnorm(n=35,mean=23,sd=4)
edad.soc <- rnorm(n=35,mean=28,sd=5)
edad.adm <- rnorm(n=35,mean=24,sd=4)

estudiantes <-
    rbind(
        data.frame(edad=edad.ing,facultad="ingeneria"),
        data.frame(edad=edad.med,facultad="medicina"),
        data.frame(edad=edad.soc,facultad="sociales"),
        data.frame(edad=edad.adm,facultad="administracion")
    )

aov.estudiantes <- aov(edad~facultad,data=estudiantes)

summary(aov.estudiantes)

#Prueba pos-anova

TukeyHSD(aov.estudiantes)

plot(TukeyHSD(aov.estudiantes))

#Prueba de normalidad

qqnorm(aov.estudiantes$residuals)
qqline(aov.estudiantes$residuals)
