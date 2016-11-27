
#Primera seccion curso R
#Hugo Andres  Dorado
#Octubre 10

1234
1
3
4

?rnorm #Ayuda en R directamente con la función

windows() #Ventana opcional para generar gráficos

#Ayudas en R

? # base directamente con la función

help.start()

help.search("poisson")

?apropos

apropos("lm")

demo(lm.glm)

######Direccionar y ubicar directorio de trabajo#####

getwd()

setwd("C:/Users/hadorado/Desktop/CURSO R UNICAUCA/Work_space")

getwd()

######Instalar paquetes en R######

install.packages("plyr")

install.packages("ggplot2") #Instalar para próxima clase

library(plyr)  #Cargar un paquete en R

detach(plyr) #descartar el uso del paquete

ls()

rm(list = ls()) #Borrar todo

#####Manipulación de objetos en R########

edad <- 25 
edad

edades <- c(12,15,18,16,21) #Crear un objeto en R

c <- 21
c

sexo <- c("M","F","M","M","F")

1:15 # Vector ordenado de valores

array(4,10) # Vector con el mismo valor repetido

seq(1,8,1.5) #Secuencia de valores

estatura <- c(1.7,1.6,NA,1.8,1.1)

#Idetificar el tipo objeto almacenado

is(estatura)
is(sexo)

#Matrices en R

matriz1 <- matrix(1:9,nrow=3,ncol=3)
matriz1

matriz2 <- matrix(1:9,3,3,byrow = T)
matriz2

#Utilizar rbind y cbind

baseDatos <- cbind(edades,estatura)

nuevInd1 <- c(28,1.6)
nuevInd2 <- c(25,1.8)

baseDatos2 <- rbind(baseDatos,nuevInd1,nuevInd2)

baseDatos2

dfBaseDatos <- data.frame(baseDatos,sexo) #Formato base de datos

plot(edades,estatura,type = "b",col="red") #Gráfico de dispersión

#Crear una lista

listaObje <- list(c,estatura,dfBaseDatos)

listaObje

#Encabezados

names(dfBaseDatos)

names(dfBaseDatos) <- c("edades","estaturas","sexos")

names(listaObje) <- c("C","estaturas","dfBaseDatos")

listaObje

head(dfBaseDatos,2)

tail(dfBaseDatos,2)

#Extracción de partes de los objetos

edades[3]  #Vectores (3ra posición)

length(edades) #Lóngitud del vector

sexo[4] # Sexo, extraer posicion
length(sexo) #extraer lóngitud del vector

edades[2:4]

edades[c(1,3,5)]

#extraer partes de una matriz

matriz1[1,2]

matriz1[2,] # extraer fila 2

matriz1[,1] #extraer fila 1

matriz1[2:3,2:3]

#Extraer partes de los data.frame

dfBaseDatos$sexos

row.names(dfBaseDatos) <- paste("persona",1:5,sep="_")

row.names(dfBaseDatos)

#Extrar partes de listas

listaObje$estaturas

listaObje$dfBaseDatos

listaObje[[2]]

listaObje[[1]]

#Operaciones basicas entre vectores

set.seed(666)

vec1 <- rnorm(20,mean = 2,sd=1)
vec2 <- rnorm(20,mean = 3,sd=1)

sum(vec1) #suma

prod(vec1) #Producto

vec1^2

mean(vec1) #media

sd(vec1) #Desviacion

var(vec1) #Varianza

vec1+vec2

sqrt(9)

9/3

resultado <- sqrt(sum((vec1-vec2)^2))/20
resultado

#Operaciones para matrices

matriz2^3

matriz1 * matriz2 #producto de componente a componente

matriz1 %*% matriz2 #producto cruz

3*matriz1 #producto escalar

matrix(1:4,1,4) %*% matrix(5:8,4,1)

matriz3 <- matrix(runif(9),3,3)

solvMatriz3 <- solve(matriz3) #encontrar la inversa

round(matriz3 %*% solvMatriz3,5)

#Más funciones 

sort(estatura)

sort(estatura,decreasing = T) #ordenar

unique(sexo) #generar valores no repetidos

fix(sort) #Evaluar la manera en que estan programadas las funciones

#Práctica en R

#Generar un data.frame con un grupo de 10 personas a las 
#cuales se les preguntó su voto 

#Agregar nombres a cada fila
#Varibles Sexo,edad,estrato,departamento,religion,voto
sexo <- c("M","F","M","F","M","F","M","M","F","M")
edad <- runif(10,20,40)
estrato<- runif(10,1,6)
depatamento <- c("cauca","valle","cord","ant","ant","cauca","valle","cord","ant","ant")
religion <- c("cat","testg","cat","cat","testg","cat","cat","testg","cat","testg")
voto <- c("si","no","no","si","no","si","no","no","si","no")

votoplev <- data.frame(sexo,edad,estrato,depatamento,religion,voto)
row.names(votoplev) <- paste("per",1:10,"_")


#Crear luego aparte las variables partido politico e ingresos

partPol <- c("polo","U","centroDemo","polo","polo","centroDemo","U","centroDemo","polo","centroDemo")

#Luego adicionarlo al data.frame

votoplev$partPol <- c("polo","U","centroDemo","polo","polo","centroDemo","U","centroDemo","polo","centroDemo") #o con cbind

#Ordenar el data.frame por edad (Ayuda: explore la funcion order)

order(votoplev$edad)

dataSort <- votoplev[order(votoplev$edad),]

?write.csv

write.csv(dataSort,"dataSort.csv")

write.table(dataSort,"C:\\Users\\hadorado\\Desktop\\CURSO R UNICAUCA\\dataSort.txt",sep = ",")

save(dataSort,votoplev,edad, file="Voto.RData")

rm(list=ls())

load("Voto.RData")

write.table(dataSort,"dataSort.csv",sep = ";",dec = ",")

edad

?source #Ejecutar totalmente un script de un archivo .R

#De la fila ordenada extraiga una muestra de las 3 ultimas 
#personas.

#adicionar 2 personas más

#

vec <- runif(10)

is(vec)

vecCha <- as.character(vec)

vecFac <- as.factor(vec)

#Convertir el tipo de variables

as.numeric(vecFac) # A númerico

as.numeric(vecCha)

as.numeric(as.character(vecFac)) #Ojo con el factor a númerico

listest <- list(a=c(3,2,3),b=7,d=1:3)

is(unlist(listest)) #Deslistar
