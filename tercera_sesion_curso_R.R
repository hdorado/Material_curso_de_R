
#Tercera sesion curso R
#Hugo Andres Dorado
#22 de octubre 2016

setwd("C:/Users/hadorado/Desktop/CURSO R UNICAUCA/Work_space")

airquality

summary(airquality)

str(airquality)

sd_airq <- apply(airquality,2,sd,na.rm=T)
mean_airq <- apply(airquality,2,mean,na.rm=T)

sd_airq/mean_airq*100 #Coeficiente de variación

cor(airquality[complete.cases(airquality),])

?png
png("correlacion.png",width = 480,height = 480)
plot(airquality)
dev.off()

#Exploracion gráfica de variables cuantitativas.

png("histograma_ozono.png",width = 520,height = 480)
hist(airquality$Ozone,breaks = 30,col="paleturquoise1",
     main = "Histograma de ozono",xlab="cantidad ozono (ppm)",
     ylab="Frecuencia")
box()
abline(v=mean(airquality$Ozone,na.rm = T),lty = 2)
text(60,10,"Me=42.13")
rug(airquality$Ozone)
dev.off()

#Boxplot 

boxplot(airquality$Solar.R,ylab="Radiacion solar (watts/mts2)",
        col="red")

boxplot(airquality$Solar.R ~ airquality$Mont,
        ylab="Radiacion solar (watts/mts2)",xlab="meses",
        col=c("blue","red","green","gold","orange"))

#Gráfico de dispersión

plot(y=airquality$Temp,x=airquality$Month,col="blue",xlim=c(5,9),ylim=c(50,120))

tempMed <- tapply(airquality$Temp,airquality$Month,mean,na.rm=T)

lines(5:9,tempMed) #lineas

tempMeAn <- tapply(airquality$Temp,airquality$Month,median,na.rm=T)

lines(5:9,tempMeAn,col="red")

legend(5,120,lty=1,col = c("black","blue"),legend = c("Media","Mediana"))
 
tabMonts <- table(airquality$Month)

#Grafico de frecuencias para variables cualitativas

#barras
barplot(tabMonts,ylim=c(0,35))
box()

#De torta
pie(tabMonts)

############################################################
###############----------ggplot2------------------##########

library(ggplot2)

ggplot(airquality,aes(x=Day,y=Wind))+
    geom_point(aes(colour=factor(Month)))

ggplot(airquality,aes(x=Day,y=Wind))+
    geom_point()+geom_smooth()+facet_grid(.~Month)

ggplot(airquality,aes(x=Day,y=Temp))+
    geom_point(aes(colour=Solar.R))+geom_smooth()+
    facet_grid(.~Month)


ggplot(airquality,aes(x=Temp))+geom_histogram()+theme_bw()+
    xlab("Temeperatura (C)")+ylab("Conteo")+ggtitle("Histograma de temperatura")+
    facet_grid(.~Month)

airquality1 <- airquality
airquality1$Month <- as.factor(airquality1$Month)

airquality1$G1 <- c(array("g1",77),array("g2",76))

airquality1$Localidad <- array(c("a","b","c"),153)

ggplot(airquality1,aes(x=Month,y=Ozone))+
    geom_boxplot()+facet_grid(Localidad~G1)


#Solucion taller

list.files()

base <- read.csv("eventos_de_platano.csv",row.names = 1)

base$FECHA_COSECHA <- as.Date(base$FECHA_COSECHA,"%m/%d/%Y")

base$rendimiento <- base$PN_ANIO/base$AREA_UM

str(base)

summary(base)

cuantBase <-base[,sapply(base, is.numeric)] 

cuantBase <- base[,c(1:5,11,13,14)]

med <- apply(cuantBase,2,mean)
sd <-  apply(cuantBase, 2, sd)

sd/med*100 #con Var

#Graficos descriptivos por cada variable; como práctica luego.

#Realizar un gráfico de ggplot en barras combinando 
#las variables variedad y dibujo de siembra como un color.

library(ggplot2)

ggplot(base,aes(x=PATRON_USADO,y=rendimiento))+
    geom_boxplot(aes(colour=DIBUJO_SIEMBRA))



