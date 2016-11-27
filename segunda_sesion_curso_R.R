
library(plyr)
library(Hmisc)
library(reshape)

################################
###### CLASE DE R GUIA #########


cut(airq$Temp,breaks = 3)


####Control de estructuras en R######

#if else

x <- 2

y <- if(x < 1){3}else{1}

if(x<1){y <- 3}else{ y <-3  }

#else no es necesario


#loops for

for(i in 1:10){
    print(y)
}

x <- c("a", "b", "c", "d")

for(i in 1:4) {
    print(x[i])
}

for(i in seq_along(x)) {
    print(x[i])
}

for(letter in x) {
    print(letter)
}

for(i in 1:4) print(x[i])




#integrar varios loops

x <- matrix(1:6, 2, 3)
for(i in seq_len(nrow(x))) {
    for(j in seq_len(ncol(x))) {
        print(x[i, j])
    }
}

#while

count <- 0
while(count < 10) {
    print(count)
    count <- count + 1
}

#

z <- 5
while(z >= 3 && z <= 10) {
    print(z)
    coin <- rbinom(1, 1, 0.5)
    if(coin == 1) { ## random walk
        z <- z + 1
    } else {
        z <- z - 1
    }
}

#Repeat

x0 <- 1
tol <- 1e-8
repeat {
    x1 <- computeEstimate()
    if(abs(x1 - x0) < tol) {
        break
    } else {
        x0 <- x1
    }
}


#next

for(i in 1:100) {
    if(i <= 20) {
        ## Skip the first 20 iterations
        next
    }
    ## Do something here
}


#Calcular tiempo de función 

ptm <- proc.time()

proc.time() - ptm


#lanzamiento de un dado

#Se desea una función que simule el lanzamiento de un dado para n ensayos

dadoPar <- function(n)
{
    lanzan <- sample(1:6,n,replace = T)
    print(lanzan)
    sum(lanzan%in% c(2,4,6))/n*100
}

dadoPar(8)

Purinas<-function(x) {
    Purinas <- 0 # asignar 0 a Purinas
    for (n in x) {
        if (n == "A") Purinas <- Purinas + 1 # contar las purinas
        if (n == "G") Purinas <- Purinas + 1 # contar las purinas
    }
    return((Purinas/(length(x)))*100)
}


