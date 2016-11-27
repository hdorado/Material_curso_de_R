



#Determinar el problema, reunión de expertos

#Descargar el conjunto de datos

#Elegir 

#-----------------------------------------------------------------------------
#Regresion lineal multiple

longley

dts <- lm(Employed~.,data=longley)

summary(dts)

modelo_2 <- step(dts)

modelo_2$residuals



#Determinar el problema, reunión de expertos

#Descargar el conjunto de datos

#Elegir 

data(iris)

head(iris)

str(iris)

summary(iris)

#boxplot

#graficos cruzados

#Analisis grafico

#Correlacion

#Realizar análisis exploratorio de datos

#Transformar datos

#Arboles de partición

set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
train.data <- iris[ind == 1, ]
test.data <- iris[ind == 2, ]

###----------------------------------------------------------------------------
###------------------------------Libreria party--------------------------------


library(party)

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length +  Petal.Width

iris_ctree <- ctree(myFormula, data = train.data)

plot(iris_ctree)

plot(iris_ctree, type = "simple")

#Validacion

testPred <- predict(iris_ctree, newdata = test.data)
table(testPred, test.data$Species)


#Librería rpart
library(rpart)
#Cifosis, curvatura arnormal
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,control = rpart.control(minsplit = 20))
#Metricas: Impureza de Gini,Ganancia de información,Reducción de la varianza
par( xpd = NA) # otherwise on some devices the text is clipped
plot(fit)
text(fit, use.n = TRUE)

###----------------------------------------------------------------------------
###------------------------------Libreria random Forest-------------------------

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))

train.data <- iris[ind==1,]

test.data <- iris[ind==2,]

# use all other variables to predict Species

library(randomForest)

rf <- randomForest(Species ~ ., data=train.data, ntree=100,
                   proximity=T)

table(predict(rf), train.data$Species)


print(rf)


plot(rf, main = "")

importance(rf)

varImpPlot(rf)

###----------------------------------------------------------------------------
###------------------------------Entrenar redes neuronales-------------------------

library(mlbench)
data(BostonHousing)
?BostonHousing
summary(BostonHousing$medv)

library(nnet)

#Vamos a construir un conjunto de datos con las variables
#independientes centradas

Boston.escalado=subset(BostonHousing,selec=-c(chas,medv))

Boston.escalado=scale(Boston.escalado)
summary(Boston.escalado)

#Dividimos por 50 para tener una respuesta entre 0 y 1
nnet.fit <- nnet(BostonHousing$medv/50 ~ Boston.escalado,
                 size=2, linout=T,trace=F)

nnet.predict <- predict(nnet.fit)*50
mean((nnet.predict - BostonHousing$medv)^2)

plot(BostonHousing$medv, nnet.predict,
     main="Neural network predictions vs actual",
     xlab="Actual")

#Analis cluster

hc <- hclust(dist(USArrests), "ave")

plot(hc, hang = -1)

memb <- cutree(hc, k = 3)

library(ggplot2)
#K means

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster


table(irisCluster$cluster, iris$Species)

################################################################################
#############ENTRENAR MODELOS UTILIZANDO CARET##################################

library(caret)
library(kernlab)
data(spam)

#Spliting date

inTrain <-  createDataPartition(y=spam$type, p=0.75, list=F)
training <- spam[inTrain,]
testing <- spam[inTrain,]


dim(training)

#Fit model

set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")
modelFit


modelFit$finalModel

predictions <- predict(modelFit,newdata =testing)
predictions


#gbm

#Stochastic Gradient Boosting

grid <- expand.grid(mtry=c(12,50))


fitControl <- trainControl(## 10-fold CV
    method = "cv",
    number = 10,
    ## repeated ten times
    )

Sys.time()->start


gbmFit3 <- train(type ~ ., data = training[sample(1:nrow(training),100),], 
                 method = "rf", 
                 trControl = fitControl, 
                 tuneGrid = grid )
gbmFit3



gbmFit3$result


predict(gbmFit3,newdata =testing)

confusionMatrix(predict(gbmFit3,newdata =testing),testing$type)
