
library(epiDisplay)
library(caret)

data("faithful")
#eruption: duracao da erupcao do geiser.
#waiting: tempo de espera para a próxima erupção.
#Geiser: Old Faithful em Yellowstone.
#Objetivo: prever o tempo de espera para próxima erupção, 
#baseado no tempo da última.
#Criando amostra treino e teste (50% / 50%)
set.seed(333)
inTrain <- createDataPartition(faithful$waiting,p=0.5,list=F)
training <- faithful[inTrain,]
testing <- faithful[-inTrain,]

plot(training$waiting~training$eruptions,ylab="Waiting",
     xlab="Eruption")

#Regresao linear com caret
modelFit<- train(waiting ~ eruptions , data=training,
                 method="lm")

#Resumo do modelo
summary(modelFit)

#Observe que o erro é medido pelo RMSE
modelFit

#Desenhando linha de regressão
abline(modelFit$finalModel,col="red",lwd=3)

#Aplicar o modelo na amostra TESTE, 
# e estimar o erro out of sample
prediction <- predict(modelFit, testing)

#Calculando RMSE, R^2 e MAE na amostra TESTE
postResample(prediction,testing$waiting)

#Comparando reta sobre amostras treino/teste
par(mfrow=c(1,2))
plot(waiting ~ eruptions, data= training ,ylab="Waiting",
     xlab="Eruption",main="Amostra Treino")
abline(modelFit$finalModel,col="red",lwd=3)

plot(waiting ~ eruptions, data= testing,ylab="Waiting",
     xlab="Eruption",main="Amostra Teste")
abline(modelFit$finalModel,col="red",lwd=3)
par(mfrow=c(1,1))

