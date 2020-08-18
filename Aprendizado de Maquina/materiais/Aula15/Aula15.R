
library(rpart)
library(rpart.plot)
library(caret)

#Carregar banco de dados golf.xlsx

playTree<-rpart(Play~.,data=golf)
rpart.plot(playTree)
#Retorna: responda sempre Yes, com 64% de acerto

#Vamos alterar os parâmetros
ctrl=rpart.control(minsplit=0)
playTree<-rpart(Play~.,data=golf,control=ctrl)
rpart.plot(playTree)

ctrl=rpart.control(minsplit=0,cp=0.1)
playTree<-rpart(Play~.,data=golf,control=ctrl)
rpart.plot(playTree)

ctrl=rpart.control(minsplit=0,cp=0,maxdepth=3)
playTree<-rpart(Play~.,data=golf,control=ctrl)
rpart.plot(playTree)

#########
#Árvores com train()

#Carregar banco de dados
load("heart_disease.rda")

set.seed(100)

#Separar amostras teste/treino
inTrain <- createDataPartition(data$hd,p=0.7,list=F)
training <- data[inTrain,]
testing <- data[-inTrain,]

#Treinar o modelo
modFit<-train(hd~., method="rpart", data=training)
#Aplicar o modelo no Teste
hdFit<-predict(modFit,testing)
#Avaliar o erro na amostra Treino
confusionMatrix(hdFit,testing$hd)
#Plota árvore
rpart.plot(modFit$finalModel)

#######
#rpart só permite alterar cp no train
modelLookup("rpart")

rpart.grid <- expand.grid(.cp=0.04)

modFit<-train(hd~., method="rpart", 
              data=training, tuneGrid=rpart.grid)

modFit

#Aplicar o modelo no Teste
pred<-predict(modFit,testing)
#Avaliar o erro na amostra Treino
confusionMatrix(pred, testing$hd)
#Plota árvore
rpart.plot(modFit$finalModel)


