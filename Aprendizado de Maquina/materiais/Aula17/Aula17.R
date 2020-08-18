
library(caret)
library(randomForest)

##Floresta Aleatória

#Carregar o banco de dados heart.rda
load("heart_disease.rda")

#Criar amostras treino/teste
set.seed(100)
inTrain <- createDataPartition(y=data$hd,p=0.75,list=F)
training <- data[inTrain,]
testing <- data[-inTrain,]

#Treinando com randomForest()
#mtry=numero de variaveis sorteadas
#ntree=numero de arvores da floresta

modelFit<-randomForest(hd ~ ., data=training, ntree=1000,
                       proximity=T)

modelFit
#Erro OOB: 19.28%

modelFit2<-randomForest(hd ~ ., data=training, ntree=1000,
                       mtry=5, proximity=T)

modelFit2  
#Erro OOB: 18.83%


##Floresta Aleatória com train

#sem controlar mtry
ctrl <- trainControl(method="oob")
modelFit<- train(hd ~ ., data=training,method="rf",
               ntree=50,trControl=ctrl)

modelFit

pred<-predict(modelFit,testing)

confusionMatrix(pred,testing$hd)

#controlando mtry
ctrl <- trainControl(method="oob")
tng <- expand.grid(.mtry=4)
modelFit2<- train(hd ~ ., data=training,method="rf",
               ntree=50,trControl=ctrl,tuneGrid=tng)
modelFit2

pred2<-predict(modelFit2,testing)

confusionMatrix(pred2,testing$hd)

#Exercicio: compare a acuracia OOB com a da 
#amostra TESTE (Validacao).
