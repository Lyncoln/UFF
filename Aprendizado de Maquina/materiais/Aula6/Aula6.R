#Métodos de Re-amostragem

library(caret)
library(kernlab)

data(spam)

set.seed(100)
inTrain <- createDataPartition(y=spam$type,p=0.80,list=F) 

#Separamos linhas para amostra treino
training <- spam[inTrain,]

#Separamos linhas para amostra teste
testing <- spam[-inTrain,]

#Bootstrap
set.seed(100)
ctrl <- trainControl(method="boot", number=10)

model_boot <- train(type~., data=training, method="glm", 
                    trControl=ctrl)

#Accuracy em cada reamostragem
model_boot$resample

#Accuracy do modelo final x Media das reamostragens
model_boot$results

#Observe que a acurácia/kappa final eh a media das 
#acuracias/kappa de cada reamostragem
mean(model_boot$resample[,1])
mean(model_boot$resample[,2])


###Bootstrap fora do train()
set.seed(32323)
folds <- createResample(y=training$type,times=10,list=F)
View(folds)



###k-fold
set.seed(100)
ctrl <- trainControl(method="cv", number=10)

model_kfold <- train(type~., data=training, method="glm",
                     trControl=ctrl)

#Accuracy em cada reamostragem
model_kfold$resample

#Accuracy do modelo final x Media das reamostragens
model_kfold$results[2]

mean(model_kfold$resample[,1])


###k-fold fora do train()

#Para retornar amostras treino (exige list=T)
set.seed(100)
folds_training<-createFolds(y=training$type,k=10,list=T,
                            returnTrain = T) 
summary(folds)

training_Fold01<-training[folds_training$Fold01,]
testing_Fold01<-training[-folds_training$Fold01,]


###k-fold repetido
set.seed(100)
ctrl <- trainControl(method="repeatedcv", number=10,
                     repeats=3)

model_rkfold <- train(type~., data=training, method="glm", 
                      trControl=ctrl)

model_rkfold

###E se eu quiser saber como foi o desempenho 
#de cada amostra teste em cada Fold?
#Basta inserir o comando savePredictions = "final" 
#no trainControl

set.seed(100)
ctrl <- trainControl(method="repeatedcv", number=10,
                     repeats=3, savePredictions = "final")

model_rkfold <- train(type~., data=training, method="glm", 
                      trControl=ctrl)

model_rkfold$pred

#Vamos realizar a avaliação dos classificadores criados

pred_boot<-predict(model_boot,testing)

confusionMatrix(pred_boot,testing$type)

#Faça o mesmo para model_kfold e model_rkfold