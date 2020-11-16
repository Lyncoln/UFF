###Padronizando Variaveis
library(caret)
library(kernlab)

data(spam)

#Criar amostras treino/teste
set.seed(100)
inTrain <- createDataPartition(y=spam$type,p=0.75,list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

#Vamos observar o comportamento da variavel "receive" 
mean(training$receive)
sd(training$receive)

#Podemos criar alteracoes nas variaveis de estudo, 
#para tornar a analise mais eficiente. Vamos transforma-las
#em variaveis de média 0 e desvio padrao 1.

padr<-preProcess(training, method=c("center","scale"))

training_pdr<-predict(padr,training)

testing_pdr<-predict(padr,testing)


#Vamos observar o comportamento da variavel "receive" 
mean(training_pdr$receive)
sd(training_pdr$receive)

#Utilizando funcao train()
set.seed(100)
ctrl<-trainControl(method="repeatedcv", number=10, rep=3)
modelFit<-train(type~., data=spam,method="knn", trControl=ctrl)

set.seed(100)
ctrl<-trainControl(method="repeatedcv", number=10, rep=3)
modelFit_pp<-train(type~., data=spam,method="knn", trControl=ctrl,
         preProcess=c("center", "scale"))

#Comparando os modelos
results <- resamples(list(KNN=modelFit, KNN_pp=modelFit_pp))

#Resumo
summary(results)

#Parametro grafico
scales <- list(x=list(relation="free"), y=list(relation="free"))

#Boxplot comparativo da accuracy e kappa
bwplot(results, scales=scales)


###Normalizacao dos dados

ctrl<-trainControl(method="repeatedcv", number=10, rep=3)

#Sem normalizar
set.seed(100)
modelFit <- train(type~.,data = spam, method = "knn",
                     trControl=ctrl)

#BoxCox
set.seed(100)
modelFit_bc <- train(type~.,data = spam, method = "knn",
                 preProcess="BoxCox", trControl=ctrl)

#Yeo-Johnson
set.seed(100)
modelFit_YJ <- train(type~.,data = spam, method = "knn",
                     preProcess="YeoJohnson", trControl=ctrl)

#Transformação Exponencial de Manly
set.seed(100)
modelFit_exM <- train(type~.,data = spam, method = "knn",
                     preProcess="expoTrans", trControl=ctrl)

#Comparando os modelos
results <- resamples(list(semNorm=modelFit, Box=modelFit_bc,
                     YJ=modelFit_YJ, exM=modelFit_exM))

#Resumo
summary(results)

#Parametro grafico
scales <- list(x=list(relation="free"), y=list(relation="free"))

#Boxplot comparativo da accuracy e kappa
bwplot(results, scales=scales)

#Uma vez escolhido o metodo, realizamos o treinamento e o teste
set.seed(100)
inTrain <- createDataPartition(y=spam$type,p=0.75,list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

ctrl<-trainControl(method="repeatedcv", number=10, rep=3)

modelFit<-train(type~., data=training,method="knn",
                preProcess="BoxCox", trControl=ctrl)

pred<- predict(modelFit,testing)
confusionMatrix(pred,testing$type)
