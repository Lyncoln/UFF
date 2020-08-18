library(caret)
library(dplyr)
library(kernlab)
library(RANN)

data(spam)

#Vamos criar uma coluna com diversos NA`s
set.seed(100)
selectNA <- rbinom(dim(spam)[1],size=1,p=0.10)==1

#Vou fazer uma copia padronizada para compara o desempenho
capitalAve<-(spam$capitalAve-mean(spam$capitalAve))/sd(spam$capitalAve)
original<-capitalAve[selectNA]
#Inserir NA's
spam$capitalAve[selectNA]<-NA
#Calcular quantidade de NA's
sum(is.na(spam$capitalAve))

#Criar amostras treino/teste
set.seed(100)
inTrain <- createDataPartition(y=spam$type,p=0.75,list=F)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

#Verificar quantidade de NA's
sum(is.na(training$capitalAve))

#Utilizando funcao preProcess()
preproc_NA <- preProcess(training,method = "knnImpute",
                         k=5)

training_knn <- predict(preproc_NA,training)

#Observe que training_knn eh o banco de dados training,
#com todos os dados padronizados e sem NA's.

#Aplicamos o mesmo método no banco testing
testing_knn <- predict(preproc_NA,testing)

#Comparar diferenca original x dados imputados
#coordenadas no banco spam com NA's
x<-which(selectNA==T)
#coordenadas no banco training que eram NA's
y<-which(inTrain %in% x)
#coordenadas das dados que eram do banco training
z<-which(x %in% inTrain)
#dados imputados no training
imput_train<-training_knn$capitalAve[y]
#dados originais
original_train<-original[z]
#avaliando
plot(original_train,imput_train)
postResample(original_train,imput_train)

#dentro do train() há um bug
modelFit<-train(type~., data=training,
                preProcess="knnImpute")


###Variáveis Dummy
library(ISLR)
library(epiDisplay)
data(Wage)

#jobclass: Information indicating type of job
tab1(Wage$jobclass)
#health_ins: indicating whether worker has health insurance
tab1(Wage$race)

#Criando modelo para Variaveis dummies p/ 
#jobclass e health_ins
dummies <- dummyVars(wage~jobclass+race,data=Wage)

#Cria novas variaveis dummies, 0=nao possui, 1=possui.
jobdummies<-predict(dummies,newdata=Wage)

#Podemos adicionar essas variáveis no banco de dados.
Wage_dummy<-cbind(Wage,jobdummies)

#Se eu desejar, posso remover as antigas 
#variáveis categóricas
Wage_dummy<-dplyr::select(Wage_dummy,-c(jobclass,race))

#Para realizar modelos de regressão, não é necessário 
#ter duas variáveis dummies para a mesma característica,
#pois estatíamos inserindo variáveis 
#com colinearidade perfeita no modelo.
dummies <- dummyVars(wage~jobclass+race,
                     data=Wage,fullRank=T)

jobdummies<-predict(dummies,newdata=Wage)

