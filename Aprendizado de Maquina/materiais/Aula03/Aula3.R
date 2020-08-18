library(caret)
library(kernlab)
data(spam)

#Selecinando amostra de treino
#p=porcentagem p/ grupo de treino
#Retorna numero da linha a ser selecionanda
set.seed(100)
inTrain <- createDataPartition(y=spam$type,p=0.75,list=F) 

#Separamos linhas para amostra treino
training <- spam[inTrain,]

#Separamos linhas para amostra teste
testing <- spam[-inTrain,]

#Vamos verificar que as proporções entre spam e não-spam 
#são as mesmas entre as amostras

library(epiDisplay)
tab1(spam$type)
tab1(training$type,col='blue')
tab1(testing$type,col='orange')
