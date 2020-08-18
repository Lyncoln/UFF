library(caret)
library(kernlab)
data(spam)

set.seed(100)
inTrain <- createDataPartition(y=spam$type,p=0.75,list=F) 

#Separamos linhas para amostra treino
training <- spam[inTrain,]

#Separamos linhas para amostra teste
testing <- spam[-inTrain,]

#train(): Treina o algoritimo, pelo método escolhido
#Lista de métodos: 
names(getModelInfo())
#type ~. simboliza usar todas variaveis para descrever a var. type.
modelFit <- train(type ~ .,data=training,method="glm")

#Observar o desempenho na amostra TREINO
modelFit

#Realizamos o teste
prediction <- predict(modelFit,newdata=testing)

#Realizar avaliacao da execucao na amostra teste
confusionMatrix(prediction,testing$type)

#Trocando o classe positiva
confusionMatrix(prediction,testing$type, positive="spam")
