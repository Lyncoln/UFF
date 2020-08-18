##Pre-Processamento

library(ISLR)
library(caret)

#Near Zero Covariates

data(Wage)

#"near zero covariates" = variaveis que nao auxiliam na 
#predicao, pois aparecem em muitos ou quase nenhum individuo.

nearZeroVar(Wage,saveMetrics = T) 
#saveMetrics = T mostra todos detalhes, 
#se F mostra apenas variaveis a serem ignoradas

#Retorna ordem (coluna) da variável nearzerovar
nzv<-nearZeroVar(Wage,saveMetrics = F)
nzv
Wage_nzv<-Wage[,-nzv]

#Retorna nome da variavel nearzerovar
nzv<-nearZeroVar(Wage,saveMetrics = F,names=T)
nzv
Wage_nzv<-dplyr::select(Wage,-nzv)

#Trocando ponto de corte

nearZeroVar(Wage,saveMetrics = T,freqCut = 8)

epiDisplay::tab1(Wage$race)
#freqRatio  8.464164 > 8
2480/293
#percUnique  0.1333333 < 10
4*100/3000
#Comparando com os valores da tabela
nearZeroVar(Wage,saveMetrics = T,freqCut = 8)[4,]

#via train()

data(Wage)

set.seed(100)
inTrain <- createDataPartition(y=Wage$wage,p=0.80,list=F) 

#Separamos linhas para amostra treino
training <- Wage[inTrain,]

#Separamos linhas para amostra teste
testing <- Wage[-inTrain,]

ctrl <- trainControl(method="boot", number=10,
                     preProcOptions = list(freqCut = 95/5,
                                           uniqueCut = 10))

modelFit <- train(wage~., data=training, method="lm", 
                  trControl=ctrl, preProcess="nzv")

modelFit$preProcess$method$remove

#Aplicamos normamente na amostra teste
pred_boot<-predict(modelFit,testing)

postResample(pred_boot,testing$wage)


###Correlacao

library(kernlab)

#Carregamos banco de dados
data(spam)

#Calculamos matriz de correlacao das var. quantitativas 
descrCor <-  cor(spam[1:57])

#Resumo das correlacoes
#upper.tri = seleciona a diag superior da matriz
summary(descrCor[upper.tri(descrCor)])

#Quais variaveis tem alta correlacao?
findCorrelation(descrCor, cutoff = .75, verbose=T)

#Novo banco sem var. com alta correlacao
highCor<-findCorrelation(descrCor, cutoff = .75,
                         names=T)

spam2<- dplyr::select(spam,-highCor)

#Via train()

data(spam)

set.seed(100)
inTrain <- createDataPartition(y=spam$type,p=0.80,list=F) 

#Separamos linhas para amostra treino
training <- spam[inTrain,]

#Separamos linhas para amostra teste
testing <- spam[-inTrain,]

ctrl <- trainControl(method="boot", number=10,
                     preProcOptions = list(cutoff = 0.75))

modelFit <- train(type~., data=training, method="glm", 
                  trControl=ctrl, preProcess=c("nzv","corr"))

modelFit$preProcess$method$remove

#Aplicamos normamente na amostra teste
pred_boot<-predict(modelFit,testing)

confusionMatrix(pred_boot,testing$type)

