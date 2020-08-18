
###Removendo dependencias lineares

testData2 <- matrix(0, nrow=6, ncol=6)
testData2[,1] <- c(1, 1, 1, 1, 1, 1)
testData2[,2] <- c(1, 1, 1, 0, 0, 0)
testData2[,3] <- c(0, 0, 0, 1, 1, 1)
testData2[,4] <- c(1, 0, 0, 1, 0, 0)
testData2[,5] <- c(0, 1, 0, 0, 1, 0)
testData2[,6] <- c(0, 0, 1, 0, 0, 1)

dl<-caret::findLinearCombos(testData2)

dl

testData<-testData2[,-dl$remove]

####Funcao preProcess

#O pre-processamento por ser realizado:
    #1) Por funcoes proprias (ex.: nearZeroVar(),
    #    findLinearCombos()
    #2) Dentro do train()
    #3) Utilizando a funcao preProcess()


Wage<-ISLR::Wage

set.seed(100)
inTrain <- caret::createDataPartition(y=Wage$wage,p=0.75,list=F) 

#Separamos linhas para amostra treino
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

#Criar pre processamento
tratamento<-caret::preProcess(training,
                  method = c("nzv","corr"),
                  freqCut = 95/5, uniqueCut = 10, 
                  cutoff = .75)

tratamento

#Aplicando pre processamento
training_pp<-predict(tratamento,training)

#Na amostra TESTE devemos aplicar o mesmo pre processamento
testing_pp<-predict(tratamento,testing)

#Metodos disponiveis em PreProcess

?preProcess
#"BoxCox", "YeoJohnson", "expoTrans", "center", 
#"scale", "range", "knnImpute", "bagImpute", 
#"medianImpute", "pca", "ica", "spatialSign", 
#"corr", "zv", "nzv", and "conditionalX"
