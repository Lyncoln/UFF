#carregar banco saude.csv

#remover variáveis qualitativas
saude2<-saude[,-1]

###Utilizando prcomp()
pca <- prcomp(saude2, scale=TRUE) 

## plot pc1 and pc2
plot(pca$x[,1], pca$x[,2])

# Construindo scree plot
screeplot(pca)

###Utilizando pacote MVar.pt

library(MVar.pt)

PC <- PCA(saude2, type=2) # Executa o PCA
Titles = c("Grafico das Observacoes", 
           "Circulo de Correlacoes")
Plot.PCA(PC, Titles)

PC[["mtxAutvlr"]]

###PCA com train()

library(caret)

#Criar amostras treino/teste
set.seed(100)
inTrain <- createDataPartition(y=saude$Sexo,p=0.75,list=F)
training <- saude[inTrain,]
testing <- saude[-inTrain,]

ctrl<-trainControl(method="boot", number=30, 
                   preProcOptions = list(thresh = 0.95))
                   
modelFit_pca<- train(Sexo~.,data=training, method="knn",
                     preProcess="pca", trControl=ctrl )
#Detalhes do treinamento
modelFit_pca
#Detalhes do pre-processamento
modelFit_pca$preProcess

###PCA com preProcess()

#padrao: thresh=0.95
pca<-preProcess(training,method="pca")

training_pca<-predict(pca,training)

#alterando thresh
pca<-preProcess(training,method="pca", thresh=0.8)
