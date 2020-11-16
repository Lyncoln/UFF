install.packages("randomForest")
#########
library(caret)
library(randomForest)
library(ggplot2)

##Floresta Aleatória

#Carregar o banco de dados heart.rda

load("heart_disease.rda")

#Com a funcao randomForest()
#25 árvores
set.seed(42)
modelFit<-randomForest(hd ~ ., data=data, ntree=25,
                    proximity=T)

#Confusion matrix das amostras OOB
modelFit

#Votos das árvores para cada amostra
head(modelFit$votes)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(modelFit$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), 
           each=nrow(modelFit$err.rate)),
  Error=c(modelFit$err.rate[,"OOB"],
          modelFit$err.rate[,"Healthy"],
          modelFit$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type),size=1.1)

#Blue line = The error rate specifically for calling 
#"Unheathly" patients that are OOB.
#Green line = The overall OOB error rate.
# Red line = The error rate specifically for calling 
#"Healthy" patients that are OOB.

#100/1.000/10.000 árvores
set.seed(42)
modelFit<-randomForest(hd ~ ., data=data, ntree=100,
                    proximity=T)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(modelFit$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), 
           each=nrow(modelFit$err.rate)),
  Error=c(modelFit$err.rate[,"OOB"],
          modelFit$err.rate[,"Healthy"],
          modelFit$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

#Observe que a taxa de erro estabilizou