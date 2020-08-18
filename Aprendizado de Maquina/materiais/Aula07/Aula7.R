############
library(caret)
library(kernlab)

data(spam)

##Comparando modelos de classificação

ctrl <- trainControl(method="repeatedcv", number=10,repeats=3)

#glm
{set.seed(100)
t0 = Sys.time()
model_glm <- train(type~., data=spam, method="glm", 
                   trControl=ctrl)
glm_time<-Sys.time()-t0}
#svm
{set.seed(100)
t0 = Sys.time()
model_svm <- train(type~., data=spam, method="svmLinear", 
                   trControl=ctrl)
svm_time<-Sys.time()-t0}
#rpart
{set.seed(100)
t0 = Sys.time()
model_rpart <- train(type~., data=spam, method="rpart", 
                     trControl=ctrl)
rpart_time<-Sys.time()-t0}
#knn
{set.seed(100)
t0 = Sys.time()  
model_knn <- train(type~., data=spam, method="knn", 
                   trControl=ctrl)
knn_time<-Sys.time()-t0}

#Comparando os modelos
results <- resamples(list(GLM=model_glm, SVM=model_svm, RPART=model_rpart, KNN=model_knn))

#Resumo
summary(results)

#Tempos de Processamento
#Metodo 1
results$timings
barplot(results$timings$Everything,
        names.arg=rownames(results$timings))

##Metodo 2
glm_time
svm_time
rpart_time
knn_time

#Parametro grafico
scales <- list(x=list(relation="free"), y=list(relation="free"))

#Boxplot comparativo da accuracy e kappa
bwplot(results, scales=scales)

# Densidade da accuracy
densityplot(results, scales=scales, pch = "|", auto.key=TRUE)

#Comportamento de cada fold
parallelplot(results)

#Comparando comportamento de cada fold em diferentes testes
xyplot(results, models=c("GLM", "SVM"))
xyplot(results, models=c("KNN", "RPART"))

#Calcula diferença de accuracy/kappa entre modelos
diffs <- diff(results)
#Resumo e p-valor
summary(diffs)


