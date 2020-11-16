library(tidyverse);library(caret)
setwd("I:/GitHub/UFF/Aprendizado de Maquina/Listas/Lista4")

load("chd.rda")



# Criando Dummie

dummie = dummyVars(TenYearCHD ~ education + sex,
                   data = chd,
                   fullRank = T)

pred_dummie = predict(dummie,
                      newdata = chd)

chd_dummie = cbind(chd,pred_dummie)

chd_dummie = select(chd_dummie, -c(education,sex))

chd_dummie = chd_dummie %>% 
  mutate(education.2 = as.factor(education.2)) %>% 
  mutate(education.3 = as.factor(education.3)) %>% 
  mutate(education.4 = as.factor(education.4)) %>% 
  mutate(sexM = as.factor(sexM)) 
  
set.seed(100)

input = preProcess(chd_dummie,
                   method = "knnImpute",
                   k = 5)

data_inputada = predict(input, chd_dummie)
data_inputada = na.omit(data_inputada)

ctrl_boot = trainControl(method = "boot", number = 20, 
                         preProcOptions = list(cutoff = 0.75, freqCut = 95/5, uniqueCut = 10))
ctrl_cv = trainControl(method="repeatedcv", number = 10, repeats = 3, 
                       preProcOptions = list(cutoff = 0.75, freqCut = 95/5, uniqueCut = 10))

#svmPoly

set.seed(100)
model_svmPoly_cv = train(TenYearCHD ~ ., method = "svmPoly", trControl = ctrl_cv,
                         data = data_inputada,
                         preProcess = c("nzv","corr"))
set.seed(100)
model_svmPoly_boot = train(TenYearCHD ~ ., method = "svmPoly", trControl = ctrl_boot,
                         data = data_inputada,
                         preProcess = c("nzv","corr"))

#svmRadial

set.seed(100)
model_svmRadial_cv = train(TenYearCHD ~ ., method = "svmRadial", trControl = ctrl_cv,
                           data = data_inputada,
                           preProcess = c("nzv","corr"))
set.seed(100)
model_svmRadial_boot = train(TenYearCHD ~ ., method = "svmRadial", trControl = ctrl_boot,
                           data = data_inputada,
                           preProcess = c("nzv","corr"))

#GLM
set.seed(100)
model_glm_cv = train(TenYearCHD ~ ., method = "glm", trControl = ctrl_cv,
                     data = data_inputada,
                     preProcess = c("nzv","corr"))
set.seed(100)
model_glm_boot = train(TenYearCHD ~ ., method = "glm", trControl = ctrl_boot,
                     data = data_inputada,
                     preProcess = c("nzv","corr"))
#KNN
set.seed(100)
model_knn_cv = train(TenYearCHD ~ ., method = "knn", trControl = ctrl_cv,
                     data = data_inputada,
                     preProcess = c("nzv","corr"))
set.seed(100)
model_knn_boot = train(TenYearCHD ~ ., method = "knn", trControl = ctrl_boot,
                     data = data_inputada,
                     preProcess = c("nzv","corr"))
#RF

set.seed(100)
model_rf_cv = train(TenYearCHD ~ ., method = "rf", trControl = ctrl_cv,
                    data = data_inputada,
                    preProcess = c("nzv","corr"))
set.seed(100)
model_rf_boot = train(TenYearCHD ~ ., method = "rf", trControl = ctrl_boot,
                    data = data_inputada,
                    preProcess = c("nzv","corr"))


# Comparando Modelos ------------------------------------------------------

resultados_cv = caret::resamples(
  list(
    svmPoly_cv = model_svmPoly_cv,
    svmRadial_cv = model_svmRadial_cv,
    glm_cv = model_glm_cv,
    knn_cv = model_knn_cv,
    rf_cv = model_rf_cv
  )
)
summary(resultados_cv)

resultados_boot = caret::resamples(
  list(
    svmPoly_boot = model_svmPoly_boot,
    svmRadial_boot = model_svmRadial_boot,
    glm_boot = model_glm_boot,
    knn_boot = model_knn_boot,
    rf_boot = model_rf_boot
  )
)
summary(resultados_boot)

#Modelo escolhido GLM com repeatedcv


# Treinando ---------------------------------------------------------------
library(tidyverse);library(caret)
load("chd.rda")

dummie = dummyVars(TenYearCHD ~ education + sex,
                   data = chd,
                   fullRank = T)

pred_dummie = predict(dummie,
                      newdata = chd)

chd_dummie = cbind(chd,pred_dummie)

chd_dummie = select(chd_dummie, -c(education,sex))

chd_dummie = chd_dummie %>% 
  mutate(education.2 = as.factor(education.2)) %>% 
  mutate(education.3 = as.factor(education.3)) %>% 
  mutate(education.4 = as.factor(education.4)) %>% 
  mutate(sexM = as.factor(sexM)) 

set.seed(100)

input = preProcess(chd_dummie,
                   method = "knnImpute",
                   k = 5)

data_inputada = predict(input, chd_dummie)
data_inputada = na.omit(data_inputada)
data = data_inputada

set.seed(100)
treino = createDataPartition(y =data$TenYearCHD,
                             p = 0.80,
                             list = F)

amostra_treino = data[treino,]
amostra_teste = data[-treino,]



set.seed(100)

ctrl_cv = trainControl(method="repeatedcv", number = 10, repeats = 3, 
                       preProcOptions = list(cutoff = 0.75, freqCut = 95/5, uniqueCut = 10))

lyncoln_modelfit = train(TenYearCHD ~ ., method = "glm", trControl = ctrl_cv,
                 data = amostra_treino,
                 preProcess = c("nzv","corr"))

predic = predict(lyncoln_modelfit,
                 newdata = amostra_teste)
confusionMatrix(predic,
                amostra_teste$TenYearCHD)

save(lyncoln_modelfit,file = "lyncolnsousa.RData")


