library(caret);library(randomForest);library(dplyr)
load("Banco_de_dados.rda")
seed = 216054055

set.seed(seed)
data_partition = createDataPartition(y = data$diagnosis, p = 0.8, list = F)
treino = data[data_partition,] %>% as.data.frame()
teste = data[-data_partition,] %>% as.data.frame()

set.seed(seed)
data_processada = preProcess(treino,
                             method = c("knnImpute", "nzv", "corr"), 
                     freqCut = 95/5, uniqueCut = 10, cutoff = 0.8, k = 5)

set.seed(seed)

data_inputed_treino = predict(data_processada, treino)

set.seed(seed)
lyncoln_rf <- randomForest(diagnosis ~ .,
                      data = data_inputed_treino ,
                      mtry = 7,
                      ntree = 4331,
                      proximity = T)


aplica_pp_modelo = function(data_base, pp = data_processada, modelo = lyncoln_rf){
  library(randomForest);library(caret)
  data_pp = predict(pp, data_base)
  resultado = predict(modelo, data_pp)
  caret::confusionMatrix(resultado, data_pp$diagnosis)
}

aplica_pp_modelo(teste)

save(data_processada, lyncoln_rf,aplica_pp_modelo, file="lyncoln.Rdata")
