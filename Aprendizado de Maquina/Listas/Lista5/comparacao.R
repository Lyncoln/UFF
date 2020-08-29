library(caret);library(randomForest);library(dplyr)
seed = 216054055
#Após ter selecionado 3 pares de hyper parametros, iremos realizar a comparação
load("Banco_de_dados.rda")


input = preProcess(as.data.frame(data),
                   method = c("knnImpute","nzv","corr"),
                   freqCut = 95/5,
                   uniqueCut = 10,
                   cutoff = 0.8,
                   k = 5)

data_inputada = predict(input, data)

set.seed(seed)
rf_m7_n4331 = randomForest(diagnosis ~ .,
                           data = data_inputada,
                           proximity = T,
                           ntree = 4331,
                           mtry = 7)

set.seed(seed)
rf_m3_n916 = randomForest(diagnosis ~ .,
                           data = data_inputada,
                           proximity = T,
                           ntree = 916,
                           mtry = 3)

set.seed(seed)
rf_m1_n1067 = randomForest(diagnosis ~ .,
                           data = data_inputada,
                           proximity = T,
                           ntree = 1067,
                           mtry = 1)

calculo_rf = function(modelo){
  aux = modelo
  aux = aux$confusion
  acuracia = (aux[1,1] + aux[2,2])/(sum(aux[1:2,1:2]))
  sensibilidade = aux[1,1]/(aux[1,1]+aux[2,1])
  especificidade = aux[2,2]/(aux[2,2]+aux[1,2])
  resultado = data.frame(modelo$mtry,modelo$ntree,acuracia,sensibilidade,especificidade)
  colnames(resultado) = c("mtry","ntree","Acurácia","Sensibilidade", "Especificidade")
  return(resultado)
}

#Comparação
{
resultados = data.frame()
resultados = rbind(calculo_rf(rf_m1_n1067),resultados)
resultados = rbind(calculo_rf(rf_m3_n916),resultados)
resultados = rbind(calculo_rf(rf_m7_n4331),resultados)
resultados
}

#Modelo escolhido será aquele com mtry = 7 e ntree = 4331