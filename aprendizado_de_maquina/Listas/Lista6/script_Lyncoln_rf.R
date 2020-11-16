library(caret);library(dplyr)
load("banco_de_dados.rda")


df_rf = data.frame(0,0,0,0,0,0)
colnames(df_rf) = c("ntry",
                    "mtree",
                    "Acurácia",
                    "Sensibilidade",
                    "Especificidade",
                    "Tempo de processamento",
                    "Critério")
df_rf = df_rf[-1,]


ntree = c(500,1000,1500)
mtry = c(2,3,5,8,13)
for(i in mtry){
  for(j in ntree){
  data_partition = createDataPartition(y = spam$type, p = 0.8, list = F)
  treino = spam[data_partition,] %>% as.data.frame()
  teste = spam[-data_partition,] %>% as.data.frame()
  data_processada = preProcess(treino,
                               method = c("knnImpute", "nzv", "corr"), 
                               freqCut = 95/5, uniqueCut = 10, cutoff = 0.8, k = 5)
  data_inputed_treino = predict(data_processada, treino)
  data_inputed_teste = predict(data_processada, teste)
  ctrl = trainControl(method = "oob")
    tng = expand.grid(mtry = i)
    modelo_rf = train(type ~ .,
                      data = data_inputed_treino,
                      method = "rf",
                      ntree = j,
                      trControl = ctrl,
                      tuneGrid = tng)
    
    resultado = predict(modelo_rf,data_inputed_teste)
    confusao = confusionMatrix(resultado, data_inputed_teste$type)
    df_estatisticas = data.frame(i,
                                 j,
                           confusao$overall[1],
                           confusao$byClass[1],
                           confusao$byClass[2],
                           modelo_rf$times$everything[3][[1]],
                           0.5*confusao$overall[1] + 0.25*confusao$byClass[1] + 0.25*confusao$byClass[2]
                           )
    colnames(df_estatisticas) = c("ntry",
                                  "mtree",
                        "Acurácia",
                        "Sensibilidade",
                        "Especificidade",
                        "Tempo de processamento",
                        "Critério")
    df_rf = rbind(df_rf,df_estatisticas)
    row.names(df_rf) = NULL
  }
}
df_rf = df_rf %>% 
  arrange(-Critério)
writexl::write_xlsx(df_rf,"df_rf.xlsx")

