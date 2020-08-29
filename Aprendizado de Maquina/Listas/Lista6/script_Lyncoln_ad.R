library(caret);library(dplyr)
load("banco_de_dados.rda")



df_ad = data.frame(0,0,0,0,0,0)
colnames(df_ad) = c("maxdeepth",
                    "mfinal",
                    "Acurácia",
                    "Sensibilidade",
                    "Especificidade",
                    "Tempo de processamento",
                    "Critério")
df_ad = df_ad[-1,]


profundidade = c(1,2,3,5,8)
mfinal = c(150,300,450)
for(i in profundidade){
  for(j in mfinal){
    data_partition = createDataPartition(y = spam$type, p = 0.8, list = F)
    treino = spam[data_partition,] %>% as.data.frame()
    teste = spam[-data_partition,] %>% as.data.frame()
    data_processada = preProcess(treino,
                                 method = c("knnImpute", "nzv", "corr"), 
                                 freqCut = 95/5, uniqueCut = 10, cutoff = 0.8, k = 5)
    data_inputed_treino = predict(data_processada, treino)
    data_inputed_teste = predict(data_processada, teste)
    ctrl = trainControl(method = "none")
    tng = expand.grid(mfinal = j,
                      maxdepth = i,
                      coeflearn = c("Breiman"))
    modelo_ad = train(type ~ .,
                      data = data_inputed_treino,
                      method = "AdaBoost.M1",
                      trControl = ctrl,
                      tuneGrid = tng)
    
    resultado = predict(modelo_ad,data_inputed_teste)
    confusao = confusionMatrix(resultado, data_inputed_teste$type)
    df_estatisticas = data.frame(i,
                                 j,
                                 confusao$overall[1],
                                 confusao$byClass[1],
                                 confusao$byClass[2],
                                 modelo_ad$times$everything[3][[1]],
                                 0.5*confusao$overall[1] + 0.25*confusao$byClass[1] + 0.25*confusao$byClass[2]
    )
    colnames(df_estatisticas) = c("maxdeepth",
                                  "mfinal",
                                  "Acurácia",
                                  "Sensibilidade",
                                  "Especificidade",
                                  "Tempo de processamento",
                                  "Critério")
    df_ad = rbind(df_ad,df_estatisticas)
    row.names(df_ad) = NULL
  }
}
df_ad = df_ad %>% 
  arrange(-Critério)
writexl::write_xlsx(df_ad,"df_ad.xlsx")
