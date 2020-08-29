library(caret);library(dplyr)
load("banco_de_dados.rda")

# AdaBoost ----------------------------------------------------------------

#maxdeepth = 2
#mfinal = 450

seed = 216054055

set.seed(seed)
data_partition = createDataPartition(y = spam$type, p = 0.8, list = F)
treino = spam[data_partition,] %>% as.data.frame()

set.seed(seed)
data_processada = preProcess(treino,
                             method = c("knnImpute", "nzv", "corr"), 
                             freqCut = 95/5,
                             uniqueCut = 10,
                             cutoff = 0.8,
                             k = 5)

data_inputed_treino = predict(data_processada, treino)
data_inputed_validacao = predict(data_processada, validation)

ctrl = trainControl(method = "none")
tng = expand.grid(mfinal = 450,
                  maxdepth = 2,
                  coeflearn = c("Breiman"))

set.seed(seed)
modelo_ad = train(type ~ .,
                  data = data_inputed_treino,
                  method = "AdaBoost.M1",
                  trControl = ctrl,
                  tuneGrid = tng)

resultado = predict(modelo_ad,data_inputed_validacao)
confusao = confusionMatrix(resultado, data_inputed_validacao$type)

df_final = data.frame("maxdeepth" = 2,
                    "mfinal" = 450,
                    "Acurácia" = confusao$overall[1],
                    "Sensibilidade" = confusao$byClass[1],
                    "Especificidade" = confusao$byClass[2],
                    "Tempo de processamento" = modelo_ad$times$everything[3][[1]],
                    "Critério" = 0.5*confusao$overall[1] + 0.25*confusao$byClass[1] + 0.25*confusao$byClass[2])
writexl::write_xlsx(df_final,"df_final.xlsx")

save(data_processada,modelo_ad,file = "modelo_final.Rdata")
