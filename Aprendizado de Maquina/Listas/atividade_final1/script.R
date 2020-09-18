library(caret);library(dplyr);library(pROC)

matricula = 216054055

data = read.csv("https://query.data.world/s/6uhvcowok54gcj5jz3i2ccwldlb6rl", 
         header=TRUE, stringsAsFactors=FALSE) %>% 
  select(-Name) %>% 
  mutate(TARGET_5Yrs = factor(.$TARGET_5Yrs,
                              levels = c(0,1),
                              labels = c("Não","Sim")))

data = data %>% 
  janitor::clean_names()

tabela = do.call(cbind, lapply(data[,-20],function(x){summary(x,digits = 3)})) %>% 
  .[-7,] %>% 
  as.data.frame() 
na = lapply(data[,-20],function(x){sum(is.na(x))}) %>% 
  as.matrix() %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate_all(function(x){unlist(x)})
row.names(na) = "NA"

tabela = rbind(tabela,na) 

prop_resposta =
  ggplot(data,aes(x = target_5yrs,fill = target_5yrs)) + 
  geom_bar() +
  geom_text(aes(label= scales::percent(..count../sum(..count..))),
            stat="count",
            vjust = -0.2)+
  ylab("Frequência")+
  guides(fill=F)


# PP ----------------------------------------------------------------------
set.seed(matricula)
imputacao = preProcess(data,
                       method = "knnImpute",
                       k = 5)
data_pp = predict(imputacao,
                  data)
nearZeroVar(data_pp,
            saveMetrics = T)

# Nenhuma variável é nzv

correlacao = cor(data_pp[,-20],
                 method = "pearson")

summary(correlacao[upper.tri(correlacao)])

colunas = findCorrelation(correlacao,
                          cutoff = 0.85,
                          verbose = T,
                          names = T)

data_pp = data_pp %>% 
  select(!all_of(colunas))

findLinearCombos(data_pp[,c(1:13)])


# Escolhendo modelo --------------------------------------------------------

grid = expand.grid(interaction.depth = c(1,2,3,5,8),
                   n.trees = c(500,1000,1500,2500),
                   shrinkage = c(0.01,0.02,0.03),
                   n.minobsinnode = 10)
#Boots

ctrl_boot = trainControl(method = "boot",
                         number = 10,
                         classProbs=TRUE,
                         summaryFunction = twoClassSummary,
                         savePredictions = TRUE
                        )
set.seed(matricula)
modelo_boot = train(target_5yrs ~ .,
                    data = data_pp,
                    method = "gbm",
                    distribution = "bernoulli",
                    trControl = ctrl_boot,
                    tuneGrid = grid,
                    metric="ROC",
                    verbose = FALSE,
                    bag.fraction = 0.8)

tabela_boot = modelo_boot$results %>% 
  as_tibble() %>% 
  arrange(-ROC)

tabela_boot

predicao_boot = predict(modelo_boot,data_pp,type = "prob")[,2]

#CV
ctrl_cv = trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3,
                       classProbs=TRUE,
                       summaryFunction = twoClassSummary,
                       savePredictions = TRUE)

set.seed(matricula)
modelo_cv = train(target_5yrs ~ .,
                  data = data_pp,
                  method = "gbm",
                  distribution = "bernoulli",
                  trControl = ctrl_cv,
                  tuneGrid = grid,
                  metric="ROC",
                  verbose = FALSE,
                  bag.fraction = 0.8)

tabela_cv = modelo_cv$results %>% 
  as_tibble() %>% 
  arrange(-ROC)

tabela_cv

predicao_cv = predict(modelo_cv,data_pp,type = "prob")[,2]


par(pty = "s")
roc(data_pp$target_5yrs, predicao_boot, plot = TRUE, col = "red",lwd = 3,legacy.axes = TRUE, 
    print.auc = TRUE, print.auc.x = 0.6, print.auc.y = 0.35,
    xlab = "Proporção Falsos Positivos", ylab = "Proporção Verdadeiros Positivos")
plot.roc(data_pp$target_5yrs, predicao_cv, add = TRUE, col = "blue",lwd = 2,
         print.auc = TRUE, print.auc.x = 0.6, print.auc.y = 0.2)
par(pty = "m")

tempo_processamento = tibble(metodo = c("Bootstrap","repeatedcv"),
                             tempo  = c(modelo_boot$times$everything[3],
                                          modelo_cv$times$everything[3]))
tempo_processamento



graf_tempo = 
ggplot(tempo_processamento,aes(x = metodo,y = tempo,fill = metodo)) + 
  geom_bar(stat = "identity") +
  guides(fill=F) +
  geom_text(aes(label = paste0(tempo,'s')),  vjust = -0.2)
# Aplicação do modelo escolhido -------------------------------------------
# n.tres = 1000 ; shrinkage = 0.01 ; interaction.dep = 1

data = read.csv("https://query.data.world/s/6uhvcowok54gcj5jz3i2ccwldlb6rl", 
                header=TRUE, stringsAsFactors=FALSE) %>% 
  select(-Name) %>% 
  mutate(TARGET_5Yrs = factor(.$TARGET_5Yrs,
                              levels = c(0,1),
                              labels = c("Não","Sim")))

#PP
colunas #Variáveis para serem retiradas
data = data %>% 
  janitor::clean_names() %>% 
  select(-colunas)

set.seed(matricula)
data_partition = createDataPartition(y = data$target_5yrs, p = 0.75, list = F)
treino = data[data_partition,] %>% as.data.frame()
teste = data[-data_partition,] %>% as.data.frame()

set.seed(matricula)
imputacao = preProcess(treino,
                       method = "knnImpute",
                       k = 5)
data_treino_pp = predict(imputacao,
                         treino)
data_teste_pp = predict(imputacao,
                         teste)

ctrl = trainControl(method = "repeatedcv",
                    number = 10,
                    repeats = 3,
                    classProbs=TRUE,
                    summaryFunction = twoClassSummary,
                    savePredictions = TRUE)

grid = expand.grid(interaction.depth = 1,
                   n.trees = 1000,
                   shrinkage = 0.01,
                   n.minobsinnode = 10)

set.seed(matricula)
modelo = train(target_5yrs ~ .,
               data = data_treino_pp,
               method = "gbm",
               distribution = "bernoulli",
               trControl = ctrl,
               tuneGrid = grid,
               metric="ROC",
               verbose = FALSE,
               bag.fraction = 0.8)

summary(modelo)
predicao = predict(modelo,data_teste_pp,type = 'prob')[,2]
roc(data_teste_pp$target_5yrs,predicao)

otimizador<-function(predicao,resposta){
  require(ROCR)
  predicao2 = prediction(predicao, resposta)
  performance = performance(predicao2,"tpr","fpr")
  funcao <- attr(performance, "y.values")[[1]] -
    (attr(performance, "x.values")[[1]])
  c <-  performance@alpha.values[[1]][which.max(funcao)]
  plot(performance, colorize=T, lwd=2)
  c
}

valor_corte = otimizador(predicao,data_teste_pp$target_5yrs)
predicao2 = predicao >= valor_corte
predicao2 = factor(predicao2,levels = c(FALSE,TRUE), labels = c("Não","Sim"))
resultado = confusionMatrix(predicao2,
                data_teste_pp$target_5yrs)

#Usar na apresentação
save(tabela,
     prop_resposta,
     colunas,
     modelo_boot,
     modelo_cv,
     tabela_boot,
     tabela_cv,
     graf_tempo,
     modelo,
     valor_corte,
     resultado,
     file = "objetos.Rdata"
     )
