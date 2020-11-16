library(dplyr);library(caret);library(gbm);library(readr)

dataset <- read_csv("dataset.csv", 
                    na = "empty") %>% 
  select(-Id) %>% 
  mutate(LotFrontage = as.numeric(LotFrontage)) %>% 
  mutate(MasVnrArea = as.numeric(MasVnrArea)) %>% 
  mutate(GarageYrBlt = as.numeric(GarageYrBlt)) %>% 
  mutate_if(is.character,function(x){x = factor(x)}) 

matricula = as.integer((216054054 + 216054055)/2)

# Tratando base -----------------------------------------------------------

#Tratando GarageYrBlt
dataset = 
  dataset %>% 
  mutate(GarageYrBlt = 2020 - GarageYrBlt) %>% 
  mutate(GarageYrBlt = if_else(is.na(GarageYrBlt), 0, GarageYrBlt))
#Tratando LotFrontage
dataset = 
  dataset %>% 
  mutate(LotFrontage = if_else(LotFrontage<=60, "[0,60]",
                               if_else(LotFrontage<=110, "(60,110]",">110"))) %>% 
  mutate(LotFrontage = if_else(is.na(LotFrontage),"NA",LotFrontage)) %>% 
  mutate(LotFrontage = factor(LotFrontage))
#Tratando MasVnrArea
dataset =
  dataset %>% 
  mutate(MasVnrArea = ifelse(MasVnrArea<=100,"[0,100]",">100")) %>% 
  mutate(MasVnrArea = if_else(is.na(MasVnrArea),"NA",MasVnrArea)) %>%
  mutate(MasVnrArea = factor(MasVnrArea))


dataset = dataset %>% 
  janitor::clean_names()


# PP ----------------------------------------------------------------------
set.seed(matricula)
imputacao = preProcess(dataset[,-80],
                       method = c("pca","nzv","corr"),
                       thresh = 0.95,
                       freqCut = 95/5,
                       uniqueCut = 10,
                       cutoff = 0.85)
data_pp = predict(imputacao,
                  dataset) 
# data_pp$sale_price = (data_pp$sale_price - mean(data_pp$sale_price))/sd()
# Comparando --------------------------------------------------------------

grid = expand.grid(interaction.depth = c(1,2,3,5,8,13),
                   n.trees = c(500,1000,1500),
                   shrinkage = c(0.01,0.02,0.03),
                   n.minobsinnode = 10)


#Boots

ctrl_boot = trainControl(method = "boot",
                         number = 10,
                         savePredictions = TRUE)
set.seed(matricula)



modelo_boot = train(sale_price~ .,
                    data = data_pp,
                    method = "gbm",
                    distribution = "gaussian",
                    trControl = ctrl_boot,
                    tuneGrid = grid,
                    verbose = TRUE,
                    bag.fraction = 0.8)


tabela_boot = modelo_boot$results %>% 
  as_tibble() %>% 
  arrange(RMSE)

#shrinkage = 0.03 n.tress = 1500 interaction.depth = 5

modelo_boot$times
tabela_boot

modelo_boot
cumsum(summary(modelo_boot)$rel.inf)
#Somente 20 variáveis explicam 95% do gbm
summary(modelo_boot)$rel.inf
summary(modelo_boot)$rel.inf[1:20]
summary(modelo_boot)$var[1:20]



variaveis = c("exter_qual","bsmt_exposure","neighborhood","bsmt_fin_type1","kitchen_qual")

data_pp2 = data_pp %>% 
  select(all_of(variaveis),starts_with("PC"),sale_price)

ctrl_boot = trainControl(method = "boot",
                         number = 10,
                         savePredictions = TRUE)


set.seed(matricula)
modelo_boot2 = train(sale_price~ .,
                    data = data_pp2,
                    method = "gbm",
                    distribution = "gaussian",
                    trControl = ctrl_boot,
                    tuneGrid = grid,
                    verbose = TRUE,
                    bag.fraction = 0.8)

summary(modelo_boot2)
modelo_boot2$times

tabela_boot2 = modelo_boot2$results %>% 
  as_tibble() %>% 
  arrange(RMSE)


# aplicando modelo --------------------------------------------------------

#shrinkage = 0.03 interaction.depth = 5 n.tress = 1500


dataset <- read_csv("dataset.csv", 
                    na = "empty") %>% 
  select(-Id) %>% 
  mutate(LotFrontage = as.numeric(LotFrontage)) %>% 
  mutate(MasVnrArea = as.numeric(MasVnrArea)) %>% 
  mutate(GarageYrBlt = as.numeric(GarageYrBlt)) %>% 
  mutate_if(is.character,function(x){x = factor(x)}) 


#Tratando GarageYrBlt
dataset = 
  dataset %>% 
  mutate(GarageYrBlt = 2020 - GarageYrBlt) %>% 
  mutate(GarageYrBlt = if_else(is.na(GarageYrBlt), 0, GarageYrBlt))
#Tratando LotFrontage
dataset = 
  dataset %>% 
  mutate(LotFrontage = if_else(LotFrontage<=60, "[0,60]",
                               if_else(LotFrontage<=110, "(60,110]",">110"))) %>% 
  mutate(LotFrontage = if_else(is.na(LotFrontage),"NA",LotFrontage)) %>% 
  mutate(LotFrontage = factor(LotFrontage))
#Tratando MasVnrArea
dataset =
  dataset %>% 
  mutate(MasVnrArea = ifelse(MasVnrArea<=100,"[0,100]",">100")) %>% 
  mutate(MasVnrArea = if_else(is.na(MasVnrArea),"NA",MasVnrArea)) %>%
  mutate(MasVnrArea = factor(MasVnrArea))


dataset = dataset %>% 
  janitor::clean_names()

# PP ----------------------------------------------------------------------

set.seed(matricula)
data_partition = createDataPartition(y = dataset$sale_price, p = 0.80, list = F)
treino = dataset[data_partition,] %>% as.data.frame()
teste = dataset[-data_partition,] %>% as.data.frame()

set.seed(matricula)
imputacao = preProcess(treino[,-80],
                       method = c("pca","nzv","corr"),
                       thresh = 0.95,
                       freqCut = 95/5,
                       uniqueCut = 10,
                       cutoff = 0.85)

variaveis = c("exter_qual","bsmt_exposure","neighborhood","bsmt_fin_type1","kitchen_qual")

data_treino_pp = predict(imputacao,treino) %>% 
  select(all_of(variaveis),starts_with("PC"),sale_price)

data_teste_pp = predict(imputacao, teste) %>% 
  select(all_of(variaveis),starts_with("PC"),sale_price)


# aplicando modelo  -------------------------------------------------------


grid = expand.grid(interaction.depth = 5,
                   n.trees = 1500,
                   shrinkage = 0.03,
                   n.minobsinnode = 10)


modelo_final = train(sale_price~ .,
                     data = data_treino_pp,
                     method = "gbm",
                     distribution = "gaussian",
                     trControl = ctrl_boot,
                     tuneGrid = grid,
                     verbose = TRUE,
                     bag.fraction = 0.8)

summary(modelo_final)
modelo_final$times

predicao = predict(modelo_final,
                   data_teste_pp)

resultado = postResample(predicao,data_teste_pp$sale_price)
resultado

#save(modelo_boot,modelo_boot2,file = "modelo_bck2.Rdata")


# Criando função ----------------------------------------------------------

aplica_modelo = function(data){
  id = data %>% 
    select(Id)
  
  data = data %>% 
  select(-Id) %>% 
    mutate(LotFrontage = as.numeric(LotFrontage)) %>% 
    mutate(MasVnrArea = as.numeric(MasVnrArea)) %>% 
    mutate(GarageYrBlt = as.numeric(GarageYrBlt)) %>% 
    mutate_if(is.character,function(x){x = factor(x)}) 
  
  
  #Tratando GarageYrBlt
  data =
    data %>%
    mutate(GarageYrBlt = 2020 - GarageYrBlt) %>%
    mutate(GarageYrBlt = if_else(is.na(GarageYrBlt), 0, GarageYrBlt))
  # #Tratando LotFrontage
  data =
    data %>%
    mutate(LotFrontage = if_else(LotFrontage<=60, "[0,60]",
                                 if_else(LotFrontage<=110, "(60,110]",">110"))) %>%
    mutate(LotFrontage = if_else(is.na(LotFrontage),"NA",LotFrontage)) %>%
    mutate(LotFrontage = factor(LotFrontage))
  # #Tratando MasVnrArea
  data =
    data %>%
    mutate(MasVnrArea = ifelse(MasVnrArea<=100,"[0,100]",">100")) %>%
    mutate(MasVnrArea = if_else(is.na(MasVnrArea),"NA",MasVnrArea)) %>%
    mutate(MasVnrArea = factor(MasVnrArea))
   data = data %>%
    janitor::clean_names()
   
   variaveis = c("exter_qual","bsmt_exposure","neighborhood","bsmt_fin_type1","kitchen_qual")

   data = predict(imputacao,data)%>%
     select(all_of(variaveis),starts_with("PC"),sale_price)


   predicao = predict(modelo_final,
                      data)

   #return(postResample(predicao,data$sale_price))
   return(tibble(id,Preco_predito = predicao))
   
}

dataset <- read_csv("dataset.csv",
                    na = "empty")

set.seed(matricula)

data_partition = createDataPartition(y = dataset$SalePrice, p = 0.80, list = F)

treino = dataset[data_partition,] %>% as.data.frame()

teste = dataset[-data_partition,] %>% as.data.frame()

aplica_modelo(teste)

#save(imputacao,modelo_boot,modelo_boot2,modelo_final,aplica_modelo,file = "objetos_atvfinal_boot.Rdata")
