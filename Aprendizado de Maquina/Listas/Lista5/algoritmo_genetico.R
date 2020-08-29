library(caret);library(randomForest);library(dplyr)


matricula = 216054055
load("Banco_de_dados.rda")
data_base = data

#Estou criando um procedimento de algorítmo genético para tentar maximizar 
#Os hyper parametros

primeira_pop = function(tamanho, maximo = 5000,data = data_base){
  ###
  mtry = sample(1:(ncol(data)-1), tamanho,replace = T)
  ntree = sample(1:maximo, tamanho, replace = T)
  populacao = data.frame(mtry = mtry,
                         ntree = ntree)
  ###
  input = preProcess(as.data.frame(data),
                     method = c("knnImpute","nzv","corr"),
                     freqCut = 95/5,
                     uniqueCut = 10,
                     cutoff = 0.8,
                     k = 5)
  data_inputada = predict(input, data)
  modelos = apply(populacao,1,function(x){randomForest(diagnosis ~ .,
                                              data = data_inputada,
                                              proximity = T,
                                              ntree = x[2],
                                              mtry = x[1])})
  return(modelos)
}

fitness_function = function(modelo){
  aux = modelo$confusion
  acuracia = (aux[1,1] + aux[2,2])/(sum(aux[1:2,1:2]))
  sensibilidade = aux[1,1]/(aux[1,1]+aux[2,1])
  especificidade = aux[2,2]/(aux[2,2]+aux[1,2])
  estatisticas = c(acuracia,sensibilidade,especificidade)
  hyper = c(modelo$mtry,modelo$ntree)
  resultado = 3*estatisticas[1] + 1.5* estatisticas[2] + 1.5*estatisticas[3]
  return(data.frame(mtry = hyper[1],
                    ntree = hyper[2],
                    resultado = resultado))
}

fitness_function_all = function(populacao){
  data_fit = data.frame()
  for(elemento in populacao){
    data_fit = rbind(data_fit,fitness_function(elemento))
  }
  row.names(data_fit)= c()
  data_fit = data_fit %>% arrange(resultado)
  data_fit = data_fit %>% mutate(ind = 1:nrow(data_fit))
  data_fit = data_fit %>% mutate(prob = ind/sum(ind))
  data_fit = data_fit %>% select(-ind) %>% arrange(-prob)
  return(data_fit)
}

selecao = function(data){
  num_sort = 2
  linhas = sample(1:nrow(data),num_sort,prob = data$prob)
  return(data[linhas,])
}

crossover = function(data, n){
  ####
  filhos = data.frame()
  for(i in 1:n){
    prob = runif(1,0,1)
    mtry = as.integer(prob*data[1,1]) + as.integer((1-prob)*data[2,1])
    ntree = as.integer(prob*data[1,2]) + as.integer((1-prob)*data[2,2])    
    if(mtry<=0) mtry = 1
    if(mtry>=30) mtry = 30
    filho = data.frame(mtry = mtry,
                       ntree = ntree)
    filhos = rbind(filhos,filho)
  }
  ####
  return(filhos)
}


mutacao = function(data){
  num_obs = nrow(data)
  for(elemento in 1:num_obs){
    chanc_mut = abs(runif(1))
    if(chanc_mut <= 0.05){
      mtry = data[elemento,1] + sample(-1:1,1)
      mtree = data[elemento,2] + sample(-100:100,1)
      if(mtry<=0) mtry = 1
      if(mtry>=30) mtry = 30
      data[elemento,1] = mtry
      data[elemento,2] = mtree
    }
  }
  return(data)
}

treino_filhos = function(filhos,data=data_base){
  input = preProcess(as.data.frame(data),
                     method = c("knnImpute","nzv","corr"),
                     freqCut = 95/5,
                     uniqueCut = 10,
                     cutoff = 0.8,
                     k = 5)
  data_inputada = predict(input, data)
  modelos = apply(filhos,1,function(x){randomForest(diagnosis ~ .,
                                                    data = data_inputada,
                                                    proximity = T,
                                                    ntree = x[2],
                                                    mtry = x[1])})
  return(modelos)
}


# execução do algoritmo ---------------------------------------------------

pop_init = primeira_pop(10)
fit_pop = fitness_function_all(pop_init)

num_gem = 20
for( i in 1:num_gem){
  pais = selecao(fit_pop)
  filhos = crossover(pais,10)
  filhos = mutacao(filhos)
  filhos = treino_filhos(filhos)
  fit_pop = fitness_function_all(filhos)
}
pais = selecao(fit_pop)

# mtry = 7
# ntree = 4331
# 
# m = 3
# n = 916
# 
# m = 1
# ntree = 1067