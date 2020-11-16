#setwd("Analise_Multivariada//Lista_6")
library(dplyr)
#Leitura da base de dados

BD = read.csv("Dados.csv")

testa_var = function(base){
  resultado = c()
  combinacoes = combn(as.vector(unique(base[,1])),2)
  for(i in 1:length(combinacoes[1,])){
    x1 = combinacoes[,i][1]
    x2 = combinacoes[,i][2]
    bd_aux = BD %>% filter(Regiao==x1 | Regiao ==x2)#
    bd_aux = arrange(bd_aux, Regiao)#
    teste = MVTests::BoxM(as.vector(bd_aux[,-1]),as.vector(bd_aux[,1]))
    resultado = c(resultado, paste0(x1," x ",x2," P-valor: ", teste$p.value))
  }
  return(resultado)
}

testa_var(BD)

testa_media = function(base){
  resultado = c()
  combinacoes = combn(as.vector(unique(base[,1])),2)
  for(i in 1:length(combinacoes[1,])){
    x1 = combinacoes[,i][1]
    x2 = combinacoes[,i][2]
    bd_aux = BD %>% filter(Regiao==x1 | Regiao ==x2)#
    bd_aux = arrange(bd_aux, Regiao)
    teste = MVTests::BoxM(as.vector(bd_aux[,-1]),as.vector(bd_aux[,1]))
    if(teste$p.value > 0.05){
      teste2 = MVTests::TwoSamplesHT2(bd_aux[,-1],c(rep(1,40),rep(2,40)))#
      resultado = c(resultado, paste0(x1," x ",x2," p-valor: ",teste2$p.value," Variâncias Iguais"))
    }
    else{
      teste2 = MVTests::TwoSamplesHT2(bd_aux[,-1],c(rep(1,40),rep(2,40)),Homogenity = F)#
      resultado = c(resultado, paste0(x1," x ",x2," p-valor: ",teste2$p.value," Variâncias diferentes"))
    }
  }
  return(resultado)
}

testa_media(BD)


#Estou pegando os nomes da coluna Regiao

Regioes = BD %>% distinct(Regiao)
Regioes = as.vector(Regioes[,1])

## Estou criando uma lista que contem as bases de dados filtradas por cada Regiao
# bases = lapply(Regioes, function(x) {select(filter(BD, Regiao == x),-1)})
# names(bases) = Regioes


for(i in Regioes){
  print(paste0("Intervalo de confiança para: ",i))
  print(MVTests::OneSampleHT2(select(filter(BD,Regiao==i),-1), mu = rep(0,6))$CI)

  
}
