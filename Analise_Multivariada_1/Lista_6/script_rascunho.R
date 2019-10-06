setwd("Analise_Multivariada//Lista_6")
library(dplyr)
#Leitura da base de dados

BD = read.csv("Dados.csv")

#Estou pegando os nomes da coluna Regiao

Regioes = BD %>% distinct(Regiao)
Regioes = as.vector(Regioes[,1])

#Estou criando uma lista que contem as bases de dados filtradas por cada Regiao

bases = lapply(Regioes, function(x) {select(filter(BD, Regiao == x),-1)})
names(bases) = Regioes

##Normalidades
##lapply(Regioes, function(x)print(mvShapiroTest::mvShapiro.Test(as.matrix(select(filter(BD,Regiao == x),-1)))))

#Depressao x Campanha
#Teste de normalidade

mvShapiroTest::mvShapiro.Test(as.matrix(bases$Depressao)) # P-valor > 0.05 => Normalidade
mvShapiroTest::mvShapiro.Test(as.matrix(bases$Campanha)) # P-valor > 0.05 => Normalidade

#Teste de variâncias
#H0: Sigma1 = Sigma2 x H1: Sigma1 =/= Sigma2

x = rbind(bases$Depressao,bases$Campanha)
x$Regiao = c(rep("Depressao",length(bases$Depressao[,1])),rep("Campanha",length(bases$Campanha[,1])))
biotools::boxM(x[,-7],x[,7])
MVTests::BoxM(x[,-7],x[,7])
#Variâncias iguais!
#Supor independência entre as regiões

ICSNP::HotellingsT2(bases$Depressao,bases$Campanha)
MVTests::TwoSamplesHT2(x[,-7],c(rep(1,40),rep(2,40))) %>% summary()
