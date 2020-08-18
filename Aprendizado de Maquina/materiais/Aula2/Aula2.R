library(kernlab)
library(dplyr)

#Carregar banco de dados
data(spam)

#Fixamos a posicao na tabela aleatoria
set.seed(100)

#Sorteamos 10 observacoes do banco SPAM
sorteio <- sample(dim(spam)[1],size=10)

small.spam<-spam[sorteio,]

#Vamos plotar os dados 
plot(small.spam$capitalAve[small.spam$type=="spam"],
     col="red",ylim=c(0,12),xlim=c(1,7),
     ylab="capitalAve",pch=19)
points(small.spam$capitalAve[small.spam$type=="nonspam"]
       ,col="blue",pch=19)

#Colocamos linhas para separar spam de nao spam
abline(h=1.66,col="blue")
abline(h=5.5,col=" red")

### Regra para minimizar o "in sample error"

regra1<-select(small.spam,capitalAve,type)
regra1<-arrange(regra1,capitalAve)
regra1<-mutate(regra1,predic=NA)

for(i in 1:10){if (regra1$capitalAve[i] <=1.66)
  {regra1$predic[i]<-"nonspam"}
  {if (regra1$capitalAve[i] > 1.66  & regra1$capitalAve[i] 
       <=5.5){regra1$predic[i]<-"spam"} }
  {if (regra1$capitalAve[i] > 5.5){regra1$predic[i]<-"nonspam"} 
    }
}

table(regra1$predic,regra1$type)

#in sample error: 0%

####Regra simplificada

#Plotar dados
plot(small.spam$capitalAve[small.spam$type=="spam"],
     col="red",ylim=c(0,12),xlim=c(1,7),
     ylab="capitalAve",pch=19)
points(small.spam$capitalAve[small.spam$type=="nonspam"]
       ,col="blue",pch=19)

#Colocamos linhas para separar spam de nao spam
abline(h=2.5,lty=2)

### Regra simplificada
regra2<-select(small.spam,capitalAve,type)
regra2<-arrange(regra2,capitalAve)
regra2<-mutate(regra2,predic=NA)

for(i in 1:10){
regra2$predic[i] <- ifelse(regra2$capitalAve[i]<= 2.5,"nonspam","spam")
}

table(regra2$predic,regra2$type)/dim(regra2)[1]

#in sample error: 20%

####Vamos aplicar a regra1 em todo o banco.

#Selecionamos apenas as varíaveis de interesse
regra1total<-select(spam,capitalAve,type)
#Criamos uma nova variável, para inserir o valor
#da predicao
regra1total<-mutate(regra1total,predic=NA)

for(i in 1:{dim(regra1total)[1]})
{if (regra1total$capitalAve[i] <=1.66){regra1total$predic[i]<-"nonspam"}
  {if (regra1total$capitalAve[i] > 1.66  & regra1total$capitalAve[i] <=5.5){regra1total$predic[i]<-"spam"} }
  {if (regra1total$capitalAve[i] > 5.5){regra1total$predic[i]<-"nonspam"} 
    }
}

table(regra1total$predic,regra1total$type)

table(regra1total$predic,
      regra1total$type)/dim(regra1total)[1]

#Erro total: 49,79%
0.3523147+0.1456205

####Vamos aplicar a regra2 em todo o banco.

#Selecionamos apenas as varíaveis de interesse
regra2total<-select(spam,capitalAve,type)
#Criamos uma nova variável, para inserir o valor
#da predicao
regra2total<-mutate(regra2total,predic=NA)

for(i in 1:{dim(regra2total)[1]}){
  regra2total$predic[i] <- ifelse(regra2total$capitalAve[i] <= 2.5,"nonspam","spam")
}

table(regra2total$predic,regra2total$type)/dim(regra2total)[1]

#erro total: 27,75%
0.1593132+0.1182352


