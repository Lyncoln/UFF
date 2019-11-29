#PACOTES
library(MASS)
library(mvtnorm)
library(ggplot2)
library(biotools)
#GERAÇÃO DOS DADOS

sigma1=matrix(c(2,1,1,2),2,2)
sigma2=matrix(c(2,0,0,2),2,2)
media1=c(5,5)
media2=c(4,3)
x1=rmvnorm(40,media1,sigma1)
plot(x1)
x2=rmvnorm(40,media2,sigma1)

x=rbind(x1,x2)
x=as.data.frame(x)
u1<-matrix("u1",40,1)
u2<-matrix("u2",40,1)
u=rbind(u1,u2)
x=cbind(x,u)
x

boxM(x[,-3],x[,3])
teste=hotelling.test(x1,x2)


nome=matrix("nome1",40,1)
x1=as.data.frame(x1)
x1=cbind(x1,nome)

nome=matrix("nome2",40,1)
x2=as.data.frame(x2)
x2=cbind(x2,nome)
#names(x2)=c("x1","nome")

dados=rbind.data.frame(x1,x2)
dados
summary(dados)
#GRÁFICO DOS DADOS
cores <- rainbow(length(levels(dados[, "nome"])))
plot(dados)
pairs(dados[, -3], pch = 21, bg = cores[dados$nome], lower.panel = NULL)

######################################################################
###################MÉTODO DA VALIDAÇÃO CRUZADA########################
######################################################################

#####VERIFICAR A PROPORÇÃO DE ELEMENTOS QUE SÃO MAL CLASSIFICADOS#####

#CV=T   CLASSIFICAÇÃO DE CADA ELEMENTO E PROBABILIDADE 
#DE CADA ELEMENTOS ESTAR EM CADA POPULAÇÃO  

lda11=lda(as.data.frame(dados[,-3]),dados$nome,CV=T)
lda11
classificacao =xtabs(~ lda11$class + nome, data = dados)
classificacao
pct_de_acertos=diag(classificacao)/colSums(classificacao)*100
pct_de_acertos

#####UTILIZAR A AMOSTRA PARA CLASSIFICAR NOVOS ELEMENTOS#########
#lda(x,grouping)
lda12=lda(as.data.frame(dados[,-3]),dados$nome,CV=F)
lda12$scaling


lda12_pred=predict(lda12, dados[,-3])
lda12_pred
lda12_pred$class

#################################################################
################  MÉTODO DA RESUBSTITUIÇÃO  #####################
#################################################################

lda2 = lda(nome ~ V1 + V2,data = dados)
lda2 
lda2$counts
lda2$scaling
names(lda2)
lda2$svd

lda2class = predict(lda2, dados)$class
names(predict(lda2, dados))
tabela2 = xtabs(~ lda2class + nome, data = dados)
cat("\n Matriz de confusão (com ressubstituição):")
tabela2

 dados_hist= as.matrix(dados[, -3]) %*% coef(lda2)
dim(dados_hist)

ldahist(dados_hist[, 1],  dados$nome)





