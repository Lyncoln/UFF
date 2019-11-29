#PACOTES
library(MASS)
library(mvtnorm)
library(ggplot2)

#GERAÇÃO DOS DADOS

# sigma1=matrix(c(2,1,1,2),2,2)
# sigma2=matrix(c(2,0,0,2),2,2)
# media1=c(5,5)
# media2=c(4,3)
# x1=rmvnorm(40,media1,sigma1)
# plot(x1)
# x2=rmvnorm(40,media2,sigma1)
# 
# # x1=rnorm(40,3,2)
# # x2=rnorm(40,6,1)
# 
# nome=matrix("nome1",40,1)
# x1=as.data.frame(x1)
# x1=cbind(x1,nome)
# 
# nome=matrix("nome2",40,1)
# x2=as.data.frame(x2)
# x2=cbind(x2,nome)
# #names(x2)=c("x1","nome")
# 
# dados=rbind.data.frame(x1,x2)
dados=iris
dados
summary(dados)
#GRÁFICO DOS DADOS
cores <- rainbow(length(levels(dados[, "Species"])))
pairs(dados[, -5], pch = 21, bg = cores[dados$Species], lower.panel = NULL)

######################################################################
###################MÉTODO DA VALIDAÇÃO CRUZADA########################
######################################################################

#####VERIFICAR A PROPORÇÃO DE ELEMENTOS QUE SÃO MAL CLASSIFICADOS#####

#CV=T   CLASSIFICAÇÃO DE CADA ELEMENTO E PROBABILIDADE 
#DE CADA ELEMENTOS ESTAR EM CADA POPULAÇÃO  

lda11=lda(as.data.frame(dados[,-5]),dados$Species,CV=T)
lda11
classificacao =xtabs(~ lda11$class + Species, data = dados)
classificacao
pct_de_acertos=diag(classificacao)/colSums(classificacao)*100
pct_de_acertos

#####UTILIZAR A AMOSTRA PARA CLASSIFICAR NOVOS ELEMENTOS#########
#lda(x,grouping)
lda12=lda(as.data.frame(dados[,-5]),dados$Species,CV=F)
lda12$scaling


lda12_pred <- predict(lda12, dados[,-5])
lda12_pred
lda12_pred$class

#################################################################
################  MÉTODO DA RESUBSTITUIÇÃO  #####################
#################################################################
names(dados)
lda2 = lda(Species ~ Sepal.Length +Sepal.Width + Petal.Length +Petal.Width,data = dados)
lda2 
lda2$lev
lda2$scaling
names(lda2)
lda2$svd

lda2class = predict(lda2, dados)$class
names(predict(lda2, dados))
tabela2 = xtabs(~ lda2class + Species, data = dados)
cat("\n Matriz de confusão (com ressubstituição):")
tabela2


dados_hist = as.matrix(dados[, -5]) %*% coef(lda2)
dim(dados_hist)

ldahist(dados_hist[, 1],  dados$Species)
ldahist(dados_hist[, 2],  dados$Species)
#ldahist(FD[, 2], dados$nome)

x=c(1,3,4,5)
x=as.data.frame(t(x))
names(x)=c("Sepal.Length", "Sepal.Width" , "Petal.Length" ,"Petal.Width")
predict(lda2,x)$class

plot(dados_hist[, 1], dados_hist[, 2], pch = 20, col = cores,
     xlab = "Função discriminante 1", ylab = "Função discriminante 2")
points(dados_hist[, 1], dados_hist[, 2], pch = 13, col = cores, cex = 1.5)
text(dados_hist[, 1], dados_hist[, 2], lda2$lev)

