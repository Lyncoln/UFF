library(mvtnorm)
library(corrgram)
#mu<-matrix(c(2,2,3,1), nrow=4) 		    #vetor de m?dias
#Sigma<-matrix(c(5,0,1,0,0,5,0,0,1,0,4,-1,0,0,-1,2), nrow=4) 	#matriz de vari?ncias e covari?ncias
#p=length(mu)

#mu<-matrix(c(100,20,50), nrow=3) 		    #vetor de m?dias
#Sigma<-matrix(c(5,2,0,2,3,1,0,1,2), nrow=3) 

x<-rmvnorm(100,mu, Sigma)
x
matcor=cor(x)
eigen(cov(x))

CP=prcomp(x)
summary(CP)
plot(CP, type="lines")


dados<-EPLDDE_AGRO
dados=dados[,6:22]

matcor=cor(dados)
corrgram(matcor,type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

CP=prcomp(dados)
CP
summary(CP)
plot(CP, type="lines")
abline(h=1,col="red")
paran(dados,10000)

CP2 = princomp(x,cor=TRUE)
summary(CP2)
plot(CP2, type="lines")
y<-var(x)
y


