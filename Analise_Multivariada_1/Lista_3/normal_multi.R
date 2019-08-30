

#------------------- Normal bivariada com elementos não correlacionados ----------------------
#install.packages("grDevices")
library(grDevices) 
#install.packages("mvtnorm")
library(mvtnorm)


# Distribuição normal multivariada: Exemplo normal bivariada

mu<-matrix(c(1,1), nrow=2) 		    #vetor de médias
Sigma<-matrix(c(7,0,0,7), nrow=2) 	#matriz de variâncias e covari?ncias
p=length(mu)

# Densidade da normal bivariada

x1 <- seq(-10, 10, length= 50)
x1
x2 <- x1
f <- matrix(0, nrow=length(x1), ncol=length(x2))

for (i in 1:length(x1))
  for(j in 1: length(x2))
    f[i,j] <- dmvnorm(c(x1[i],x2[j]), mean=mu, sigma=Sigma)

persp(x1, x2, f, theta = 70, phi = 30, col = "lightblue", ticktype = "detailed")


# Contornos elípticos

contour(x1, x2, f, draw=T, nlevels=20, labcex=0.8, xlab=expression(x[1]),ylab=expression(x[2]), drawlabels=FALSE)


#------------------- Normal bivariada com elementos correlacionados ----------------------



mu<-matrix(c(1,0), nrow=2) 		#vetor de m?dias
Sigma<-matrix(c(2,1,1,1), nrow=2) 	#matriz de vari?ncias e covari?ncias
p=length(mu)

# Densidade da normal bivariada

library(grDevices) 
library(mvtnorm)

x1 <- seq(-5, 5, length= 50)
x2 <- x1
f <- matrix(0, nrow=length(x1), ncol=length(x2))

for (i in 1:length(x1))
  for(j in 1: length(x2))
    f[i,j] <- dmvnorm(c(x1[i],x2[j]), mean=mu, sigma=Sigma)

persp(x1, x2, f, theta = 70, phi = 30, col = "red", ticktype = "detailed")


# Contornos el?pticos

contour(x1, x2, f, draw=T, nlevels=20, labcex=0.8, xlab=expression(x[1]),ylab=expression(x[2]), drawlabels=FALSE)


# Gerando amostras da normal multivariada
?rmvnorm
rmvnorm(100, mean=mu, sigma=Sigma)
Sigma
pairs(rmvnorm(100, mean=mu, sigma=Sigma))


#------------------- Verificando graficamente normalidade no caso univariado ----------------------

data(iris)
names(iris)
library(stats)

for (i in 1:4)
{
  print(shapiro.test(iris[,i]))
}

par(mfrow=c(2,2))
for (i in 1:4)
{
  qqnorm(iris[,i])
  qqline(iris[,i])
}

#------------------- Verificando graficamente normalidade no caso multivariado ----------------------

x=as.matrix(iris[,1:4])

#vetor de m?dias amostrais
m=apply(x,2,mean)
m
#matriz de variâncias e covariâncias amostrais
S=var(x)
S

invS=solve(S)
d = q = NULL
n= nrow(x)
p= ncol(x)

for (i in 1:n){
  d = c(d, (x[i,]-m) %*% invS %*% matrix(x[i,]-m,ncol=1))
  prob = (i-0.5)/n
  q = c(q, qchisq(prob, df=p))
}
d=sort(d)
plot(d,q)
abline(a=0,b=1)



#------------------- Verificando normalidade no caso multivariado ----------------------

install.packages("mvShapiroTest")
library(mvShapiroTest)
?mvShapiro.Test
mvShapiro.Test(as.matrix(iris[,1:4]))


######Exercício
#1) utilizando o vetor de medias e a matriz de variancias abaixo:
#a) Crie uma matriz (n=100) de var aleatórias com dist. normal multivariada.
#b) Verifique a normalidade graficamente e pelo teste.
#C) Verifique graficamente a densidade da normal trivariada
mu<-matrix(c(-3,1,4), nrow=3)     #vetor de médias
Sigma<-matrix(c(1,-2,0,-2,5,0,0,0,2), nrow=3) 	#matriz de variâncias e covariâncias



