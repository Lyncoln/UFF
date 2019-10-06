#setwd("C://Milla//UFF//Ano 2015//Analise Multivariada//Aula Pratica//Aula 4//Aula - dois vetores")

# ----------------- Problema -----------------

#Existe diferenca entre os generos segundo os seguintes testes realizados?

#Leiam a base e guardem em um objeto chamado dados

# ----------------- Analise descritiva -----------------

dados=read.csv("avaliacoes_genero.csv")

vetor=matrix(dados$genero, ncol=1)
#Sexo Feminino
# M?dia amostral
dadosF=dados[dados$genero==0,1:7]
Xbarra0 <- matrix(apply(dadosF,2,mean),ncol=1)
Xbarra0

n0 <- nrow(dadosF)
n0
p0 <- ncol(dadosF)
p0

# Matriz de variancias e covariancias amostrais
S0 <- var(dadosF)
S0

# Matriz de correlacoes amostrais
R0=cor(dados)
R0

#Sexo Masculino
# M?dia amostral
dadosM=dados[dados$genero==1,1:7]
Xbarra1 <- matrix(apply(dadosM,2,mean),ncol=1)
Xbarra1

n1 <- nrow(dadosM)
n1
p1 <- ncol(dadosM)
p1

# Matriz de variancias e covariancias amostrais
S1 <- var(dadosM)
S1

# Matriz de correlacoes amostrais
R1=cor(dadosM)
R1

#vetores de médias
cbind(Xbarra0,Xbarra1)

# ----------------- Graficos de dispersao multivariados -----------------

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(dadosF, upper.panel= panel.cor, main="Dispersao e correlacao", pch=16)
pairs(dadosM, upper.panel= panel.cor, main="Dispersao e correlacao", pch=16)

# ----------------- Verificando normalidade multivariada -----------------
# ----------------- Graficamente

# Distancias dj

par(mfrow=c(1,2))

d<-rep(0,n0)

for (i in 1: n0)
  d[i] <- t(t(dadosF[i,]) - Xbarra0) %*% solve(S0) %*% (t(dadosF[i,]) - Xbarra0)
 
qqplot(d, qchisq(ppoints(n0), p0), pch=16, main="Grafico de quantis", xlab="quantis amostrais", ylab="quantis teoricos" )  # ExercÌcio: ?ppoints e proponha uma alternativa
abline(0,1)

d<-rep(0,n1)

for (i in 1: n1)
  d[i] <- t(t(dadosM[i,]) - Xbarra1) %*% solve(S1) %*% (t(dadosM[i,]) - Xbarra1)
 
qqplot(d, qchisq(ppoints(n1), p1), pch=16, main="Grafico de quantis", xlab="quantis amostrais", ylab="quantis teoricos" )  # ExercÌcio: ?ppoints e proponha uma alternativa
abline(0,1)
par(mfrow=c(1,1))

# ----------------- Teste de Shapiro Wilk 

library(mvShapiroTest)

mvShapiro.Test(as.matrix(dadosF))
mvShapiro.Test(as.matrix(dadosM))
 
#Vamos inicialmente fazer o teste ignorando o resultado do teste de normalidade, ok?? 
#So para treinarmos o teste.

# ----------------- Teste para a media ----------------- 
p=p0
Sp=((n0-1)*S0+(n1-1)*S1)/(n0+n1-2)
T2 = (1/n0+1/n1)^{-1} * t(Xbarra0-Xbarra1) %*% solve(Sp) %*% (Xbarra0-Xbarra1)
Fcal=(n0+n1-p-1)/((n0+n1-2)*p)*T2
Fcal

qchisq(0.95,2)
#Qual a sua decisao?

#Qual o p-valor do teste?

#Utilizando a funcao existe no R

install.packages("Hotelling")
require(Hotelling)

analise=hotelling.stat(dadosF,dadosM)
#O que significam cada uma das quantidades?
analise

teste=hotelling.test(dadosF,dadosM)
teste


#Usando o Teorema Central do Limite. Como tomar a decisao?

xcal=(n0+n1)/2*t(Xbarra0-Xbarra1)%*%solve(Sp)%*%(Xbarra0-Xbarra1)
xcal

#p-valor???


install.packages("biotools")
library(biotools)

boxM(dados[, -8], dados[, 8])

a=dados[, -8]
dim(a)



# ----------------- IC simultaneos para medias de duas populacoes independentes ----------------- 

ICsim=function(dados1,dados2,alfa,i){
n1 = nrow(dados1)
S1 = cov(dados1)
n2 = nrow(dados2)
S2 = cov(dados2)
p = ncol(dados1)

Xbarra1 <- matrix(apply(dados1,2,mean),ncol=1)
Xbarra2 <- matrix(apply(dados2,2,mean),ncol=1)
Sp=((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
t.tab=qt(alfa/(2*p),df=(n1+n2-2),lower.tail=FALSE)
ampl=t.tab*sqrt((1/n1+1/n2)*Sp[i,i])

linf=(Xbarra1[i]-Xbarra2[i])-ampl
lsup=(Xbarra1[i]-Xbarra2[i])+ampl

med1=paste("m",i,1,sep="")
med2=paste("m",i,2,sep="")

cat("IC(",med1,"-",med2,")","=",linf,";",lsup)	
}

ICsim(dadosF,dadosM,0.05,1)
ICsim(dadosF,dadosM,0.05,2)
ICsim(dadosF,dadosM,0.05,3)
ICsim(dadosF,dadosM,0.05,4)
ICsim(dadosF,dadosM,0.05,5)
ICsim(dadosF,dadosM,0.05,6)
ICsim(dadosF,dadosM,0.05,7)




