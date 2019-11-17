setwd("C://Milla//UFF//Ano 2015//Analise Multivariada//Aula Pratica//Aula 6 - CP e AF")

dados = read.csv("fatorial.csv")
names(dados)
fix(dados)
dados=dados2

library(corrgram)
matcor=cor(dados[,c(-1,-2,-10,-11)])
corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)
# todas as correla??es est?o em cor azul porque s?o positivas, com tons mais fortes para as
#correla??es mais altas. Para estas, o ?ngulo (sentido hor?rio) no gr?fico de setores do painel superior
#(upper.panel = panel.pie) ? maior.



eigen(cov(dados[,c(-1,-2,-10,-11)]))

CP=prcomp(dados[,c(-1,-2,-10,-11)])
summary(CP)
plot(CP, type="lines")


names(CP)

plot(CP$rotation,xlim=c(-1,1.1),ylim=c(-1,0.5))
abline(h=0)
abline(v=0)
biplot(CP, col = c("gray", "black"))
abline(h=0)
abline(v=0)

CP2 = princomp(dados[,c(-1,-2,-10,-11)],cor=TRUE)
names(CP2)
summary(CP2)
CP2$loadings
CP2$sdev
CP2$scale 
CP2$scores #variaveis ou scores criados a partir das CP

round(cor(CP2$scores))

loadings(CP2)

###Analise fatorial

library(mvShapiroTest)

mvShapiro.Test(as.matrix(dados[,c(-1,-2,-10,-11)]))


matrizdados=dados[,c(-1,-2,-10,-11)]

#######
n <- nrow(matrizdados)

p <- ncol(matrizdados)


# --- Analise descritiva e exploratoria --- #

Xbarra <- apply(matrizdados,2,mean)
Xbarra

S <- cov(matrizdados)
S

###########Adequacao da AF - KMO

partial.cor <- function (X, ...)
{
  R <- cor(X, ...)
  RI <- solve(R)
  D <- 1/sqrt(diag(RI))
  Rp <- -RI * (D %o% D)
  diag(Rp) <- 0
  rownames(Rp) <- colnames(Rp) <- colnames(X)
  Rp
}
matcorp <- partial.cor(matrizdados)
idiag <- seq(1, by = p + 1, length = p)
somar2 <- sum((as.numeric(matcor)[-idiag])^2)
cat("\n KMO = ",somar2 / (somar2 + sum((as.numeric(matcorp)[-idiag])^2)))

#####Estimacao via CP

acpcor <- prcomp(matrizdados, scale = TRUE)
summary(acpcor)

plot(1:ncol(matrizdados), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Vari?ncia", pch = 20, cex.axis = 1.3, cex.lab = 1.3)


k <- 2
carfat <- acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
colnames(carfat) <- paste("Fator", 1:k, sep = " ")

comum <- rowSums(carfat^2)
vespec <- diag(matcor) - comum
estimat <- cbind(comum, vespec, diag(matcor))
rownames(estimat) <- colnames(matrizdados)
colnames(estimat) <- c("Comunalidade", "Vari?ncia ?nica", "Vari?ncia")
estimat

resid <- matcor - (carfat %*% t(carfat) + diag(vespec))

carfatr <- varimax(carfat)

plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)

plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)




# --- Estimacao via componentes principais --- #
summary(princomp(matrizdados, cor=TRUE))

princomp(matrizdados, cor=TRUE)[[1]]^2

# --- Criterio de Kaiser --- #

sum(princomp(matrizdados, cor=TRUE)[[1]]>1)

#Por que maiores do que 1?

# --- Cargas fatoriais --- #

cbind(sqrt(eigen(cor(matrizdados))[[1]][1]) * eigen(cor(matrizdados))[[2]][,1],
      sqrt(eigen(cor(matrizdados))[[1]][2]) * eigen(cor(matrizdados))[[2]][,2],
      sqrt(eigen(cor(matrizdados))[[1]][3]) * eigen(cor(matrizdados))[[2]][,3])


# --- Rotacao varimax --- #
varimax(cbind(sqrt(eigen(cor(matrizdados))[[1]][1]) * eigen(cor(matrizdados))[[2]][,1],
              sqrt(eigen(cor(matrizdados))[[1]][2]) * eigen(cor(matrizdados))[[2]][,2],
              sqrt(eigen(cor(matrizdados))[[1]][3]) * eigen(cor(matrizdados))[[2]][,3]))

#Verificando se a matriz de rotacao e ortogonal
A<-varimax(cbind(sqrt(eigen(cor(matrizdados))[[1]][1]) * eigen(cor(matrizdados))[[2]][,1],
                 sqrt(eigen(cor(matrizdados))[[1]][2]) * eigen(cor(matrizdados))[[2]][,2],
                 sqrt(eigen(cor(matrizdados))[[1]][3]) * eigen(cor(matrizdados))[[2]][,3]))$rotmat

round(A %*% t(A),2)

########## Estimacao via MV
# factanal so estima por maxima verossimilhanca
analisefatorial <- factanal(matrizdados, factors=3, rotation="varimax", scores="regression")
analisefatorial

print(analisefatorial, digits=2, cutoff=.3, sort=TRUE)

load <- analisefatorial$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(matrizdados),cex=.7) # add variable names

#--- Cargas fatoriais ---#
cargas <- analisefatorial$loadings[]

#--- Comunalidades  ---#
comunalidades <- diag(cargas %*% t(cargas))

#--- Variancia especifica ---#
varespecifica <- analisefatorial$uniqueness

#--- Estimativa de R  ---#
Rchapeu <- cargas %*% t(cargas) + varespecifica

#--- Matriz de residuos  ---#
residuos <- cor(matrizdados) - Rchapeu

round(residuos, 2)




#comp=fa(cor(matrizdados),3,rotate="varimax",fm="ml")


#####Outro exemplo
dados = read.table("heptatlon.txt", header=TRUE)
dados
