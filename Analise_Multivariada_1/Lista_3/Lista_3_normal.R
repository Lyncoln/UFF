#QUESTÃO 1

#X~N_3(mu,sigma), sendo
mu=c(10,0,1)
mu
sigma=matrix(c(5,5,5,5,15,10,5,10,10),3,3,byrow=T)
sigma
# a) Determine a distribuição de X_1+2X_2-3X_3

A = matrix(c(1,2,-3), ncol = 3)
mu_a = A %*% mu; mu_a
sigma_a = A %*% sigma %*% t(A); sigma_a


# b) Determine a distribuição condicional de X_1|(X_2,X_3)=(-1,3)

## X1
A = matrix(c(1,0,0), nrow = 1) ; A
mu1_b = A %*% mu ; mu1_b
sigma1_b = A %*% sigma %*% t(A); sigma1_b

## [X_2,X_3]
A = matrix(c(0,1,0,0,0,1),nrow = 2,byrow = T); A
mu2_b = A %*% mu; mu2_b
sigma2_b = A %*% sigma %*% t(A); sigma2_b

## X1/[X2,X3] = (-1,3)
mu_cond = mu1_b + t(matrix(c(5,5))) %*% solve(sigma2_b) %*% (matrix(c(-1,3)) - mu2_b); mu_cond
sigma_cond = sigma1_b - t(matrix(c(5,5))) %*% solve(sigma2_b) %*% matrix(c(5,5)); sigma_cond

#QUESTÃO 2

#X~N_3(mu,sigma), sendo
mu=c(0,0,0)
mu
sigma=matrix(c(5,0,0,0,7,-1,0,-1,2),3,3,byrow=T)
sigma
# a) Determine a distribuição condicional de X_1|(X_2,X_3)=(4,4)

##X1
A = matrix(c(1,0,0),ncol = 3); A
mu1_a = A %*% mu; mu1_a
sigma1_a = A %*% sigma %*% t(A); sigma1_a
##[X2,X3]
A = matrix(c(0,1,0,0,0,1),byrow = T, ncol = 3); A
mu2_a = A %*% mu ; mu2_a 
sigma2_a = A %*% sigma %*% t(A); sigma2_a

##X1/[X2,X3]=(4,4) ~ N(0,5)
mu_cond = 0 + c(0,0) %*% solve(sigma2_b) %*% (c(4,4) - mu2_a); mu_cond
sigma_cond = sigma1_a -  c(0,0) %*% solve(sigma2_b) %*% c(0,0); sigma_cond



# b) Determine a distribuição condicional de X_2|(X_1,X_3)=(2,3)

##X_2
A = matrix(c(0,1,0),ncol = 3); A
mu1_b = A %*% mu; mu1_b
sigma1_b = A %*% sigma %*% t(A); sigma1_b

##[X1,X3] 
A = matrix(c(1,0,0,0,0,1),byrow = T, ncol = 3); A
mu2_b = A %*% mu; mu2_b
sigma2_b = A %*% sigma %*% t(A); sigma2_b

##X_2/[X1,X3] = (2,3) ~ N(-3/2, 7.5)

mu_cond = mu1_b + c(0,-1) %*% solve(sigma2_b) %*% (c(2,3) - mu2_b); mu_cond
sigma_cond =  sigma1_b - c(0,-1) %*% solve(sigma2_b) %*% c(0,-1); sigma_cond 

#QUESTÃO 3

#X~N_4(mu,sigma), sendo
mu=c(1,-1,2,0)
mu
sigma=matrix(c(2,0,1,0,0,3,0,2,1,0,5,0,0,2,0,3),4,4,byrow=T)
sigma

#RESPONDA:

# a) X_1 e X_2 são independentes?

##)Sim, pois temos que sigma1,2 = simga2,1 = 0 e possuem distribuição normal.

# b) Qual a distribuição de [X_1,X_2]?

A = matrix(c(1,0,0,1,0,0,0,0), nrow = 2 ,byrow =F)
mu_b = A %*% mu
sigma_b = A %*% sigma %*% t(A)
## [X_1,X_2] ~ N_2 (mu_b, sigma_b)


# c) Plote o gráfico da função de distibuição de [X_1,X_3] e seus contornos.

A = matrix(c(1,0,0,0,0,0,1,0), byrow = T, ncol = 4)
mu_c = A %*% mu
sigma_c = A %*% sigma %*% t(A) 
library(mvtnorm)
x1 = seq(-10, 10, length= 50)
x2 = x1
f <- matrix(0, nrow=length(x1), ncol=length(x2))

for (i in 1:length(x1))
  for(j in 1: length(x2))
    f[i,j] <- dmvnorm(c(x1[i],x2[j]), mean=mu_c, sigma=sigma_c)
persp(x1, x2, f, theta = 70, phi = 30, col = "lightblue", ticktype = "detailed")
contour(x1, x2, f, draw=T, nlevels=20, labcex=0.8, xlab=expression(x[1]),ylab=expression(x[2]), drawlabels=FALSE)


# d) Qual a distribuição condicional de (X_1,X_2)|(X_3,X_4)=(2,3)? 

##[X1,X2]
A = matrix(c(1,0,0,0,0,1,0,0),ncol = 4,byrow = T); A
mu1_d = A %*% mu; mu1_d 
sigma1_d = A %*% sigma %*% t(A); sigma1_d

##[X3,X4]

A = matrix(c(0,0,1,0,0,0,0,1),ncol = 4, byrow = T); A
mu2_d = A %*% mu; mu2_d
sigma2_d = A %*% sigma %*% t(A); sigma2_d

##[X1,X2]/[X3,X4] = (2,3) ~ N_2( mu_cond, sigma_cond )

mu_cond = mu1_d + matrix(c(1,0,0,2), ncol=2,byrow = T) %*% solve(sigma2_d) %*% (c(2,3) - mu2_d); mu_cond 
sigma_cond = sigma1_d +matrix(c(1,0,0,2), ncol=2,byrow = T) %*% solve(sigma2_d) %*% matrix(c(1,0,0,2), ncol=2,byrow = T); sigma_cond


