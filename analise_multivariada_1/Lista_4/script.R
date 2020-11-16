
# 1) ---------------------------------------------------------------


mu = c(5,3)
sigma = matrix(c(15,5,5,4),ncol =2, byrow = T)
set.seed(10)
amostra = mvtnorm::rmvnorm(100, mu, sigma)


# 2) ----------------------------------------------------------------------

mvShapiroTest::mvShapiro.Test(amostra)

# 3) ----------------------------------------------------------------------


#H0: m = m0
#H1: m =/= m0


#Na mão
xbarra = c(mean(amostra[,1]),mean(amostra[,2]))
S = var(amostra)
n = length(amostra[,1])
p = length(amostra[1,])
RC = qf(0.05, 2, 98, lower.tail = F)
T2 = n*t(xbarra - mu) %*% solve(S) %*% (xbarra - mu)
T2 = (n-p)/((n-1)*p) * T2

#Com pacote

teste = ICSNP::HotellingsT2(amostra, mu = mu)

#O pacote isola a F na direita, região crítica muda


#Obtivemos p-valor maior que 5%, então não recusamos a hipótese nula
#Ou seja, m = m0.

# 4) ----------------------------------------------------------------------


library(ggplot2)

amostra = as.data.frame(amostra)
ggplot(amostra, aes(x = amostra[,1], y = amostra[,2])) +
  geom_point() +
  stat_ellipse()+
  geom_point(aes(x = mean(amostra[,1]), y = mean(amostra[,2])), col = "blue")


# 5) ----------------------------------------------------------------------

t.test(amostra[,1], conf.level = 1 - 0.05/p)
t.test(amostra[,2], conf.level = 1 - 0.05/p)

