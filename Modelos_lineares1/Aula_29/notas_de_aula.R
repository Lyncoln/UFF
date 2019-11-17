#################################################################################
# 29ª AULA DE MODELOS LINEARES I (AULA PRÁTICA - 18/10/2019) - PÓS-2ª VE        #
# EXERCICIO 1 - PÁGINA 34  DAS NOTAS DE AULA DE MODELOS LINEARES I              #
# MODELO DE REGRESSÃO LINEAR MÚLTIPLA - MULTICOLINEARIDADE                      #   
#################################################################################

public=c(17,6,13,11,23,16,15,5,10,12,20,12,8,8)                                 
exp=c(35.7,11.4,28.6,25.8,50.6,27.2,31.3,10.0,18.9,25.2,39.9,32.5,13.6,19.0) 
desemp=c(52.1,24.6,49.2,30.0,82.2,42.4,55.7,21.1,27.7,36.3,69.1,38.8,22.8,34.7)  

#Slides da aula 29 slide 34

banco = data.frame(public,exp,desemp); str(banco)
pairs(banco, pch = 19)
cor(banco)

modelo1 = lm(desemp~public+exp); summary(modelo1)

## 1º modo

modelo_public = lm(public~exp);summary(modelo_public)
R2_public = summary(modelo_public)$r.squared; R2_public
VIF_public = 1 / (1-R2_public);VIF_public
#Como VIF_public > 10, a variável número de publicações está fortemente correlacionada
#Com o tempo de experiencia, logo existe problema de colineariedade

modelo_exp = lm(exp~public);summary(modelo_exp)
R2_exp = summary(modelo_exp)$r.squared; R2_exp
VIF_exp = 1 / (1-R2_exp);VIF_exp
#Como VIF_exp > 10, a variável número tempo de experiencia está fortemente correlacionada
#Com o número de publicações, logo existe problema de colineariedade

## 2º modo
library(car)
vif(modelo1)



modelo1 = lm(desemp~public+exp); summary(modelo1)
modelo2 = lm(desemp~public); summary(modelo2)
modelo3 = lm(desemp~exp); summary(modelo3)

anova(modelo2,modelo1)
anova(modelo3,modelo1)

#modelo 2 tem sqres menor, logo é preferível

#Análise das hipóteses básicas

ris = rstandard(modelo2);summary(ris)
qqnorm(ris,pch = 19);abline(0,1)
shapiro.test(ris)

desemp.est = fitted(modelo2)

plot(desemp.est~desemp,pch = 19);abline(0,1)
R = cor(desemp, desemp.est);R
plot(ris~desemp.est,pch = 19, ylim = c(-3,3)); abline(h = c(-2,0,2))
plot(ris~public,pch = 19, ylim = c(-3,3)); abline(h = c(-2,0,2))
