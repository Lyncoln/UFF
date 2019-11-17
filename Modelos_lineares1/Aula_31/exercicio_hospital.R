BD = haven::read_sav("/mnt/publica/1 AREA DO ALUNO/Lyncoln/UFF/Modelos_lineares1/Aula_31/Banco_Hospital.sav")
BD = BD[,-1]
BD = dplyr::select(BD,Pinfectados,dplyr::everything())

modelo1 = lm(data = BD);summary(modelo1)
BD = BD[,-c(3,4)]
modelo2 = lm(data = BD);summary(modelo2)

ris = rstandard(modelo2)
Di = cooks.distance(modelo2)

sort(Di)

#Verificando graficamente os valores discrepantes
plot(ris~Di, pch = 19,ylim = c(-3,3), xlim = c(0,1.2));abline(h = c(-2,2) , v = c(0.14) , col = c("blue","blue","red","red"))
id = which(Di>0.14);id
Di[id]
text(Di[id],ris[id],rownames(BD)[id],pos = 3, cex = 0.8)

#Retirando o ponto 112

BD = BD[-112,]

modelo3 = lm(data = BD);summary(modelo3)

ris = rstandard(modelo3)
Di = cooks.distance(modelo3)

plot(ris~Di, pch = 19,ylim = c(-3,3), xlim = c(0,1.2));abline(h = c(-2,2) , v = c(0.14) , col = c("blue","blue","red","red"))
id = which(Di>0.14);id
Di[id]
text(Di[id],ris[id],rownames(BD)[id],pos = 3, cex = 0.8)

#Retirando o ponto 47

BD = BD[-47,]

modelo4 = lm(data = BD);summary(modelo4)

ris = rstandard(modelo4)
Di = cooks.distance(modelo4)

plot(ris~Di, pch = 19,ylim = c(-3,3), xlim = c(0,1.2));abline(h = c(-2,2) , v = c(0.11) , col = c("blue","blue","red","red"))
id = which(Di>0.11);id
sort(Di)
Di[id]
text(Di[id],ris[id],rownames(BD)[id],pos = 3, cex = 0.8)

#Retirando o ponto 53

BD = BD[-53,]

modelo5 = lm(data = BD);summary(modelo5)

ris = rstandard(modelo5)
Di = cooks.distance(modelo5)

plot(ris~Di, pch = 19,ylim = c(-3,3), xlim = c(0,1.2));abline(h = c(-2,2) , v = c(0.11) , col = c("blue","blue","red","red"))
id = which(Di>0.11);id
sort(Di)
Di[id]
text(Di[id],ris[id],rownames(BD)[id],pos = 3, cex = 0.8)


