# UNIVERSIDADE FEDERAL FLUMINENSE (UFF)
# INSTITUTO DE MATEMÁTICA E ESTATÍSTCIA (IME)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# 31ª AULA DE MODELOS LINEARES I (AULA PRÁTICA - 6ª FEIRA: 01/11/2019)
# PROF.: DR. JOSÉ RODRIGO DE MORAES (DEPARTAMENTO DE ESTATÍSTICA - GET/UFF)
# ASSUNTO: OBSERVAÇÕES INFLUENTES 
# ENTRADA DE DADOS - NOTAS DE AULA (Slides 72, 73 e 74)

notav1=c(5,3,4,4,7,5,5,1,2,2,7,3.50,2,4,9,8,9.50,9,9,8)
tempoteo=c(36,21,29,26,38,35,32,20,25,30,40,31,23,22,40,39,42,40,40,36)
tempoexs=c(17,10,13,11,23,16,15,8,10,12,20,12,8,8,20,24,26,25,25,23)
notav2_Y=c(5.40,2.40,4.90,3.20,8.20,4.20,5.60,2.10,2.80,3.60,6.90,3.90,2.30,3.50,9.00,8.50,10.00,9.20,9.00,8.20)

tempototal = tempoteo + tempoexs
banco = data.frame(notav2_Y,notav1,tempototal)
str(banco)
modelo1 = lm(notav2_Y~notav1+tempototal); summary(modelo1)

ris = rstandard(modelo1); summary(ris)

id = which(abs(ris)>2);id

Ychapeu = fitted(modelo1)
summary(Ychapeu)


##Medidas não centradas de alavancagem
hii = hatvalues(modelo1); hii
p = sum(hii); p

h_pc = 2*p/20; h_pc

subset(hii,subset = hii>h_pc) # As obs 10 e 14 tem potencial pra influenciar no ajuste
#do modelo

##Medidas centradas de alavancagem
hii_centrada = hii - 1/20; hii_centrada

subset(hii_centrada, hii_centrada >= 0.2) # As obs 10 e 14 são observações com moderado
#potencial para influenciar no ajuste do modelo(ie,obs de moderagem alavancagem)

##Medidas de distância de Cook

Di = cooks.distance(modelo1); Di
subset(Di, Di > 1)
sort(Di)

plot(ris ~Ychapeu, pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2),lty = c(2,1,2))
qqnorm(ris,pch = 19);abline(0,1)
shapiro.test(ris)

#Gráfico de dispersão entre a ordem das obs e resíduos
plot(ris, pch = 19,ylim = c(-3,3));abline(h = c(-2,0,2),lty = c(2,1,2))
id = which(abs(ris)>2);id
ris[id]
text(id,ris[id],rownames(banco)[id],pos = 3, cex = 0.8)


#Gráfico de dispersão entre a ordem das obs e medias centradas de alavancagem 
plot(hii_centrada, pch = 19,ylim = c(0,0.6));abline(h = c(0.2,0.5))
id = which(abs(hii_centrada)>0.2);id
hii_centrada[id]
text(id,hii_centrada[id],rownames(banco)[id],pos = 3, cex = 0.8)


#Gráfico de dispersão entre a ordem das obs e medias de alavancagem 
plot(hii, pch = 19,ylim = c(0,0.6));abline(h = h_pc)
id = which(abs(hii)>h_pc);id
hii[id]
text(id,hii[id],rownames(banco)[id],pos = 3, cex = 0.8)

#Gráfico de dispersão entre ordem das obs com distância de cook
plot(Di, pch = 19,ylim = c(0,1.2));abline(h = 0.2)
id = which(Di>0.2);id
Di[id]
text(id,Di[id],rownames(banco)[id],pos = 3, cex = 0.8)

#Gráfico de dispersão entre ordem os resíduos estudentizados com distancia decook
plot(ris~Di, pch = 19,ylim = c(-3,3), xlim = c(0,1.2));abline(h = c(-2,2) , v = c(0.2) , col = c("blue","blue","red","red"))
id = which(Di>0.2);id
Di[id]
text(Di[id],ris[id],rownames(banco)[id],pos = 3, cex = 0.8)

###2 PASSO:Eliminar a obs 6 (discrepante e mais influente do que as de mais) e avaliar o seu 
#Impacto nas estimativas dos parâmetros do modelo
summary(modelo1)

banco1 = banco[-6,]
modelo2 = lm(data = banco1); summary(modelo2)
banco1$ris=rstandard(modelo2)
banco1$Ychapeu = fitted(modelo2)
banco1$hii = hatvalues(modelo2)
p = sum(banco1$hii)
h_pc = 2*p/19; h_pc
subset(banco1, banco1$hii >= h_pc)
banco1$hii_centrada = banco1$hii - 1/19
subset(banco1, banco1$hii_centrada > 0.2)
banco1$Di = cooks.distance(modelo2)
subset(banco1, banco1$Di > 0.2)

plot(banco1$Di, pch = 19,ylim = c(0,1.2));abline(h = 0.2)
plot(banco1$ris~banco1$Di, pch = 19, ylim = c(-3,3),xlim = c(0,1.2));abline(h = c(-2,2),
                                                                            v = c(0.2))
summary(modelo1)
summary(modelo2)
qqnorm(banco1$ris, pch = 19);abline(0,1)
shapiro.test(banco1$ris)

plot(banco1$ris~banco1$Ychapeu, pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2))
