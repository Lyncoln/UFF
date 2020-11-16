

# 1) ----------------------------------------------------------------------

BD1 =readr::read_table2("tabela_1.tsv")

#a)

BD1$Ano = 1:17
BD1 = BD1[,c(2,1)]

#Yi = B0 + B1Xi1 + ei
modelo1 = lm(BD1); summary(modelo1)

#Yi = B0*B1^Xi1 * ei
#log(Yi) = log(B0) + Xi1*log(B1) + log(ei)

prod_log = log10(BD1$Producao_de_automoveis)
modelo2 = lm(prod_log ~ BD1$Ano); summary(modelo2)


#Yi = B0 + B1Xi1 + B2Xi1^2 + ei

ano_sqd = BD1$Ano ^ 2
modelo3 = lm(BD1$Producao_de_automoveis ~ BD1$Ano +ano_sqd  ); summary(modelo3)


#b)

summary(modelo1)$adj.r.squared
summary(modelo2)$adj.r.squared
summary(modelo3)$adj.r.squared

#c)


#Escolherei modelo 2 pois ele possui a variável ano com efeito signficativo para produção de automóveis
#E seu R2 ajustado é alto



# 2) ----------------------------------------------------------------------

BD2 =readr::read_table2("tabela_2.tsv")
BD2 = BD2[,-1]
BD2 = BD2[,c(2,1)]

#a)

plot(BD2, pch = 19)

#Parece existir uma relação linear positiva

#b)

modelo1 = lm(BD2); summary(modelo1)
#Todas as variáveis tem efeitos significativos
ris = rstandard(modelo1)
y_chapeu = fitted(modelo1)

plot(ris ~ BD2$Altura, pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2))
#Não parece ser uma núvem aleatória de pontos, evidência de violação de alguma hipótese básica

#c)

altura_sqrd = BD2$Altura ^ 2
modelo2 = lm(BD2$VEF ~BD2$Altura + altura_sqrd); summary(modelo2)

ris = rstandard(modelo2)
y_chapeu = fitted(modelo2)

plot(ris ~ y_chapeu, pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2))

#d)

#A qualidade de ajuste do modelo melhorou em cerca de 2%


# 3) ----------------------------------------------------------------------

BD3 =readr::read_table2("tabela_3.tsv")

#a)

modelo1 = lm(BD3); summary(modelo1)
#Ambas as variáveis possuem relação significativa

ris = rstandard(modelo1)
y_chapeu = fitted(modelo1)

plot(ris ~ BD3$Temperatura, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
plot(ris ~ BD3$Concentracao, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
plot(ris ~ y_chapeu, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
#Existe uma relação curvelinea, violação da hipótese de lineariedade

#b)

temperatura_sqrd = BD3$Temperatura ^ 2
modelo2 = lm(BD3$Producao~BD3$Temperatura + BD3$Concentracao+ temperatura_sqrd);summary(modelo2)

ris = rstandard(modelo2)
y_chapeu = fitted(modelo2)

plot(ris ~ BD3$Temperatura, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
plot(ris ~ BD3$Concentracao, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
plot(ris ~ temperatura_sqrd, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
plot(ris ~ y_chapeu, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
qqnorm(ris, pch = 19);abline(0,1)
#As hipóteses foram satisfeitas


# 4) ----------------------------------------------------------------------

BD4 =readr::read_table2("tabela_4.tsv")
BD4 = BD4[,c(2,1)]
#a)

modelo1 = lm(BD4); summary(modelo1)
#Os beta possuem efeito significânte

ris = rstandard(modelo1)
y_chapeu = fitted(modelo1)

plot(ris ~ y_chapeu, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
qqnorm(ris, pch = 19);abline(0,1)
shapiro.test(ris)
#Não é normal

#b)

BD4$No_de_bacterias = sqrt(BD4$No_de_bacterias)

modelo2 = lm(BD4); summary(modelo2)
#Os beta possuem efeito significânte

ris = rstandard(modelo2)
y_chapeu = fitted(modelo2)

plot(ris ~ y_chapeu, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
qqnorm(ris, pch = 19);abline(0,1)
shapiro.test(ris)
#É normal
plot(BD4$No_de_bacterias~BD4$Tempo_de_exposicaoo, pch = 19)
#Não é linear

#c)

BD4$Tempo_de_exposicaoo_2= BD4$Tempo_de_exposicaoo ^ 2
modelo3 = lm(BD4); summary(modelo3)
ris = rstandard(modelo3)
y_chapeu = fitted(modelo3)

plot(ris~BD4$Tempo_de_exposicaoo, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
plot(ris~y_chapeu, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2))
#Lineariedade satisfeita
qqnorm(ris, pch = 19); abline(0,1)
#Normalidade satisfeita

#d)

distancia_cook =cooks.distance(modelo3)
id = which(distancia_cook >= 1); id
plot(ris~distancia_cook, pch = 19, ylim= c(-4,4), xlim = c(0,14))
abline(h = c(-2,0,2) , v = 1)
text(distancia_cook[id], ris[id], id, pos = 3)
#A observação 1 é autamente influente

BD4 = BD4[-1,]



modelo4 = lm(BD4); summary(modelo4)
ris = rstandard(modelo4)

distancia_cook =cooks.distance(modelo4)
id = which(distancia_cook >= 1); id
plot(ris~distancia_cook, pch = 19, ylim= c(-4,4), xlim = c(0,14))
abline(h = c(-2,0,2) , v = 1)
text(distancia_cook[id], ris[id], id, pos = 3)

#A variavel Tempo de exposição^2 não possui mais efeito significante