
# 1) ----------------------------------------------------------------------

#a) Regressão linear múltipla

#b)
BD1 = readr::read_table2("tabela_1.tsv")
BD1 = BD1[,-1]

pairs(BD1, pch = 19)
cor(BD1)[1,]

#c)

cor(BD1[,-1])
#Manufatura e Pop possuem cor > |0.90|, o que é um indício de multicolineariedade

#d)

modelo1 = lm(data = BD1); summary(modelo1)
#Pela comparação do teste individual e geral de significância, não é possível ter conclusão
#De multicoliniearidade

#e)
BD1_aux = BD1[,-1]

modelo_temp = lm(BD1_aux)
modelo_manuf = lm(dplyr::select(BD1_aux, Manuf, dplyr::everything()))
modelo_pop = lm(dplyr::select(BD1_aux, Pop, dplyr::everything()))
modelo_vento = lm(dplyr::select(BD1_aux, Vento, dplyr::everything()))
modelo_precip = lm(dplyr::select(BD1_aux, Precip, dplyr::everything()))
modelo_dias = lm(dplyr::select(BD1_aux, N_dias, dplyr::everything()))

r2_temp = summary(modelo_temp)$r.squared
r2_manuf = summary(modelo_manuf)$r.squared
r2_pop = summary(modelo_pop)$r.squared
r2_vento = summary(modelo_vento)$r.squared
r2_precip = summary(modelo_precip)$r.squared
r2_dias = summary(modelo_dias)$r.squared

vif = function(x){
  1/(1-x)
}

vif_temp = vif(r2_temp)
vif_manuf = vif(r2_manuf)
vif_pop = vif(r2_pop)
vif_vento = vif(r2_vento)
vif_precip = vif(r2_precip)
vif_dias = vif(r2_dias)

#Manuf e Pop apresentam VIF > 10, o que indica existência de multicolineariedade

#f)

#Será retirada a variável Pop pois ela não é muito informativa para o problema

BD1_f = BD1[,-4]

modelo2 = lm(data = BD1_f); summary(modelo2)
modelo3 = lm(data = BD1_f[,-c(4,5,6)]); summary(modelo3)

#Yi_hat = 77.237 -1.048*Xi1 + 0.024304Xi2 
summary(modelo3)$r.squared

# 2) ----------------------------------------------------------------------
#a)
BD2 = read.csv("tabela_2.csv")[,-1]
BD2 = BD2[,-1]

BD2 =  BD2 %>% 
  select(PCB, Idade)

plot(PCB ~ Idade, BD2, pch=19)
#Pelo gráfico parece que há uma violação na hipóteses de homocedasticidade
#quanto maior a idade do peixe maior a varição de PCB
cor(BD2)[2,1]

#b)
modelo1 = lm(BD2); summary(modelo1)
ris = rstandard(modelo1)
plot(ris ~ Idade, BD2, 
     pch = 19, 
     ylim = c(-3, 3))

abline(h=c(-2,0,2), 
       lty = c(2,1,2),
       col = c('red','black','red'))

#Não parece haver uma nuvem de pontos aleatorimaente distrubuidos entre
#o zero

qqnorm(ris, pch = 19); abline(0,1)
#Existe violação de normalidade e heterocedasticidade

#c)
rbruto_squared = resid(modelo1)**2
Idade_2 = BD2$Idade**2
modelo_aux = lm(rbruto_squared ~ BD2$Idade + Idade_2)
nrow(BD2) * summary(modelo_aux)$r.squared
qchisq(0.95, 2)

# Pelo teste de White a um nível de significância de 5% o modelo é heterocedástico

#d)
BD2$PCB = log(BD2$PCB)
modelo2 = lm(BD2); summary(modelo2)
plot(PCB ~ Idade, BD2, pch=19)
ris = rstandard(modelo2)
plot(ris ~ BD2$Idade, 
     pch=19, 
     ylim = c(-3, 3)); abline(h=c(-2,0,2))

rbruto_squared = resid(modelo2)**2
Idade_2 = BD2$Idade**2
modelo_aux = lm(rbruto_squared ~ BD2$Idade + Idade_2)
nrow(BD2) * summary(modelo_aux)$r.squared
qchisq(0.95, 2)

qqnorm(ris, pch = 19); abline(0,1)

#os problemas foram resolvidos

#e)
summary(modelo2)$r.squared / summary(modelo1)$r.squared

#Um aumento de aproximadamente 28% no poder de explicação de modelo

#f)

#Sim dividindo o modelo por 1/x_i

# 3) ----------------------------------------------------------------------
#a)
BD3 = readr::read_table2("tabela_3.tsv") %>% 
  select(nota_2, nota_1)
modelo1 = lm(BD3); summary(modelo1)
plot(BD3, pch = 19)
ris = rstandard(modelo1)
plot(ris ~ BD3$nota_1, pch=19, ylim = c(-3,3)); abline(h = c(-2,0,2))

#b)
resid_squared = resid(modelo1)**2
nota_1_sqd = BD3$nota_1**2
modelo_aux = lm(resid_squared ~ BD3$nota_1 + nota_1_sqd)
# 1 - hipoteses
# h0 homocedastico
# h1 heterocedastico

# 2 - estatistica de teste
# W = N * R2 ~ qchisq_gl
# gl numero de variaveis indepednentes
nrow(BD3) * summary(modelo_aux)$r.squared

# 3 - RC
# w > chisq alpha, glq
qchisq(0.95, 2)
# 4 - tomada de decisão
# Não pertece a RC o modelo é homocedastico

#c)
# Ychap_i = 18,3833 + 0,7743*x_1
# o valor estimado da note na segunda avaliacao do aluno que tirou 0 
# na primeira prova é de 18,3833

# 0,7743 é o valor estimado de acréscimo para a nota na segunda avalição
# para cada ponto na primeira avaliação

# 4) ----------------------------------------------------------------------

BD4 = readr::read_table2("tabela_4.tsv")
BD4 = BD4[,-1]
BD4 = BD4[,c(2,1)]

#a)

plot(BD4,pch = 19)
text(BD4, as.character(1:nrow(BD4)),pos = 3)
#As observações 11,12 e 13 se encontram afastadas da grande massa de dados, isso é um indício de ser
#outilers.

#b)

modelo1 = lm(BD4);summary(modelo1)
distancia_cook = cooks.distance(modelo1)
ris = rstandard(modelo1)

#Plot distância de cook
plot(distancia_cook, pch=19, ylim=c(0,2))
id = which(abs(distancia_cook)>0.1);id
text(id, distancia_cook[id],id,pos = 3)

#Plot de ris
plot(ris~fitted(modelo1), pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2))
id = which(abs(ris)>2);id
text(fitted(modelo1)[id], ris[id],id, pos = 4)

#Plot de ris + distancia de cook
plot(ris ~ distancia_cook, 
     pch = 19, 
     ylim = c(-3,3), 
     xlim = c(0,1.5))
abline(h = c(-2,2), 
       v = c(0.1), 
       col = c("blue","blue","red","red"))
id = which(abs(distancia_cook)>0.1);id
text(distancia_cook[id], ris[id],id,pos = 3)

#Observado que os pontos 11 e 12 são outliers e os pontos 11 12 e 13 são possíveis pontos influentes

qqnorm(ris, pch = 19);abline(0,1)

#c)

id = which(abs(ris)>2);id

BD4c = BD4[-id,]
modelo2 = lm(BD4c); summary(modelo2)


distancia_cook = cooks.distance(modelo2)
ris = rstandard(modelo2)

#Plot distância de cook
plot(distancia_cook, pch=19, ylim=c(0,10))
id = which(abs(distancia_cook)>1);id
text(id, distancia_cook[id],id,pos = 3)

#Plot de ris
plot(ris~fitted(modelo2), pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2))
id = which(abs(ris)>2);id
text(fitted(modelo2)[id], ris[id],id, pos = 4)

#Plot de ris + distancia de cook
plot(ris ~ distancia_cook, 
     pch = 19, 
     ylim = c(-5,5), 
     xlim = c(0,10))
abline(h = c(-2,2), 
       v = c(1), 
       col = c("blue","blue","red","red"))
id = which(abs(distancia_cook)>1);id
text(distancia_cook[id], ris[id],id,pos = 3)

qqnorm(ris, pch = 19);abline(0,1)
shapiro.test(ris)


#d)

BD4d = BD4c[-11,]
modelo3 = lm(BD4d); summary(modelo3)

distancia_cook = cooks.distance(modelo3)
ris = rstandard(modelo3)

#Plot distância de cook
plot(distancia_cook, pch=19, ylim=c(0,10))
id = which(abs(distancia_cook)>1);id
text(id, distancia_cook[id],id,pos = 3)

#Plot de ris
plot(ris~fitted(modelo3), pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2))
id = which(abs(ris)>2);id
text(fitted(modelo3)[id], ris[id],id, pos = 4)

#Plot de ris + distancia de cook
plot(ris ~ distancia_cook, 
     pch = 19, 
     ylim = c(-5,5), 
     xlim = c(0,10))
abline(h = c(-2,2), 
       v = c(1), 
       col = c("blue","blue","red","red"))
id = which(abs(distancia_cook)>1);id
text(distancia_cook[id], ris[id],id,pos = 3)

qqnorm(ris, pch = 19); abline(0,1)

#Não houve violação na hipótese de homos. e a distribuição dos ris é normal.



# 5) ----------------------------------------------------------------------

BD5 = readr::read_table2("tabela_5.tsv")
BD5 = BD5[,-1]

#a)
pairs(BD5)
#Y~X1 -> relação negativa moderada; Y~X2 -> relação negativa fraca; Y~X3 -> relação negativa moderada

#b)

cor(BD5)

modelo1 = lm(BD5);summary(modelo1)

pairs(BD5, pch = 19)
modelo_aux_x1 = lm(BD5[,-1]); summary(modelo_aux_x1)
modelo_aux_x2 = lm(BD5[,-1][,c(2,1,3)]); summary(modelo_aux_x2)
modelo_aux_x3 = lm(BD5[,-1][,c(3,1,2)]); summary(modelo_aux_x3)

r2_x1 = summary(modelo_aux_x1)$r.squared
r2_x2 = summary(modelo_aux_x2)$r.squared
r2_x3 = summary(modelo_aux_x3)$r.squared

vif_x1 = 1 / (1-r2_x1);vif_x1
vif_x2 = 1 / (1-r2_x2);vif_x2
vif_x3 = 1 / (1-r2_x3);vif_x3

#A um nível de significância de 5%, todas as variávies possuem uma relação significativa
#Todos os vif <10; logo não existe evidências de multicolineariedade

#c)

y_chapeu = fitted(modelo1)
ris = rstandard(modelo1)
plot(ris ~y_chapeu, pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2))
id = which(ris>2);id
text(y_chapeu[id],ris[id],id,pos = 3)

#d)

hii = hatvalues(modelo1)
p = sum(hii)
h_pc = 2*p/19; h_pc
subset(hii,subset = hii>h_pc) #id = which(hii>h_pc)

k = 1:length(hii)
plot(hii~k, pch = 19); abline(h = h_pc, col = "red")

#A observação 1 tem alta alavancagem(alto potencial para influenciar o ajuste do modelo)

#e)
ris = rstandard(modelo1)
distancia_cook = cooks.distance(modelo1)
plot(distancia_cook, pch = 19)
id = which(distancia_cook>0.1)
text(as.numeric(names(distancia_cook[id])), distancia_cook[id], id, pos = 2)
x = boxplot(distancia_cook, pch = 19)
text(1,distancia_cook[id],id, pos = 2)

#A observação 12 possui alta influência

#f)

#Plot de ris + distancia de cook
plot(ris ~ distancia_cook, 
     pch = 19, 
     ylim = c(-10,10), 
     xlim = c(0,10))
abline(h = c(-2,2), 
       v = c(1), 
       col = c("blue","blue","red","red"))
id = which(abs(ris)>2);id
text(distancia_cook[id], ris[id],id,pos = 3)

#A observação 12 é um outlier mas possui D < 1


#g)

#Iremos retirar a observação 12

BD5a = BD5[-12,]
modelo2 = lm(BD5a); summary(modelo2)
#Melhorou R2


# 6) ----------------------------------------------------------------------

BD6 = readr::read_csv("tabela_6.csv")
BD6 = BD6[,c(-1,-2)]

#a)

modelo1 = lm(BD6[,c(2,1)]); summary(modelo1)
ris = rstandard(modelo1)
y_chapeu = fitted(modelo1)

plot(ris ~ y_chapeu, pch = 19);abline(h = c(-2,0,2))
qqnorm(ris, pch = 19);abline(0,1)

#As hipóteses de normalidade, homost. e independencia aparentemente não foram violadas.

#b)

distancia_cook = cooks.distance(modelo1)
ris = rstandard(modelo1)

plot(ris ~ distancia_cook,
     pch = 19, 
     ylim = c(-3,3),
     xlim = c(0,1.5))
abline(h = c(-2,2),
       v = 1)
id = which(abs(ris) >= 2);id
text(distancia_cook[id], ris[id],id, pos = 4,col = "red")

#c)

maiores = sort(distancia_cook,decreasing = T)
maiores = maiores[1:2]
BD6c = BD6[-as.numeric(names(maiores)),]
modelo2 = lm(BD6c[,c(2,1)]); summary(modelo2)

#Não houve um acrescimo muito significativo.

#f)

#Como houve uma pequena melhora no R2.ajustado para o modelo2, escolheria-se o modelo2


# 7) ----------------------------------------------------------------------

BD7 = readr::read_table2("tabela_7.tsv")
BD7 = BD7[,-1]
BD7[25,] = c(192,17)

#a)
plot(BD7, pch = 19)
#Para maiores valores de vendas, parece haver um pequeno aumento na variabilidade do gosto_com_propaganda

#b)

modelo1 = lm(BD7); summary(modelo1)
ris = rstandard(modelo1); y_chapeu = fitted(modelo1)
plot(ris~y_chapeu, pch = 19, ylim= c(-3,3)); abline(h = c(-2,0,2))
#É possível notar um aumento proporcional crescente

#c)

resid_squared = resid(modelo1)**2
gasto_sqd = BD7$gasto_com_propaganda**2
modelo_aux = lm(resid_squared ~ BD7$gasto_com_propaganda + gasto_sqd)
# 1 - hipoteses
# h0 homocedastico
# h1 heterocedastico

# 2 - estatistica de teste
# W = N * R2 ~ qchisq_gl
# gl numero de variaveis indepednentes
nrow(BD7) * summary(modelo_aux)$r.squared

# 3 - RC
# w > chisq alpha, glq
qchisq(0.95, 2)
# 4 - tomada de decisão
# Pertence a região crítica, logo conclui-se heterosedasticidade

#d)

# Yi_hat/Xi = B0_hat/Xi + B1_hat*Xi/Xi           Y' = Yi_hat/Xi    Xi' = 1/Xi
# Yi_hat' = B1_hat + BO_hat * Xi'


vendas_t = BD7$vendas / BD7$gasto_com_propaganda
gasto_t = 1 / BD7$gasto_com_propaganda

# Yi_hat' = B1_hat + BO_hat * Xi'
# Yi_hat = B1_hat*Xi + BO_hat
# Yi_hat = 7.9511 + 51.9267*Xi

