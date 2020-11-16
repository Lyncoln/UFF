# Concentração da substância, em mg/L 
Conc_subst=c(1.00,3.70,1.00,9.00,2.00,2.25,2.91,2.75,3.00,3.50,3.75,9.45,4.25,7.00,4.75,5.00,5.50,6.00,6.50,7.00,7.50,8.00,8.25,9.40,9.43,8.94,9.20,9.50,8.00,9.00)
# Ganho de peso, em Kg
Ganho_peso=c(9.40,11.40,12.00,16.00,11.00,12.50,10.40,11.50,12.50,14.00,14.50,17.00,13.25,14.80,14.00,14.10,12.50,15.20,14.20,16.50,17.00,14.50,16.00,17.00,14.90,15.00,19.00,17.50,16.00,17.50)
modelo1 = lm(Ganho_peso ~ Conc_subst)
summary(modelo1)
anova(modelo1)


# Exercício ---------------------------------------------------------------
# Continuação do exercício da aula 6


tempo_estudo = c(96, 77, 0, 0, 78, 64, 89, 47, 90, 93, 18, 86, 0, 30, 59, 77, 74, 67)
desempenho = c(95, 80, 0, 0, 79, 77, 72, 66, 98, 90, 0, 95, 35, 50, 72, 55, 75, 66)

#h)

R = cor(tempo_estudo, desempenho)

# Obtemos R > 0.91041, ou seja, temos uma relação linear positiva forte entre o tempo 
# de estudo e desempenho dos alunos. Ou seja, quanto maior o tempo de estudo
# maior tende a ser o desempenho do aluno.

#i)
B1chap = R * (sd(desempenho)/sd(tempo_estudo));  B1chap

#j)
modelo = lm(desempenho~tempo_estudo)
tabela_anova = anova(modelo); tabela_anova



# SQReg = 14873 é a variação do desempenho dos alunos
# explicada pelo modelo ajustado, ou seja, é a variação
# Do desempenho dos alunos explicada em função  de seu tempos de estudo.

# SQRes = 3071.2 é a variação não explicada pelo modelo ajustado.


#k)

# Hipóteses ->  H0: B1 = 0  x  H1: B1 =/= 0
QMReg = tabela_anova$`Sum Sq`[1]; QMReg
QMRes = tabela_anova$`Mean Sq`[2]; QMRes

# Estatística -> F = QMReg / QMRes ~ F(1,n-2)

Fobs = QMReg / QMRes; Fobs

f_quantil = qf(0.05, tabela_anova$Df[1],tabela_anova$Df[2], lower.tail = F); f_quantil

# RC -> {f e R, f >= 4.493998}
# Obtivemos que Fobs pertence a Região crítica então recusamos H0, ou seja,
# Há evidências estatísticas que existe relação significativa entre desempenho
# do aluno e o seu tempo de estudo.

#i)

e2 = sum(resid(modelo)^2)
difxi = sum((tempo_estudo - mean(tempo_estudo))^2)

Fobs = (B1chap^2 * difxi) / (e2/(length(tempo_estudo) - 2)); Fobs

#m)

x = summary(modelo)
R = x$r.squared

# R = 0.8288463 é o coeficiente de determinação do modelo, isso significa que
# 82.88% da variação do desempenho dos alunos é explicada 
# em função do tempo de estudo.

#o)

Xi = data.frame(tempo_estudo = 64);Xi
ICmedia = predict(modelo, Xi, interval = "confidence");ICmedia

#n)
y_chapeu = ICmedia[1];y_chapeu

#q)

Xi = data.frame(tempo_estudo = 80);Xi
ICobs = predict(modelo, Xi, interval = "prediction");ICobs
#p)
y_chapeu = ICobs[1]; y_chapeu


#f)

tempo_medio = mean(tempo_estudo);tempo_medio 

Xi = data.frame(tempo_estudo = tempo_medio);Xi
ICmedia = predict(modelo, Xi, interval = "confidence");ICmedia
ICobs = predict(modelo, Xi, interval = "prediction");ICobs
