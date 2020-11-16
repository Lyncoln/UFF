Conc_subst=c(1.00,3.70,1.00,9.00,2.00,2.25,2.91,2.75,3.00,3.50,3.75,9.45,4.25,7.00,4.75,5.00,5.50,6.00,6.50,7.00,7.50,8.00,8.25,9.40,9.43,8.94,9.20,9.50,8.00,9.00)

Ganho_peso=c(9.40,11.40,12.00,16.00,11.00,12.50,10.40,11.50,12.50,14.00,14.50,17.00,13.25,14.80,14.00,14.10,12.50,15.20,14.20,16.50,17.00,14.50,16.00,17.00,14.90,15.00,19.00,17.50,16.00,17.50)

bd = data.frame(Conc_subst, Ganho_peso); bd
str(bd)

plot(bd)

modelo1 = lm(Ganho_peso~Conc_subst); modelo1
summary(modelo1)

ei=resid(modelo1); ei


Yichapeu = fitted(modelo1);summary(Yichapeu)

plot(bd, pch = 19);abline(modelo1, col = "red", lwd = 4)


IC95 = confint(modelo1,level = 0.95);IC95


############

mediaX = mean(bd$Conc_subst)
mediaY = mean(bd$Ganho_peso)
xi2 = bd$Conc_subst^2
xiyi = bd$Conc_subst * bd$Ganho_peso
n = length(bd[,1])


beta1chapeu = (sum(xiyi) - n*mediaX*mediaY) / (sum(xi2) - n*mediaX^2); beta1chapeu
beta0chapeu = mediaY - beta1chapeu*mediaX; beta0chapeu


ei = bd$Ganho_peso - Yichapeu; ei
ei2 = ei^2
SQRes = sum(ei2); SQRes
sigma2chapeu = SQRes / (n-2); sigma2chapeu
sigmachapeu = sqrt(sigma2chapeu); sigmachapeu



####Exercício 

tempo_estudo = c(96, 77, 0, 0, 78, 64, 89, 47, 90, 93, 18, 86, 0, 30, 59, 77, 74, 67)
desempenho = c(95, 80, 0, 0, 79, 77, 72, 66, 98, 90, 0, 95, 35, 50, 72, 55, 75, 66)


#a)
modelo2 = lm(desempenho~tempo_estudo); summary(modelo2)

b1chapeu = ( sum(tempo_estudo*desempenho) - length(desempenho) * mean(tempo_estudo) * mean(desempenho) ) / ( sum(tempo_estudo^2) - length(tempo_estudo) * mean(tempo_estudo)^2 ); b1chapeu
b0chapeu = mean(desempenho) - b1chapeu * mean(tempo_estudo); b0chapeu

# A equação do modelo é dada por Yichapeu = 10.7269 + 0.8726*Xi
# b0chapeu = 10.7269 é interpretado como a estimativa de desempenho do aluno com 0 horas de estudo
# b1chapeu = 0.8726 é interpretado como a estimativa da quantidade de ganho de desempenho do aluno para cada hora estudada

#b)

plot(y = desempenho, x = tempo_estudo, pch = 19);abline(modelo2, col = "red", lwd = 3)

#O gráfico de dispersão indica uma relação linear positiva entre tempo de estudo do aluno e seu desempenho. Isto é, quanto maior
#o tempo de estudo do aluno maior tende a ser seu desempenho.

#c)

yichapeu = fitted(modelo2); yichapeu
ei = desempenho - yichapeu
sigma2chapeu = sum(ei^2)/ (length(ei) -2); sigma2chapeu

e_b1chapeu = sigma2chapeu / sum((tempo_estudo - mean(tempo_estudo))^2); sqrt(e_b1chapeu)
e_b0chapeu = sigma2chapeu * (1/length(desempenho) + mean(tempo_estudo)^2/ sum((tempo_estudo-mean(tempo_estudo))^2)); sqrt(e_b0chapeu)


#d)

sigma2chapeu = sum(ei^2)/ (length(ei) -2); sigma2chapeu
sigmachapeu = sqrt(sigma2chapeu); sigmachapeu

#e)


IC95 = confint(modelo2,level = 0.95);IC95

li_b0chapeu = b0chapeu - qt(0.025, length(desempenho) -2, lower.tail = F) * sqrt(e_b0chapeu); li_b0chapeu
ls_b0chapeu = b0chapeu + qt(0.025, length(desempenho) -2, lower.tail = F) * sqrt(e_b0chapeu); ls_b0chapeu

# O intervalo de confiança para a estimativa do intercepto do modelo a um nível de 
# confiança de 95% é: [-3.3012 ; 24.7550]

li_b1chapeu = b1chapeu - qt(0.025, length(desempenho) -2, lower.tail = F) * sqrt(e_b1chapeu); li_b1chapeu
ls_b1chapeu = b1chapeu + qt(0.025, length(desempenho) -2, lower.tail = F) * sqrt(e_b1chapeu); ls_b1chapeu

# O intervalo de confiança para a estimativa do b1 do modelo a um nível de 
# confiança de 95% é: [0.6625 ; 1.0828]

#f)

#Iremos realizar um teste de hipótese para verificar se existe relação estatisticamente significante 
#entres o desempenho dos alunos em relação ao tempo de estudo.

#H0 : B1 =0
#H1 : B1 =/= 0

estatistica = b1chapeu*(1/sqrt(e_b1chapeu)); estatistica

quantil_inferior = -qt(0.05/2, length(desempenho) - 2, lower.tail = F); quantil_inferior
quantil_superior = qt(0.05/2, length(desempenho) - 2, lower.tail = F); quantil_superior

#Obtivemos a estatística dentro da região crítica, ou seja, rejeitamos H0, isso é, 
#Há evidências ao nível de significancia de 5% que existe relação linear significantiva entre
#O desempenho e o tempo de estudo do aluno.

#Ou 

summary(modelo2)

#Obtivemos um pvalor < 0.05, ou seja, rejeitamos H0.

#g)


#Iremos analizar estatísticamente se b0 tem significância no modelo

#H0 : B0 = 0
#H1 : B0 =/= 0 

estatistica = b0chapeu*(1/sqrt(e_b0chapeu)); estatistica  

quantil_inferior = -qt(0.05/2, length(desempenho) - 2, lower.tail = F); quantil_inferior
quantil_superior = qt(0.05/2, length(desempenho) - 2, lower.tail = F); quantil_superior

# Obtivemos a estatistica fora da região crítica, ou seja, não rejeitamos h0,
# Isso é, há um nível de significancia de 5% acredita-se que o intercpeto do modelo é 0, ou seja
# há evididências estatísticas para acreditar que o desempenho do aluno é 0 
# quando não há horas de estudo

summary(modelo2)

#Obtivemos um pvalor > 0.05, ou seja, rejeitamos H0.
