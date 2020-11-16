# UNIVERSIDADE FEDERAL FLUMINENSE (UFF)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# AULA PRÁTICA DE MODELOS LINEARES I: REGRESSÃO LINEAR SIMPES (RLS) - n=20 indivíduos
# DATA: 06/09/2019 (6ª FEIRA)
# PROF: DR.JOSÉ RODRIGO DE MORAES (GET/UFF)
# ASSUNTO: INTERVALOS DE CONFIANÇA E PREDIÇÃO (BANDAS DE CONFIANÇA E PREDIÇÃO) 

###########################################################################################
# Um estudo foi desenvolvido com objetivo de avaliar o efeito da idade no tempo de        #
# reação a um certo estímulo. Os dados referentes as essas duas variáveis foram coletados #
# para uma amostra de n=20 indivíduos (NOTAS DE AULA: Ver Tabela 1 da 10ª-12ª Aulas).     #                                               #
###########################################################################################

# Idade (em anos) 
Idade=c(20,20,20,20,25,25,25,25,30,30,30,30,35,35,35,35,40,40,40,40)
confint(modelo1, level = 0.95)
# Tempo de reação (em segundos)
Tempo=c(96,92,106,100,98,104,110,101,116,106,109,100,112,105,118,108,113,112,127,117)

plot(Idade,Tempo, pch = 19)

R = cor(Idade,Tempo);R

modelo1 = lm(Tempo ~Idade);modelo
summary(modelo1)

plot(Idade,Tempo, pch = 19)
abline(modelo1)

anova(modelo1)

confint(modelo1, level = 0.95)

#IC para o tempo médio de reação para indivíduos com 30 e 28 anos de idade

Xi = data.frame(Idade = c(30,28));Xi
ICmedia = predict(modelo1, Xi, interval = "confidence");ICmedia_X30

# Intervalo para observação individual(Tempo de reacao de uma pessoa com 30/28 anos)

Xi = data.frame(Idade = c(30,28));Xi
ICobs = predict(modelo1, Xi, interval = "prediction");ICobs

# bandas de confianca e predicao


confianca = predict(modelo1, interval = "confidence"); confianca
predicao = predict(modelo1, interval = "prediction"); predicao

junta = data.frame(predicao, confianca[,-1]); junta
matplot(Idade,junta, lty = c(1,2,2,3,3), lwd = 1, type = "l", main = "Bandas de Confiança e Predição", xlab = "Idade", ylab = "Estimativas",col = c("black","red","red","blue","blue"))
matpoints(Idade, Tempo, pch=19)



# exer aula 11 ------------------------------------------------------------

# UNIVERSIDADE FEDERAL FLUMINENSE (UFF)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# AULA PRÁTICA DE MODELOS LINEARES I: REGRESSÃO LINEAR SIMPES (RLS) 
# DATA: 06/09/2019 (6ª FEIRA)
# PROF: DR.JOSÉ RODRIGO DE MORAES (GET/UFF)
# ASSUNTO: INTERVALOS DE CONFIANÇA E PREDIÇÃO (BANDAS DE CONFIANÇA E PREDIÇÃO) 

##################################################################################
### CORREÇÃO DO EXERCÍCIO PROPOSTO / ADICIONAL - EX. COM AS SAÍDAS DO PROGRAMA R #
##################################################################################

# Escolaridade (em anos de estudo) 
escol=c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 17, 18)

# Salário (em Unidades Monetárias - UM)
sal=c(100, 200, 300, 200, 300, 200, 200, 300, 500, 400, 500, 600, 550, 600, 800, 700, 600, 900, 800, 900)

plot(escol,sal,pch=19)

modelo1 = lm(sal~escol)
R2 = round(summary(modelo1)$r.squared, 4) * 100; R2
sigma.chapeu = x$sigma; sigma.chapeu
sigma2.chapeu = sigma.chapeu^2;sigma2.chapeu

#Intervalo de confiança para o salário médio de indivíduos com 8 e 10 anos de estudo

Xi = data.frame(escol = c(8,10));Xi
ICmedia = predict(modelo1, Xi, interval = "confidence");ICmedia

#Intervalo de predicao para o salario de individuo com escolaridade igual a 20

Xi = data.frame(escol = c(20));Xi
ICobs = predict(modelo1, Xi, interval = "prediction");ICobs
