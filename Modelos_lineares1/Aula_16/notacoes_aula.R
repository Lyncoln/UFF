# UNIVERSIDADE FEDERAL FLUMINENSE (UFF)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# DISCIPLINA: MDOELOS LINEARES I
# PROF. JOSÉ RODRIGO DE MORAES (GET/UFF)
# AULA PRÁTICA: ANALISE GRÁFICA DOS RESÍDUOS DO MODELO (MÉTODO GRÁFICO)
# DATA: 13/09/2019 (6ª FEIRA)

# EXERCÍCIO PROPOSTO 1 - ANÁLISE DOS RESÍDUOS:
# AJUSTE UM MODELO DE RLS PARA AVALIAR O EFEITO DA IDADE NO TEMPO DE REAÇÃO.

# Idade (em anos) 
Idade=c(20,20,20,20,25,25,25,25,30,30,30,30,35,35,35,35,40,40,40,40)

# Tempo de reação (em segundos)
Tempo=c(96,92,106,100,98,104,110,101,116,106,109,100,112,105,118,108,113,112,127,117)

plot(Tempo~Idade, pch = 19)

modelo1 = lm(Tempo~Idade); summary(modelo1)
x = summary(modelo1)
anova(modelo1)

#Residuos brutos
ei = resid(modelo1);ei; summary(ei)

#Residuos padronizados
riP = ei/x$sigma; riP

#Residuos estudentizados
riS = rstandard(modelo1); riS
#Valores estimados da variável resposta
Ychapeu = fitted(modelo1); Ychapeu
#Análise gráfica dos resíduos para avaliação das hipóteses básicas do modelo

#Residuos padronizados x Idade
plot(riP~Idade, pch = 18, ylim = c(-3,3))
abline(h = c(-2,0,2))
#Residuos padronizados x tempo estimado
plot(riP~Ychapeu, pch = 18, ylim = c(-3,3))
abline(h = c(-2,0,2))

#Residuos studentizados x Idade
plot(riS~Idade, pch = 18, ylim = c(-3,3))
#Residuos studentizados x tempo estimado
plot(riS~Ychapeu,pch = 18, ylim = c(-3,3))
abline(h = c(-2,0,2))


#Avaliação de hipótese de normalidade dos erros

qqnorm(riP, pch = 19,main="", xlab = "Quantis teóricos", ylab = "Quantis dos resíduos padronizados")
abline(0,1,pch=30)
shapiro.test(riP)
ks.test(riP, pnorm)

qqnorm(riS, pch = 19,main="", xlab = "Quantis teóricos", ylab = "Quantis dos resíduos padronizados")
abline(0,1,pch=30)
shapiro.test(riS)
ks.test(riS, pnorm)



# EXERCÍCIO PROPOSTO 2 - ANÁLISE DOS RESÍDUOS:
# PERGUNTA: EXISTE ALGUM TIPO DE VIOLAÇÃO DAS HIPÓTESES BÁSICAS DO MODELO?
# ALUNOS COM MELHORES DESEMPENHOS NA 1ª AVALIAÇÃO DA DISCIPLINA TENDEM A APRESENTAR MELHORES DESEMPENHOS NA 2ª AVALIAÇÃO?
# n=27 ALUNOS MATRICULADOS NUMA DETERMINADA DISCIPLINA

# Nota na 1ª avaliação
Nota1=c(45,55,55,55,55,65,65,65,65,65,65,75,75,75,75,75,75,85,85,85,85,85,85,95,95,95,95)

# Nota na 2ª avaliação
Nota2=c(52,54,63,60,62,57,72,77,80,61,75,62,77,91,71,89,70,89,93,97,74,80,66,94,97,83,95)

plot(Nota2~Nota1, pch = 19)
modelo1 = lm(Nota2~Nota1); summary(modelo1)
anova(modelo1)

riS = rstandard(modelo1); riS

Ychapeu = fitted(modelo1); Ychapeu
#Análise gráfica dos resíduos para avaliação das hipóteses básicas do modelo


plot(riS~Ychapeu, pch = 18, ylim = c(-3,3))
abline(h = c(-2,0,2))
#A medida que aumenta a nota estimada na segunda avaliação, aumenta a var dos riS
#Indicativo de violação da hipótese de homost.


plot(riS~Nota1,pch = 18, ylim = c(-3,3))
abline(h = c(-2,0,2))
#A medida que aumenta a nota na primeira avaliação, aumenta a varaição do riS
#Indicativo de violação da hipótese de homost.



#Avaliação da hipotese de normalidade
qqnorm(riS, pch = 19,main="", xlab = "Quantis teóricos", ylab = "Quantis dos resíduos padronizados")
abline(0,1,pch=30)
shapiro.test(riS)
ks.test(riS, pnorm)


#O modelo não é adequado para representar os dados observados, 
#Violação de homost.

#alguma medida de correção precisa ser feita



# EXERCÍCIO PROPOSTO 3 - ANÁLISE DOS RESÍDUOS:
# PERGUNTA: FAMÍLIAS COM MAIOR QUANTIDADE DE FILHOS TENDEM A APRESENTAR MAIORES RENDIMENTOS?
# EXISTE ALGUM TIPO DE VIOLAÇÃO DAS HIPÓTESES BÁSICAS DO MODELO?
# n=14 FAMÍLIAS DE COMUNIDADES DE BAIXA RENDA

# Número de filhos
Nfilhos=c(3,3,3,3,4,4,4,5,5,5,6,7,7,5)

# Renda mensal de famílias residentes 
Renda=c(508.10,498.40,568.20,577.30,651.70,657.00,713.40,697.50,755.30,758.90,787.60,792.10,841.40,831.80)

# Banco de dados
banco=data.frame(Nfilhos, Renda)


plot(Renda~Nfilhos, pch = 19)
abline(modelo1)

modelo1 = lm(Renda~Nfilhos); summary(modelo1)
anova(modelo1)

riS = rstandard(modelo1); riS

Ychapeu = fitted(modelo1); Ychapeu
#Análise gráfica dos resíduos para avaliação das hipóteses básicas do modelo


plot(riS~Nfilhos, pch = 18, ylim = c(-3,3))
abline(h = c(-2,0,2))
#Violação de linearidade.

plot(riS~Nota1,pch = 18, ylim = c(-3,3))
abline(h = c(-2,0,2))
#Violação de linearidade


#Avaliação da hipotese de normalidade
qqnorm(riS, pch = 19,main="", xlab = "Quantis teóricos", ylab = "Quantis dos resíduos padronizados")
abline(0,1,pch=30)
shapiro.test(riS)
ks.test(riS, pnorm)

#O modelo não é adequado para representar os dados observados, há uma 
#Violação de linearidade