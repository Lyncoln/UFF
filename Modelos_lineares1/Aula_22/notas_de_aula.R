# UNIVERISDDAE FEDERAL FLUMINENSE (UFF)
# INSTITUTO DE MATEMÁTICA E ESTATÍSTICA (IME)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# AULA PRÁTICA - 22ª AULA DE MODELOS LINEARES I- DATA: 27/09/2019 (LIGRE - LAB. DE ESTATÍSTICA)
# NOTAS DE AULA DO PROF. Dr.JOSÉ RODRIGO DE MORAES
# ASSUNTO: REGRESSÃO LINEAR MÚLTIPLA - TESTE DE SIGNIFICÂNCIA GERAL E INDIVIDUAL, ANÁLISE DOS RESÍDUOS

## ENTRADA DE DADOS
Dmental=c(17,19,20,20,20,21,21,22,22,23,24,24,25,26,26,26,26,27,27,27,27,28,28,28,28,28,29,30,30,31,31,31,31,32,33,34,34,34,41,41)
Evividos=c(46,39,27,3,10,44,37,35,78,32,33,18,81,22,50,48,45,21,55,45,60,97,37,30,13,40,5,59,44,35,95,63,42,38,45,70,57,40,49,89)
Pse=c(84,97,24,85,15,55,78,91,60,74,67,39,87,95,40,52,61,45,88,56,70,89,50,90,56,56,40,72,53,38,29,53,7,32,55,58,16,29,3,75)

#Ultimo exercicio dos slides das aulas 21 e 22

BD = data.frame(Dmental, Evividos, Pse)
str(BD)

pairs(BD, pch = 20)
plot(Dmental~Evividos, pch = 20)
plot(Dmental~Pse, pch = 20)

#Verificando correlação entre as variáveis explicativas (Danos no ajuste do modelo)
plot(Pse~Evividos, pch = 20)


modelo1 = lm(Dmental~Evividos+Pse); summary(modelo1)
confint(modelo1, level = 0.95)
vcov(modelo1)
x = summary(modelo1)
EP_beta = x$coefficients[,2]; EP_beta
VAR_beta = EP_beta ^ 2; round(VAR_beta,4)

ei = resid(modelo1); summary(ei)
riS = rstandard(modelo1); riS



# Análise gráfica dos resíduos --------------------------------------------

plot(riS~Evividos, pch = 20, ylim = c(-3,3)); abline(h=c(-2,0,2),col = c("red","black","red"), lty = c(2,1,2))
plot(riS~Pse, pch = 20, ylim = c(-3,3)); abline(h=c(-2,0,2),col = c("red","black","red"), lty = c(2,1,2))
plot(riS~fitted(modelo1), pch = 20, ylim = c(-3,3)); abline(h=c(-2,0,2),col = c("red","black","red"), lty = c(2,1,2))

qqnorm(riS, pch = 20);abline(0,1)
shapiro.test(riS)
ks.test(riS, pnorm, 0, 1)


# Medida de qualidade de ajuste -------------------------------------------

r2 = x$r.squared; round(r2,4)*100
r = sqrt(r2); round(r,2) # Correlação do Yi e Yichapeu
plot(fitted(modelo1)~Dmental, pch = 20); abline(0,1)
