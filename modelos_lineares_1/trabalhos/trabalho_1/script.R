#Lendo a base de dados

BD = foreign::read.spss("Modelos_lineares1//trabalhos//trabalho_1//internacoes.sav")
tabela = dplyr::as_tibble(BD); tabela

#Questão a)

modelo1 = lm(tabela$Percentual_infectados~tabela$Tempo_internacao)
summary(modelo1)


#Questão b)

modelo1
anova(modelo1)

#c)

qt(0.025, 98, lower.tail = F)
qf(0.05, 1, 98, lower.tail = F)

# Questão d)
plot(tabela$Percentual_infectados~tabela$Tempo_internacao, pch = 19,
     ylab = "Percentual de pacientes infectados",
     xlab = "Tempo médio de internação (em dias)",
     ylim = c(-1,9),
     xlim = c(0,14))
abline(modelo1, col = "Royalblue")
text(2,8,expression(hat(Y[i]) == -0.14848 + 0.47580*X[i]),col = "blue")
text(1.1,7, expression(R^2 == "26.82%"), col = "blue")

# Questão e)

R = cor(tabela$Percentual_infectados,tabela$Tempo_internacao); R
R2 = R^2; R2

# Questão f)

yichapeu = fitted(modelo1)
ei = rstandard(modelo1)
plot(ei~tabela$Tempo_internacao, pch = 20, ylim = c(-3,3), ylab = "Resíduos studentizados", xlab = "Tempo médio de internação (em dias)")
abline(h = c(-2,0,2), col = c("red","black","red"),lty = c(2,1,2))
plot(ei~yichapeu, pch = 20, ylim = c(-3,3), ylab = "Resíduos studentizados", xlab = "Percentual de pacientes infectados estimado")
abline(h = c(-2,0,2), col = c("red","black","red"),lty = c(2,1,2))
qqnorm(ei, pch = 20, main="" , ylab = "Quantis teóricos normal padrão", xlab = "Quantis dos resíduos studentizados")
abline(0,1, col = "blue")
