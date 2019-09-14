BD = foreign::read.spss("Modelos_lineares1//trabalhos//trabalho_1//internacoes.sav")

# Transformando a base de dados em um tibble para melhor visualização
tabela = dplyr::as_tibble(BD); tabela

# Gerando um gráfico de dispersão de percentual de infectados x tempo de internação
plot(tabela$Percentual_infectados~tabela$Tempo_internacao, pch = 19,
     ylab = "Percentual de infectados",
     xlab = "Tempo de internação")
abline(modelo1)
# É possível observar uma relação linear crescente, isto é, quanto maior o tempo de internação, 
# Maior tende a ser o percentual de infectados.

# Criando e analisando o modelo ajustado
modelo1 = lm(tabela$Percentual_infectados~tabela$Tempo_internacao)
summary(modelo1)
yichapeu = fitted(modelo1)

#Análise gráfica dos resíduos para avaliação das hipóteses básicas do modelo
ei = rstandard(modelo1)
plot(ei~tabela$Tempo_internacao, pch = 20, ylim = c(-3,3), ylab = "Resíduos studentizados", xlab = "Tempo de internação")
plot(ei~yichapeu, pch = 20, ylim = c(-3,3), ylab = "Resíduos studentizados", xlab = "Percentual de infectados estimado")
abline(h = c(-2,0,2), col = c("red","black","red"))
qqnorm(ei, pch = 20, main="" , ylab = "Quantis teóricos", xlab = "Quantis dos resíduos studentizados")
abline(0,1, col = "blue")
#Nenhuma hipótese do modelo foi violada
