# UNIVERSIDADE FEDERAL FLUMINENSE (UFF)
# INSTITUTO DE MATEMÁTICA E ESTATÍSTICA (IME)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# DADOS DO EXEMPLO DE APLICAÇÃO - DATA: 13/11/2019 DE MODELOS LINEARES I (AULA PRÁTICA - 2019.2 na 4ª Feira)
# ASSUNTO: VARIÁVEIS DO TIPO DUMMY (VARS INDICADORAS) - DATA: 13/11/2019 (4ª FEIRA - Pois 6ª feira Feriado)
# ENTRADA DE DADOS - NOTAS DE AULA DO PROF. DR. JOSÉ RODRIGO DE MORAES (GET/UFF)

Matern=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B","C","C","C","C","C","C","C","C","C","C")
Peca_roup=c("Sim","Sim","Sim","Sim","Sim","Não","Não","Não","Não","Não","Sim","Sim","Sim","Sim","Sim","Não","Não","Não","Não","Não","Sim","Sim","Sim","Sim","Sim","Não","Não","Não","Não","Não")
Tempo_dorm=c(2,5,4,6,5,9,7,5,6,5,3,6,6,5,5,9,7,5,6,5,9,7,7,4,5,9,9,8,9,7)

# CRIANDO O BANCO DE DADOS - DATA.FRAME
banco=data.frame(Matern, Peca_roup,Tempo_dorm);banco; str(banco)

###MODO 1 ####

matern_A = ifelse(Matern == "A", 1, 0)
matern_B = ifelse(Matern == "B", 1, 0)
peca_roupa = ifelse(Peca_roup == "Sim", 1, 0) 

banco=data.frame(Matern,
                 matern_A,
                 matern_B,
                 Peca_roup,
                 peca_roupa,
                 Tempo_dorm);banco; str(banco)

modelo1 = lm(Tempo_dorm~matern_A+matern_B+peca_roupa); summary(modelo1)
modelo2 = lm(Tempo_dorm~matern_A+matern_B); summary(modelo2)
modelo3 = lm(Tempo_dorm~peca_roupa); summary(modelo3)

#Teste F de comparabilidade de modelos encaixados

anova(modelo2, modelo1)
anova(modelo3, modelo1)

#Conclusão final: Escolhe-se o modelo completo com as variáveis tipo de maternidade e
#Presença de peça de roupa da mãe. (modelo1)
summary(modelo1)

y_chapeu = fitted(modelo1)
banco$Ychapeu = y_chapeu

ris = rstandard(modelo1)

qqnorm(ris, pch = 19); abline(0,1)

plot(ris ~ y_chapeu, pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2), lty = c(2,1,2))

library(ggplot2)

ggplot(banco, aes(x = Matern, y = Tempo_dorm, color = Peca_roup)) +
  geom_point()

ggplot(banco, aes(x = as.numeric(Matern), y =  Ychapeu, color = Peca_roup)) +
  geom_line()+
  ylim(0,10)




###MODO 2 ####

str(banco)
modelo1 = lm(Tempo_dorm~Matern+Peca_roup); summary(modelo1)
modelo1 = lm(Tempo_dorm~Matern+Peca_roup, contrasts = list(Matern = "contr.SAS",
                                                           Peca_roup = "contr.SAS")); summary(modelo1)
modelo1 = lm(Tempo_dorm~Matern+Peca_roup, contrasts = list(Matern = "contr.SAS")); summary(modelo1)
             