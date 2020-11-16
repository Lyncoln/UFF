# UNIVERSIDADE FEDERAL FLUMINENSE (UFF)
# INSTITUTO DE MATEMÁTICA E ESTATÍSTICA (IME)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# 23-25ª AULAS DE MODELOS LINEARES I (AULA PRÁTICA - DATA: 04/10/29)
# EXERCICIO 1: DADOS SOBRE MEDIDAS ANTROPOMETRICAS (EXEMPLO DAS NOTAS DE AULA)

# MODELO DE REGRESSÃO LINEAR MÚLTIPLA - COMPARAÇÃO/SELEÇÃO DE MODELO     
Esp_pele_X1=c(19.5,24.7,30.7,29.8,19.1,25.6,31.4,27.9,22.1,25.5,31.1,30.4,18.7,19.7,14.6,29.5,27.7,30.2,22.7,25.2)
Perim_coxa_X2=c(43.1,49.8,51.9,54.3,42.2,53.9,58.5,52.1,49.9,53.5,56.6,56.7,46.5,44.2,42.7,54.4,55.3,58.6,48.2,51.0)
Perim_braco_X3=c(29.1,28.2,37.0,31.1,30.9,23.7,27.6,30.6,23.2,24.8,30.0,28.3,23.0,28.6,21.3,30.1,25.7,24.6,27.1,27.5)
Qde_gordura_Y=c(11.9,22.8,18.7,20.1,12.9,21.7,27.1,25.4,21.3,19.3,25.4,27.2,11.7,17.8,12.8,23.9,22.6,25.4,14.8,21.1)

modelo1 = lm(Qde_gordura_Y~Esp_pele_X1);summary(modelo1)
modelo2 = lm(Qde_gordura_Y~Perim_coxa_X2);summary(modelo2)
modelo3 = lm(Qde_gordura_Y~Esp_pele_X1+Perim_coxa_X2);summary(modelo3)
modelo4 = lm(Qde_gordura_Y~Esp_pele_X1+Perim_coxa_X2+Perim_braco_X3);summary(modelo4)

#Exemplo slide 24: anova(reduzido,completo)
anova(modelo3,modelo4) #Escolhe-se o modelo reduzido (modelo3)
#Exclui-se a variável X3 = perimetro do braço
#anova(completo)
anova(modelo4)

#Exemplo slide 32: anova(reduzido, completo)

anova(modelo1, modelo4) #Escolhe-se o modelo completo

anova(modelo2, modelo3) 
anova(modelo1, modelo3) 


#Conclusão: O modelo selecionado é o modelo de regressão linear simples 
#Somente com a varável X2 = perímetro da coxa.

#Análise gráfica dos resíduos

ris = rstandard(modelo2); summary(ris)
qqnorm(ris, pch = 19);abline(0,1)
shapiro.test(ris)

Ychapeu = fitted(modelo2)
R = cor(Ychapeu, Qde_gordura_Y) # Coeficiente de correlação múltipla
plot(Ychapeu, Qde_gordura_Y, pch = 19);abline(0,1)

plot(ris~Perim_coxa_X2, pch = 19,ylim = c(-3,3)) ;abline(h=c(-2,0,2),col = c("red","black","red"), lty = c(2,1,2))
plot(ris~Ychapeu, pch = 19,ylim = c(-3,3)) ;abline(h=c(-2,0,2),col = c("red","black","red"), lty = c(2,1,2))

plot(Qde_gordura_Y~Perim_coxa_X2, pch = 19)


###Ex. do slide 40- página 7

rm(list = ls())

# UNIVERSIDADE FEDERAL FLUMINENSE (UFF)
# INSTITUTO DE MATEMÁTICA E ESTATÍSTICA (IME)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# 23-25ª AULAS DE MODELOS LINEARES I (AULA PRÁTICA - DATA: 04/10/29)
# PROF.: DR. JOSÉ RODRIGO DE MORAES (DEPARTAMENTO DE ESTATÍSTICA - GET/UFF)
# ASSUNTO: MODELO DE REGRESSÃO LINEAR MÚLTIPLA - COMPARAÇÃO/SELEÇÃO DE MODELO  
# ENTRADA DE DADOS SOBRE OS ESCORES EVENTOS VIVIDOS (X1), PSE (X2) E ÍNDICE DE DISTÚRBIO MENTAL (Y)
# Y = ìndice de disturbio mental
# X1 = Eventos vividos
# X2 = Pse 

Dmental=c(17,19,20,20,20,21,21,22,22,23,24,24,25,26,26,26,26,27,27,27,27,28,28,28,28,28,29,30,30,31,31,31,31,32,33,34,34,34,41,41)
Evividos=c(46,39,27,3,10,44,37,35,78,32,33,18,81,22,50,48,45,21,55,45,60,97,37,30,13,40,5,59,44,35,95,63,42,38,45,70,57,40,49,89)
Pse=c(84,97,24,85,15,55,78,91,60,74,67,39,87,95,40,52,61,45,88,56,70,89,50,90,56,56,40,72,53,38,29,53,7,32,55,58,16,29,3,75)

banco = data.frame(Dmental, Evividos, Pse); str(banco)
pairs(banco, pch = 19)
cor(banco)

#Modelos:
modelo0 = lm(Dmental~1)
modelo1 = lm(Dmental~Evividos+Pse,data = banco)
modelo2 = lm(Dmental~Evividos,data = banco)
modelo3 = lm(Dmental~Pse,data = banco)

#Teste de significancia F global
#Comparação do modelo nulo com o modelo completo, com todas
#As variáveis explicativas
modelo0 = lm(Dmental~1);summary(modelo0)
anova(modelo0, modelo1)
#mesma coisa do teste F de summary(modelo1)

modelo1 = lm(Dmental~Evividos+Pse,data = banco); summary(modelo1)
modelo2 = lm(Dmental~Evividos,data = banco); summary(modelo2)
modelo3 = lm(Dmental~Pse,data = banco); summary(modelo3)
anova(modelo2, modelo1) #Adição de Pse melhora predição do modelo
#Escolhe-se o modelo completo com amabas variáveis explicativas
#Evividos e Pse
anova(modelo3,modelo1)
#Como p-valor= 0.002 < alfa=0.05 rejeita-se H0 
#Ao nível de significância de 5%, ou seja, escolhe-se
#O modelo completo: o modelo com ambas as variáveis explicativas,
#"Eventos vividos" e "Pse"


#Conclusão final: O modelo selecionado é o modelo1
#Com ambas as variáveis

#Análise de resíduos

ris = rstandard(modelo1); summary(ris)
qqnorm(ris, pch = 19); abline(0,1)
shapiro.test(ris)

Ychapeu = fitted(modelo1)
plot(ris~Ychapeu, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2),col = c("red","black","red"), lty = c(2,1,2))
