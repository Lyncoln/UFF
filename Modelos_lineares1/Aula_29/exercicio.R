# UNIVERSIDADE FEDERAL FLUMINENSE (UFF)
# INSTITUTO DE MATEMÁTICA E ESTATÍSTCIA (IME)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# DADOS DO EXEMPLO DE APLICAÇÃO - VOLTANDO AO EXEMPLO DE RLM
# ASSUNTO: TESTE F DE COMPARABILIDADE DE MODELOS

# ENTRADA DE DADOS (ONDE Y=SALARIO DE N=25 FUNCIONÁRIOS)
Sal=c(35,25,22,39,23,30,26,30,38,40,45,37,43,22,27,17,29,27,35,19,25,29,32,28,19)
Idade=c(48,25,23,55,40,42,24,38,49,52,57,47,48,22,48,21,45,40,43,23,30,31,35,34,21)
Tempo=c(15,2,1,20,8,10,4,6,19,22,25,17,25,1,7,1,21,17,20,5,10,13,17,15,3)


BD = data.frame(Sal,Idade,Tempo);str(BD)
pairs(BD, pch = 19)
cor(BD)

modelo1 = lm(Sal~Idade+Tempo);summary(modelo1)

####

modelo_idade = lm(Idade~Tempo);summary(modelo_idade)
R2_idade = summary(modelo_idade)$r.squared; R2_idade
VIF_idade = 1 / (1-R2_idade);VIF_idade

modelo_tempo = lm(Idade~Tempo);summary(modelo_tempo)
R2_tempo = summary(modelo_tempo)$r.squared; R2_tempo
VIF_exp = 1 / (1-R2_tempo);VIF_exp


###Análise dos resíduos

ris = rstandard(modelo1);summary(ris)
qqnorm(ris,pch = 19); abline(0,1)

sal_est = fitted(modelo1)

plot(ris~sal_est, pch = 19, ylim = c(-3,3)); abline(h = c(-2,0,2))
plot(ris~Idade, pch = 19, ylim = c(-3,3)); abline(h = c(-2,0,2))
plot(ris~Tempo, pch = 19, ylim = c(-3,3)); abline(h = c(-2,0,2))

