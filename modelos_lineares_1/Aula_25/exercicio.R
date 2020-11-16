#Refazendo o exer da aula 22 com o teste F de comparação de modelos

BD = foreign::read.spss("/home/216054055/UFF/Modelos_lineares1/Aula_25/Banco_Hospital.sav")
BD = data.frame(BD)
BD = BD[,-1]


# a) ----------------------------------------------------------------------

pairs(BD,pch = 20)
cors = cor(BD); cors

# b) ----------------------------------------------------------------------

#Yi = B0 + B1Xi1 + B2Xi2 + B3Xi3 + BD4Xi4 + Ei


# c) ----------------------------------------------------------------------
modelo0 = lm(BD$Pinfectados~1); summary(modelo0)
modelo1 = lm(BD$Pinfectados~BD$Tempinterna+BD$Idadem+BD$Ncamas+BD$Nenfermeiros);summary(modelo1)
modelo2 = lm(BD$Pinfectados~BD$Tempinterna+BD$Nenfermeiros+BD$Ncamas);summary(modelo2)
modelo3 = lm(BD$Pinfectados~BD$Tempinterna+BD$Nenfermeiros);summary(modelo3)
modelo4 = lm(BD$Pinfectados~BD$Tempinterna); summary(modelo4)

anova(modelo0,modelo1) #Existe pelo menos uma variável explicativa significante

#H0 : Prefere-se Modelo reduzido X H1: Prefer-se modelo completo


anova(modelo2,modelo1) #Prefere-se o modelo reduzido modelo2
anova(modelo3,modelo2) #Prefere-se o modelo reduzido modelo3
anova(modelo4,modelo3) #prefere-se o modelo completo
