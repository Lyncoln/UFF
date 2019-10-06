
# Leitura de dados --------------------------------------------------------

BD = foreign::read.spss("/mnt/publica/1 AREA DO ALUNO/Lyncoln/UFF/Modelos_lineares1/Aula_22/Banco_Hospital.sav")
BD = data.frame(BD)
BD = BD[,-1]


# a) ----------------------------------------------------------------------

pairs(BD,pch = 20)
cors = lapply(BD[,-3], function(x) cor(BD[,3],x)); cors


# b) ----------------------------------------------------------------------

#Yi = B0 + B1Xi1 + B2Xi2 + B3Xi3 + BD4Xi4 + Ei


# c) ----------------------------------------------------------------------

modelo1 = lm(BD$Pinfectados~BD$Tempinterna+BD$Idadem+BD$Ncamas+BD$Nenfermeiros);summary(modelo1)

#Não há significancia entre Pinfectados e Idadem,Ncamas

#Vamos remover a variável explicativa Idadem

modelo2 = lm(BD$Pinfectados~BD$Tempinterna+BD$Ncamas+BD$Nenfermeiros);summary(modelo2)

#Não há relação significativa entre Pinfectados e Ncamas


modelo3 = lm(BD$Pinfectados~BD$Tempinterna+BD$Nenfermeiros);summary(modelo3)


# d) ----------------------------------------------------------------------

#Yi^ = B0^ + B1^Xi1 + B2^Xi2
resumo3 = summary(modelo3); resumo3
r2 = resumo3$r.squared; r2
r = sqrt(r2); r #ou cor(fitted(modelo3),BD$Pinfectados)
