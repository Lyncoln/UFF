
# 5) ----------------------------------------------------------------------

BD_5 <- read_table2("F:/GitHub/UFF/Modelos_lineares1/lista_3/BD_5.txt",
                    col_names = FALSE)
names(BD_5) = c("aluno","carboidrato","proteina","peso","idade")
modelo1 = lm(BD_5$carboidrato~BD_5$proteina+ BD_5$peso + BD_5$idade); summary(modelo1)
modelo2 = lm(BD_5$carboidrato~BD_5$proteina+ BD_5$peso ); summary(modelo2)

anova(modelo2, modelo1)

anova(modelo2)
anova(modelo1)

modelo3 = lm(BD_5$carboidrato~BD_5$proteina)

anova(modelo3,modelo2)

x = anova(modelo2)

#Escolhe-se o modelo2

cor(BD_5[,-1])

plot(fitted(modelo2),BD_5$carboidrato, pch = 19);abline(0,1)

ris = rstandard(modelo3)

qqnorm(ris, pch = 19); abline(0,1)

plot(ris~fitted(modelo2),ylim = c(-3,3),pch = 19);abline(h = c(-2,0,2))



# 6) ----------------------------------------------------------------------
library(dplyr)

BD_6 <- read_table2("F:/GitHub/UFF/Modelos_lineares1/lista_3/BD_6.txt",
                    col_names = FALSE)
#a)
x = BD_6[1:20,2:4]
names(x) = c("dist_mental","eventos_vividos","pse")
y = BD_6[1:20,6:8]             
names(y) = c("dist_mental","eventos_vividos","pse")
BD_6 = rbind(x,y)
pairs(BD_6, pch = 19)
plot(BD_6$dist_mental~BD_6$eventos_vividos, pch = 19)
plot(BD_6$dist_mental~BD_6$pse, pch = 19)
cor(BD_6)
#b)
BD_6 = BD_6 %>% 
  mutate("eve*pse" = eventos_vividos*pse)
modelo1 = lm(BD_6$dist_mental~BD_6$eventos_vividos+BD_6$pse+BD_6$`eve*pse`);summary(modelo1)
modelo2 = lm(BD_6$dist_mental~BD_6$eventos_vividos+BD_6$pse); summary(modelo2)

anova(modelo2,modelo1)
#Prefere-se o reduzido modelo2

modelo3 = lm(BD_6$dist_mental~BD_6$eventos_vividos)
anova(modelo3,modelo2) # Prefere-se o modelo completo modelo2

modelo4 = lm(BD_6$dist_mental~BD_6$pse)
anova(modelo4,modelo2) # Prefere-se o modelo completo modelo2

summary(modelo2)
#d)
#Intervalo de confiança

eventos_vividos = BD_6$eventos_vividos
pse = BD_6$pse
disturbio = BD_6$dist_mental
xi = data.frame(eventos_vividos = 45,
                pse = 56)

modelo2 = lm(disturbio~eventos_vividos+pse)

predict(modelo2, xi, interval = "confidence")

#e)

predict(modelo2, xi, interval = "predict")
