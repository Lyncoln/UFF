# 1) ----------------------------------------------------------------------

# a)
data_1 = read.csv('tabela_1.csv')[, -1]
modelo_1 = lm(data_1)
summary(modelo_1)
plot(data_1, pch = 19)

# b)
# Equação do modelo: ychap_i = Bchap_o + Bchap_1 * X_i
# Ychap_i = 3593.3 - 390.5 * X_i
# Interpretação:
# 3593.3 valor estimado do peso da criança quando a mãe não fumou
# O valor estimado do peso da criança aumenta caso a mãe não fume. E esse acréssimo
# é de 390.5;

ris = rstandard(modelo_1)
qqnorm(ris, pch = 19)
abline(0, 1)

# Os residuos apresentam distribuição normal
# Existe relação significante entre fumo e o peso da criança

# c)
r2 = summary(modelo_1)$r.squared
r2

# Aproximadamente 17,9% da variação do peso das crianças é explicada pelo modelo.

# d)
cook = cooks.distance(modelo_1)
plot(cook,
     ris,
     pch = 19,
     ylim = c(-5, 5),
     xlim = c(0, 1.2))
abline(v = 0.1, h = c(-2, 2))
id = which(cook > 0.1)
text(cook[id] + c(0.01, 0.014, 0.018),
     ris[id],
     id,
     pos = 4,
     cex = 0.8)

data_1 = data_1[-id,]
modelo_2 = lm(data_1)
summary(modelo_2)
# Houve um aumento de aproximadamente 11% na qualidade do ajuste.

# e)
ris = rstandard(modelo_1)
qqnorm(ris, pch = 19)
abline(0, 1)
shapiro.test(ris)
ris = rstandard(modelo_2)
qqnorm(ris, pch = 19)
abline(0, 1)
shapiro.test(ris)
# Ambos os residuos são normais

# 2) ----------------------------------------------------------------------
# a)
data_2 = read.table('tabela_2.tsv', header =  T)[, -1][, c(4, 1, 2, 3)]

modelo_1 = lm(data_2[, c(1, 2)])
modelo_2 = lm(data_2[, c(1, 3)])
modelo_3 = lm(data_2[, c(1, 4)])
modelo_4 = lm(data_2[, c(1, 2, 4)])
modelo_5 = lm(data_2[, c(1, 3, 4)])
modelo_6 = lm(data_2[, c(1, 2, 3)])
modelo_7 = lm(data_2)

summary(modelo_1)
summary(modelo_2)
summary(modelo_3)
summary(modelo_4)
summary(modelo_5)
summary(modelo_6)
summary(modelo_7)

# b)
anova(modelo_7, modelo_6)
anova(modelo_7, modelo_5)
anova(modelo_7, modelo_4)

# Escolhe-se o completo

# c)
which.max(
  c(
    summary(modelo_1)$adj.r.squared,
    summary(modelo_2)$adj.r.squared,
    summary(modelo_3)$adj.r.squared,
    summary(modelo_4)$adj.r.squared,
    summary(modelo_5)$adj.r.squared,
    summary(modelo_6)$adj.r.squared,
    summary(modelo_7)$adj.r.squared
  )
)
max(
  c(
    summary(modelo_1)$adj.r.squared,
    summary(modelo_2)$adj.r.squared,
    summary(modelo_3)$adj.r.squared,
    summary(modelo_4)$adj.r.squared,
    summary(modelo_5)$adj.r.squared,
    summary(modelo_6)$adj.r.squared,
    summary(modelo_7)$adj.r.squared
  )
)

# Com base apenas no poder de explciaçãoe escolhe-se o modelo completo

# d)
ris = rstudent(modelo_7)
plot(ris ~ fitted(modelo_7), pch = 19, ylim = c(-3,3)); abline(h=c(-2,0,2))
qqnorm(ris, pch=19); abline(0,1)

# As hipoteses foram satisfeitas

# 3) ----------------------------------------------------------------------

data_3 = read.table('tabela_3.tsv', header =  T)[, -1][,c(3,1,2)]
# a)

qqnorm(data_3$altura, pch=19); qqline(data_3$altura)

shapiro.test(data_3$altura)

# b)
data_3$grupo = ifelse(data_3$grupo == 2, 0, 1)
modelo_1 = lm(data_3[,c(1,2)]); summary(modelo_1)

# c)
modelo_2 = lm(data_3); summary(modelo_2)

# d)
anova(modelo_2, modelo_1)
# A variável idade é significante para o ajuste do modelo

# e) 
ris = rstudent(modelo_2)
qqnorm(ris, pch = 19); abline(0,1)
# Parece normal
plot(ris ~ fitted(modelo_2), pch = 19, ylim = c(-3,3)); abline(h = c(-2,0,2))
# A variância parece crescer de forma proporcional

# f)
cook = cooks.distance(modelo_2)
outliers = which(abs(ris) > 2); outliers
influentes = which(abs(cook) > .1); influentes
plot(ris ~ cook, pch = 19, ylim = c(-3,3), xlim = c(0,0.8)); abline(h = c(-2,2), v = 0.1)

data_3 = data_3[-influentes,]
modelo_3 = lm(data_3); summary(modelo_3)
# Houve uma melhoria de aproximadamente 5% no ajuste do modelo

# 4) ----------------------------------------------------------------------
data_4 = read.table('tabela_4.tsv', header =  T)[, -1]

# a)
modelo_1 = lm(data_4[,c(1,2)])
modelo_2 = lm(data_4[,c(1,3)])
modelo_3 = lm(data_4[,c(1,4)])
summary(modelo_1)
summary(modelo_2)
summary(modelo_3)

# b)
# SPC explica melhor HDL
modelo_4 = lm(data_4); summary(modelo_4)

