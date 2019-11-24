require(dplyr)

data = foreign::read.spss("tvida.sav", to.data.frame = TRUE)
head(data)

colnames(data) = tolower(colnames(data))

data = data %>% mutate(sexo = ifelse(sexo == "Mulher",1, 0),
                       alcool_severo = ifelse(historia_alcool == "Uso severo", 1, 0),
                       alcool_moderado = ifelse(historia_alcool == "Uso moderado", 1, 0)) %>% select(-historia_alcool) %>% 
  select(ln_tempo_vida, everything())

head(data)


# a) ----------------------------------------------------------------------
modelo_1 = lm(ln_tempo_vida ~ idade + sexo + alcool_severo + alcool_moderado, data); summary(modelo_1)

qt(0.05/2, 54-5,lower.tail = F)

# Variável idade não se mostrou significante a um nível de 5%.
modelo_2 = lm(ln_tempo_vida ~ sexo + alcool_severo + alcool_moderado, data)

# b) ----------------------------------------------------------------------
modelo_3 = lm(ln_tempo_vida ~ sexo, data); summary(modelo_3)
anova(modelo_3, modelo_2)
qf(0.05, 2, 50, lower.tail = F)

modelo_4 = lm(ln_tempo_vida ~ alcool_severo + alcool_moderado, data); summary(modelo_4)
anova(modelo_4, modelo_2)
qf(0.05, 1, 50, lower.tail = F)

# c) ----------------------------------------------------------------------
ris = rstandard(modelo_2)
ychapeu = fitted(modelo_2)

plot(ris~ychapeu, 
     pch = 19, 
     ylim = c(-3,3),
     ylab = "Resíduos Estudentizados",
     xlab = latex2exp::TeX("$\\hat{Y}$"));
abline(h = c(-2,0,2),
       lty = c(2,1,2),
       col = c("red","black","red"))

lmtest::bptest(modelo_2)

qqnorm(ris, 
       pch = 19,
       main = "",
       ylab = "Quantis amostrais",
       xlab = "Quantis teóricos"); abline(0,1)
shapiro.test(ris)

# d) ----------------------------------------------------------------------
summary(modelo_2)
summary(modelo_2)$r.squared
