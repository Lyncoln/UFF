
require(dplyr)
require(foreign)

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


# b) ----------------------------------------------------------------------

modelo_2 = lm(ln_tempo_vida ~ idade + sexo, data); summary(modelo_2)
anova(modelo_2, modelo_1)

modelo_3 = lm(ln_tempo_vida ~ idade + alcool_severo + alcool_moderado, data)
anova(modelo_3, modelo_1)



# c) ----------------------------------------------------------------------

modelo_4 = lm(ln_tempo_vida ~ sexo + alcool_severo + alcool_moderado, data) #Modelos em idade
anova(modelo_4, modelo_1)

ris = rstandard(modelo_4)
ychapeu = fitted(modelo_4)

plot(ris~ychapeu, pch = 19, ylim = c(-3,3));abline(h = c(-2,0,2),
                                                   lty = c(2,1,2),
                                                   col = c("red","black","red"))
qqnorm(ris, pch = 19);abline(0,1)
shapiro.test(ris)



# d) ----------------------------------------------------------------------


