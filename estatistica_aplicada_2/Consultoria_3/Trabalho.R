library(readxl); require(dplyr); require(likert); require(ggplot2)
require(corrplot); require(psych); require(magrittr); require(plyr)
library(readr)
# Ler base Base_ordenada.csv pois ja troquei a ordem das perguntas
base <- read_delim("Base_ordenada.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

base <- base[, -1]
# Analise fatorial --------------------------------------------------------

## Achando matriz de correlacao
corr <- polychoric(base[,4:20])
corrplot(corr$rho, method="circle", type = 'lower')

## Verificando as hipoteses
KMO(corr$rho)
cortest.bartlett(corr$rho)

# Critério de Kaiser para achar o num de fatores
fatores <- eigen(corr$rho)$values
fatores
nfatores <- length(fatores[fatores >=1])
paste0(nfatores, " são maiores que 1")

scree(rx = corr$rho, factor = FALSE, hline = 1)

# Fazendo a analise fatorial
AF1 <- fa(base[,4:20], nfactors = nfatores, cor="poly", 
          rotate = "varimax", fm="gls")
AF1$loadings
fa.diagram(AF1)

# Optamos por usar 3 fatores
AF2 <- fa(base[,4:20], nfactor = 3, cor = "poly", fm = "gls", 
          rotate = "varimax", scores = "Thurstone")
AF2$loadings
colnames(AF2$loadings) <- c("F1-Psicológico", "F2-Pessoal",
                            "F3-Ideologia")
fa.diagram(AF2, main = '')

# Melhor rotação foi o varimax AF1
AF2$loadings # Loaddings
AF2$communality # Variabilidade explicada de Zi explicada pelos fatores
AF2$uniquenesses # Variabilidade associada ao erro

# Usando a funcao factor.score
scores <- AF2$scores
score_resp <- cbind(scores[,1], base[, c(3, 4, 6, 14, 16, 17) + 3],
                    scores[,2], base[, c(1, 7, 8, 9, 10, 11, 12) + 3],
                    scores[,3], base[, c(2, 5, 13, 15) + 3])

# Regressão Linear --------------------------------------------------------

## Separando base para os modelos lineares
base <- read_excel("Base_Final.xlsx")
base_reg <- base %>% select(Tratamento, mangueira:escolaridade) %>% cbind(scores, .) 
base_reg %<>% dplyr::rename(Ideologico = GLS1, Psicologico = GLS2, 
                            Pessoal = GLS3) %>% na.omit() %>% 
  mutate(mangueira = factor(mangueira, levels = c(1, 0), 
                       labels = c("Sim", "Nao"))) %>% 
  mutate(branco = factor(branco, levels = c(1, 0), 
                       labels = c("Sim", "Nao"))) %>% 
  mutate(homem = factor(homem, levels = c(1, 0), 
                       labels = c("Sim", "Nao"))) %>% 
  mutate(mora_pai_mae = factor(mora_pai_mae, levels = c(1, 0), 
                       labels = c("Sim", "Nao"))) %>% 
  mutate(filhos = factor(filhos, levels = c(1, 0), 
                       labels = c("Sim", "Nao"))) %>% 
  mutate(escolaridade = factor(escolaridade, levels = c(0, 1, 2), 
                       labels = c("Fundamental", "Medio", "Completo"))) %>% 
  mutate(tratamento = factor(Tratamento, levels = c(1, 2), 
                            labels = c("Nao", "Sim"))) %>%
  select(Ideologico, Psicologico, Pessoal, - Tratamento, 
         mangueira:escolaridade) 
# Modelo 1 -------------------------------------------
modelo_f1 <- lm(Psicologico ~ . - Ideologico - Pessoal , data = base_reg)
summary(modelo_f1)
require(car)
vif(modelo_f1)
modelo_f1 <- lm(Psicologico ~ . - Ideologico - Pessoal - mora_pai_mae, 
                data = base_reg)
summary(modelo_f1)
modelo_f1 <- lm(Psicologico ~ . - Ideologico - Pessoal - mora_pai_mae -
                  filhos, data = base_reg)
summary(modelo_f1)
modelo_f1 <- lm(Psicologico ~ . - Ideologico - Pessoal - mora_pai_mae -
                  filhos - branco, data = base_reg)
summary(modelo_f1)
modelo_f1 <- lm(Psicologico ~ . - Ideologico - Pessoal - mora_pai_mae -
                  filhos - branco -  mangueira, data = base_reg)
summary(modelo_f1)
modelo_f1 <- lm(Psicologico ~ escolaridade, data = base_reg)
summary(modelo_f1)

# Residuos estudentizados
ris_f1 = rstandard(modelo_f1)
plot(ris_f1, pch = 19, ylim = c(-3, 3))
abline(h = c(-2, 2), col = 'red')

# Distancia de Cook
Di <- cooks.distance(modelo_f1)
plot(ris_f1 ~ Di, pch = 19,ylim = c(-3,3), xlim = c(0,.2))
abline(h = c(-2,2) , v = c(.15) , col = c("blue","blue","red","red"))
id <-  which(Di >= .1); id

# Normalidade
require(nortest)
qqnorm(ris_f1)
qqline(ris_f1)

require(ggpubr)
ggqqplot(ris_f1)

shapiro.test(ris_f1)
ks.test(ris_f1,"pnorm", mean(ris_f1), sd(ris_f1))
lillie.test(ris_f1) # Lilliefors
cvm.test(ris_f1) # Cramér-von Mises
sf.test(ris_f1) # Shapiro-Francia
ad.test(ris_f1) # Anderson-Darling

# Heterocedasticidade
skedastic::white_lm(modelo_f1, interactions = TRUE)

# Tabela modelo1
sjPlot::tab_model(modelo_f1, show.ci = 0,  show.reflvl = TRUE,
                  prefix.labels = "varname")

# Modelo 2 ----------------------------------------------------------------
modelo_f2 <- lm(Pessoal~ . - Ideologico - Psicologico, data = base_reg)
summary(modelo_f2)
modelo_f2 <- lm(Pessoal~ . - Ideologico - Psicologico - escolaridade, data = base_reg)
summary(modelo_f2)
modelo_f2 <- lm(Pessoal~ . - Ideologico - Psicologico - escolaridade -
                  mora_pai_mae, data = base_reg)
summary(modelo_f2)
modelo_f2 <- lm(Pessoal~ . - Ideologico - Psicologico - escolaridade -
                  mora_pai_mae - homem, data = base_reg)
summary(modelo_f2)
modelo_f2 <- lm(Pessoal ~ mangueira + branco, data = base_reg)
summary(modelo_f2)

# Multicolinearidade
require(car)
vif(modelo_f2)

# Residuos estudentizados
ris_f2 = rstandard(modelo_f2)
plot(ris_f2, pch = 19, ylim = c(-3, 3))
abline(h = c(-2, 2), col = 'red')

# Distancia de Cook
Di <- cooks.distance(modelo_f2)
plot(ris_f2~Di, pch = 19,ylim = c(-3,3), xlim = c(0,.2))
abline(h = c(-2,2) , v = c(.15) , col = c("blue","blue","red","red"))
id <-  which(Di >= .05); id

# Normalidade
require(nortest)
qqnorm(ris_f2)
qqline(ris_f2)

require(ggpubr)
ggqqplot(ris_f2)

shapiro.test(ris_f2)
ks.test(ris_f2,"pnorm", mean(ris_f2), sd(ris_f2))
lillie.test(ris_f2) # Lilliefors
cvm.test(ris_f2) # Cramér-von Mises
sf.test(ris_f2) # Shapiro-Francia
ad.test(ris_f2) # Anderson-Darling

# Heterocedasticidade
skedastic::white_lm(modelo_f2, interactions = TRUE)

# Tabela modelo1
sjPlot::tab_model(modelo_f2, show.ci = 0,  show.reflvl = TRUE,
                  prefix.labels = "varname")

# Modelo 3 TEMOS UM PROBLEMA!!!!!!!!!!!! ---------------------------------------
modelo_f3 <- lm(Ideologico ~ . - Psicologico - Pessoal , data = base_reg)
summary(modelo_f3)
modelo_f3 <- lm(Ideologico ~ . - Psicologico - Pessoal - mora_pai_mae, data = base_reg)
summary(modelo_f3)
modelo_f3 <- lm(Ideologico ~ escolaridade, data = base_reg)
summary(modelo_f3)

# Multicolinearidade
require(car)
vif(modelo_f3)

# Residuos estudentizados
ris_f3 = rstandard(modelo_f3)
plot(ris_f3, pch = 19, ylim = c(-4,4))
abline(h = c(-2, 2), col = 'red')

# Distancia de Cook
Di <- cooks.distance(modelo_f3)
plot(ris_f3~Di, pch = 19,ylim = c(-3,3), xlim = c(0,.2))
abline(h = c(-2,2) , v = c(.15) , col = c("blue","blue","red","red"))
id <-  which(Di >= .15); id

# Tirando observacao 264 (nao alterou nada)
# base_reg2 <- base_reg[-264, ]
# modelo_f3 <- lm(Ideologico ~ escolaridade, data = base_reg2)
# summary(modelo_f3)

# Normalidade
require(nortest)
qqnorm(ris_f3)
qqline(ris_f3)

require(ggpubr)
ggqqplot(ris_f3)

shapiro.test(ris_f3)
ks.test(ris_f3,"pnorm", mean(ris_f3), sd(ris_f3))
lillie.test(ris_f3) # Lilliefors
cvm.test(ris_f3) # Cramér-von Mises
sf.test(ris_f3) # Shapiro-Francia
ad.test(ris_f3) # Anderson-Darling

# Heterocedasticidade
skedastic::white_lm(modelo_f3, interactions = TRUE)

# Tabela modelo1
sjPlot::tab_model(modelo_f3, show.ci = 0,  show.reflvl = TRUE,
                  prefix.labels = "varname")
