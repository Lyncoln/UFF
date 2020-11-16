library(readxl); require(dplyr); require(likert); require(magrittr); require(plyr)

base <- read_excel("Base_Final.xlsx")

# Recodificando as variaveis  ---------------------------------------------
## Invertendo o sentido das questoes Q1 ate Q7, continuam sendo numeric

base = base %>% mutate(Q1 = if_else(Q1 == 1, 5,
                                     if_else(Q1 == 2, 4, 
                                             if_else(Q1 == 4, 2, 
                                                     if_else(Q1 == 5, 1, 3)))))
base = base %>% mutate(Q2 = if_else(Q2 == 1, 5,
                                      if_else(Q2 == 2, 4, 
                                              if_else(Q2 == 4, 2, 
                                                      if_else(Q2 == 5, 1, 3)))))
base = base %>% mutate(Q3 = if_else(Q3 == 1, 5,
                                      if_else(Q3 == 2, 4, 
                                              if_else(Q3 == 4, 2, 
                                                      if_else(Q3 == 5, 1, 3)))))
base = base %>% mutate(Q4 = if_else(Q4 == 1, 5,
                                      if_else(Q4 == 2, 4, 
                                              if_else(Q4 == 4, 2, 
                                                      if_else(Q4 == 5, 1, 3)))))
base = base %>% mutate(Q5 = if_else(Q5 == 1, 5,
                                      if_else(Q5 == 2, 4, 
                                              if_else(Q5 == 4, 2, 
                                                      if_else(Q5 == 5, 1, 3)))))
base = base %>% mutate(Q6 = if_else(Q6 == 1, 5,
                                      if_else(Q6 == 2, 4, 
                                              if_else(Q6 == 4, 2, 
                                                      if_else(Q6 == 5, 1, 3)))))
write.csv2(base, file = 'Base_ordenada.csv')

### Colocando como fatores
bd_1 = base %>% select(4:9) %>% 
  mutate_all(~ factor(.,levels = 1:5, labels = c("Concordo fortemente",
                                                 "Concordo",
                                                 "Não concordo nem discordo",
                                                 "Discordo",
                                                 "Discordo fortemente")))

bd_2 = base %>% select(10:15) %>% 
  mutate_all(~ factor(.,levels = 1:5, labels = c("Concordo fortemente",
                                                 "Concordo",
                                                 "Não concordo nem discordo",
                                                 "Discordo",
                                                 "Discordo fortemente")))

bd_3 = base %>% select(16:20) %>%
  mutate_all(~ factor(.,levels = 1:7, labels = c("Concordo fortemente",
                                                 "Concordo",
                                                 "Concordo parcialmente",
                                                 "Não concordo nem discordo",
                                                 "Discordo parcialmente",
                                                 "Discordo",
                                                 "Discordo fortemente")))

bd_labels = cbind(bd_1, bd_2, bd_3)

base_graf <- base %>% na.omit() %>% 
  mutate(Mangueira = factor(mangueira, levels = c(1, 0), 
                            labels = c("Sim", "Nao"))) %>% 
  mutate(Branco = factor(branco, levels = c(1, 0), 
                         labels = c("Sim", "Nao"))) %>% 
  mutate(Homem = factor(homem, levels = c(1, 0), 
                        labels = c("Sim", "Nao"))) %>% 
  mutate(Mora_pai_mae = factor(mora_pai_mae, levels = c(1, 0), 
                               labels = c("Sim", "Nao"))) %>% 
  mutate(Filhos = factor(filhos, levels = c(1, 0), 
                         labels = c("Sim", "Nao"))) %>% 
  mutate(Escolaridade = factor(escolaridade, levels = c(0, 1, 2), 
                               labels = c("Fundamental", "Medio", "Completo"))) %>% 
  mutate(Tratamento = factor(Tratamento, levels = c(1, 2), 
                             labels = c("Nao", "Sim"))) %>%
  select(Tratamento, Mangueira:Escolaridade) 

# Analise Descirtiva ------------------------------------

likert.bar.plot(likert(bd_1), group.order = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Q6'), 
                legend = "Escala", plot.percents= TRUE, digits = 2) +
  ggtitle("Escala Likert para as perguntas Q1 até Q6") + labs(y = "") 

likert.bar.plot(likert(bd_2), group.order = c('Q7', 'Q8', 'Q9', 'Q10', 'Q11', 'Q12'), 
                legend = "Escala", plot.percents= TRUE, digits = 2) +
  ggtitle("Escala Likert para as perguntas Q7 até Q12") + labs(y = "") 

likert.bar.plot(likert(bd_3),  group.order = c('Q13', 'Q14', 'Q15', 'Q16', 'Q17'), 
                legend = "Escala", plot.percents= TRUE, digits = 2) +
  ggtitle("Escala Likert para as perguntas Q13 até Q17") + labs(y = "")

rm(bd_1); rm(bd_2); rm(bd_3)

require(sjPlot)
w <- 9.7 ; h <- 4 
setwd("/Relatorio")
set_theme(base = theme_light()) 
p1 <- plot_frq(Tratamento, data = base_graf) + ylab("Frequência")
ggsave("Graficos\\p1.png", plot = p1, width = w, height = h)
p2 <- plot_frq(Escolaridade, data = base_graf) + ylab("Frequência")
ggsave("Graficos\\p2.png", plot = p2, width = w, height = h)
p3 <- plot_frq(Branco, data = base_graf) + ylab("Frequência")
ggsave("Graficos\\p3.png", plot = p3, width = w, height = h+1.5)
p4 <- plot_frq(Mangueira, data = base_graf) + ylab("Frequência")
ggsave("Graficos\\p4.png", plot = p4, width = w, height = h+1.5)
p5 <- plot_frq(Homem, data = base_graf) + ylab("Frequência")
ggsave("Graficos\\p5.png", plot = p5, width = w, height = h)
p6 <- plot_frq(Mora_pai_mae, data = base_graf) + xlab("Mora com pai e mãe")
ggsave("Graficos\\p6.png", plot = p6, width = w, height = h+1.5)
p7 <- plot_frq(Filhos, data = base_graf) + ylab("Frequência")
ggsave("Graficos\\p7.png", plot = p7, width = w, height = h)

gridExtra::grid.arrange(p1,p5,p4, ncol = 3)
gridExtra::grid.arrange(p3,p6,p7, ncol = 3)


.require(xtable)
Tratamento <- table(base_graf$Tratamento)
Branco <- table(base_graf$Branco)
Mangueira <- table(base_graf$Mangueira)
Homem <- table(base_graf$Homem)
Mora_pai_mae <- table(base_graf$Mora_pai_mae)
Filhos <- table(base_graf$Filhos)
tabela <- cbind(Tratamento, Branco, Mangueira, Homem, Mora_pai_mae, Filhos)
xtable(tabela)
xtable(table(base_graf$Escolaridade))
