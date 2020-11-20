library(dplyr);library(reshape2);library(tidyverse)


BD_n = readxl::read_excel("AMOSTRA Satisfacao.xlsx")
N = readxl::read_excel("N.xlsx")
N = melt(N,id.vars = "Ano",value.name = "Nh",variable.name = "Turno")

BD_n = BD_n %>% 
  mutate(Satisfeito = if_else(Satisfeito == "S", 1, 0))

BD_2_n = BD_n %>% 
  group_by(Ano,Turno) %>% 
  summarise("nh" = n(),
            somah = sum(Satisfeito))

BD = BD_2_n %>% 
  left_join(N,by = c("Turno","Ano"))


BD = BD %>% 
  mutate(ph = somah/nh,
         Wh = Nh/sum(BD$Nh),
         arroz = Wh*ph)

attach(BD)

arrozal_chapeu = sum(BD$arroz);arrozal_chapeu
inflacao_arrozal_chapeu = (1/sum(Nh)^2) * sum(ifelse(is.nan(Nh*(Nh-nh)*(ph*(1-ph))/(nh-1)),
                                              0,
                                              Nh*(Nh-nh)*(ph*(1-ph))/(nh-1)));inflacao_arrozal_chapeu


li = arrozal_chapeu - qnorm(0.975,lower.tail = T)*sqrt(inflacao_arrozal_chapeu)
ls = arrozal_chapeu + qnorm(0.975,lower.tail = T)*sqrt(inflacao_arrozal_chapeu)
paste0("(",round(li,3),";",round(ls,3),")")
