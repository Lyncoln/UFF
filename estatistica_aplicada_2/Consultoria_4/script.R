require(dplyr); require(reshape2); require(tidyverse)
require(sjPlot)

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
         wp = Wh*ph)

attach(BD)

wp_hat = sum(BD$wp); wp_hat
var_wp_hat = (1/sum(Nh)^2) * sum(ifelse(is.nan(Nh*(Nh-nh)*(ph*(1-ph))/(nh-1)),
                                        0,
                                        Nh*(Nh-nh)*(ph*(1-ph))/(nh-1)))
var_wp_hat


li = wp_hat - qnorm(0.975,lower.tail = T)*sqrt(var_wp_hat)
ls = wp_hat + qnorm(0.975,lower.tail = T)*sqrt(var_wp_hat)
paste0("(",round(li,3),";",round(ls,3),")")
detach(BD)

# Graficos
BD_unite <- BD_n 
BD_unite_noturno <- BD_n %>% filter(Turno == 'Noturno')
BD_unite_diurno <- BD_n %>% filter(Turno == 'Diurno')

plot_frq(BD_unite, Ano, title = "Teste", hjust = 0.2) + xlab('MUDAR')#+
#facet_wrap(facets = Turno)#+ coord_flip()
plot_frq(BD_unite_diurno, Ano, show.prc = FALSE) + xlab('Turno: Diurno')# + coord_flip()
plot_frq(BD_unite_noturno, Ano, show.prc = FALSE) + xlab('Turno: Noturno')#+ coord_flip()


plot_grpfrq(BD_unite_diurno$Ano, BD_unite_diurno$Satisfeito, 
            show.grpcnt = F, show.prc = FALSE) + xlab('Turno: Diurno') +
  scale_fill_manual(labels = c("Não", "Sim"), 
                    values = c("lightblue", "steelblue"))
plot_grpfrq(BD_unite_noturno$Ano, BD_unite_noturno$Satisfeito, 
            show.grpcnt = F, show.prc = FALSE) + xlab('Turno: Noturno')+
  scale_fill_manual(labels = c("Não", "Sim"), 
                    values = c("lightblue", "steelblue"))



# Total -------------------------------------------------------------------

BD_t = BD %>%
  select(-Nh) %>% 
  left_join(N,by = c("Turno","Ano"))
BD_t = BD_tt 

attach(BD_t)

total_chapeu = sum(Nh/nh * somah); total_chapeu
