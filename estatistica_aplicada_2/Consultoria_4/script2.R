BD_3 = BD_n %>% 
  left_join(N,by = c("Ano","Turno"))

BD_aux = BD %>% 
  select(Ano,Turno,ph,nh)

BD_3 = BD_3 %>% 
  left_join(BD_aux,by = c("Ano","Turno"))

elementos = expand.grid(unique(BD_t$Ano),unique(BD_t$Turno))


sh = c()
y = data.frame()
for(i in 1:nrow(elementos)){
  
  comb = elementos[i,]
  ano = comb[[1]]
  turno = as.character(comb[[2]])
  
  aux = BD_3 %>% 
    filter(Ano == ano,
           Turno == turno)
  
  mediah = aux$ph[1]
  nh = aux$nh[1]
  
  sh = sum((aux$Satisfeito - mediah)^2)/(nh-1)
  sh = if_else(is.nan(sh)|is.na(sh),0,sh)
  
  
  y = rbind(y,c(ano,turno,sh))
}

names(y) = c("Ano","Turno","sh")
y$Ano = as.numeric(y$Ano)

BD_4 = BD_t %>% 
  left_join(y, by = c("Ano","Turno"))
  
Nh = BD_4$Nh
nh = BD_4$nh
sh = as.numeric(BD_4$sh)

li = total_chapeu - qnorm(0.975,lower.tail = T)*sqrt(sum(Nh*(Nh-nh)*sh/nh))
ls = total_chapeu + qnorm(0.975,lower.tail = T)*sqrt(sum(Nh*(Nh-nh)*sh/nh))

paste0("(",round(li,3),";",round(ls,3),")")

