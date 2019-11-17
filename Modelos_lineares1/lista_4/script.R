
# 1) ----------------------------------------------------------------------

#a) Regressão linear múltipla

#b)
BD1 = readr::read_table2("F:/GitHub/UFF/Modelos_lineares1/lista_4/tabela_1.tsv")
BD1 = BD1[,-1]

pairs(BD1, pch = 19)
cor(BD1)[1,]

#c)

cor(BD1[,-1])
#Manufatura e Pop possuem cor > |0.90|, o que é um indício de multicolineariedade

#d)

modelo1 = lm(data = BD1); summary(modelo1)
#Pela comparação do teste individual e geral de significância, não é possível ter conclusão
#De multicoliniearidade

#e)
BD1_aux = BD1[,-1]

modelo_temp = lm(BD1_aux)
modelo_manuf = lm(dplyr::select(BD1_aux, Manuf, dplyr::everything()))
modelo_pop = lm(dplyr::select(BD1_aux, Pop, dplyr::everything()))
modelo_vento = lm(dplyr::select(BD1_aux, Vento, dplyr::everything()))
modelo_precip = lm(dplyr::select(BD1_aux, Precip, dplyr::everything()))
modelo_dias = lm(dplyr::select(BD1_aux, N_dias, dplyr::everything()))

r2_temp = summary(modelo_temp)$r.squared
r2_manuf = summary(modelo_manuf)$r.squared
r2_pop = summary(modelo_pop)$r.squared
r2_vento = summary(modelo_vento)$r.squared
r2_precip = summary(modelo_precip)$r.squared
r2_dias = summary(modelo_dias)$r.squared

vif = function(x){
  1/(1-x)
}

vif_temp = vif(r2_temp)
vif_manuf = vif(r2_manuf)
vif_pop = vif(r2_pop)
vif_vento = vif(r2_vento)
vif_precip = vif(r2_precip)
vif_dias = vif(r2_dias)

#Manuf e Pop apresentam VIF > 10, o que indica existência de multicolineariedade

#f)

#Será retirada a variável Pop pois ela não é muito informativa para o problema

BD1_f = BD1[,-4]

modelo2 = lm(data = BD1_f); summary(modelo2)
modelo3 = lm(data = BD1_f[,-c(4,5,6)]); summary(modelo3)

#Yi_hat = 77.237 -1.048*Xi1 + 0.024304Xi2 
summary(modelo3)$r.squared
