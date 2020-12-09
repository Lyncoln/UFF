require(dplyr)

regiao_sudeste = c("Rio de Janeiro", "São Paulo", "Minas Gerais", "Espírito Santo")



suicidio_dados <- readr::read_csv2('Dados/suicidios.csv')
municipio_dados <- readxl::read_xlsx('Dados/Municipios_2010.xlsx') %>% 
  mutate(regiao = ifelse(Nome_UF %in% regiao_sudeste, "Sudeste", "Outros")) %>% 
  filter(regiao == "Sudeste")

microrregiao_sudeste <- municipio_dados %>% 
  (function(x) x$Microrregiao) %>% unique

# which(microrregiao_sudeste == 35061)
microrregiao_sudeste[1] <- 35061
microrregiao_sudeste[153] <- 31019


estado_micro <- distinct(municipio_dados, Microrregiao, .keep_all = TRUE)

dados <- suicidio_dados %>% filter(Microrregiao %in% microrregiao_sudeste) %>% 
  mutate(
    Microrregiao = factor(Microrregiao,levels = microrregiao_sudeste, labels = microrregiao_sudeste),
    suicidio_paf = factor(suicidio_paf, levels = c(0, 1), labels = c("Não", "Sim")),
    id_legal = factor(id_legal, levels = c(0, 1), labels = c("Não", "Sim")),
    trab_armado = factor(trab_armado, levels = c(0, 1), labels = c("Não", "Sim")),
    sexo = factor(sexo, levels = c(1, 2), labels = c("Masculino", "Feminino")),
    raca = factor(raca, levels = c(1, 2, 3, 4, 5, 6), 
                  labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorado")),
    estado_civil = factor(estado_civil, levels = c(1, 2, 3, 4, 5, 6), 
                          labels = c("Solteiro", "Casado", "Viúvo", "Separado", "Divorciado", "Ignorado")),
    escolaridade = factor(escolaridade, levels = c(1, 2, 3, 4, 5, 6), 
                          labels = c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "5 a 12 anos", "Ignorado"))
  ) 

levels(dados$Microrregiao)

dados %>% summary()

require(rstanarm)

# Definindo priori
my_prior <- normal(location = 0, scale = 10)

# Modelo completo
model <- stan_glm(suicidio_paf ~ ., family = binomial(link = "logit"), 
                  data = dados, prior = my_prior, 
                  prior_intercept = my_prior, chain = 2, 
                  iter = 12000, warmup = 2000, thin = 4)
save(model, file = 'modelo_completo_microreg.RData')

model$stanfit
prior_summary(model)
plot(model, prob = 0.9)

posterior_interval(model, pars = "idade", prob = 0.95)
plot(model, "areas", pars = "idade", prob = 0.95)

posterior_interval(model, regex_pars = "raca", prob = 0.95)

plot(model, plotfun = "combo", regex_pars = "raca")
plot(model, regex_pars = "raca", prob = 0.95)
plot(model, regex_pars = "idade", prob = 0.95)
plot(model, regex_pars = "estado_civil", prob = 0.95)

plot(model, plotfun = "combo") 
posterior_vs_prior(model)












