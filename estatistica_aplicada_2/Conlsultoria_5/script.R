require(dplyr)

regiao_sudeste = c("Rio de Janeiro", "São Paulo", "Minas Gerais", "Espírito Santo")

suicidio_dados = readr::read_csv2('./Dados/suicidios.csv')
municipio_dados = readxl::read_xlsx('./Dados/Municipios_2010.xlsx') %>% 
  mutate(regiao = ifelse(Nome_UF %in% regiao_sudeste, "Sudeste", "Outros")) %>% 
  filter(regiao == "Sudeste")

microrregiao_sudeste = municipio_dados %>% 
  (function(x) x$Microrregiao) %>%
  unique

dados = suicidio_dados %>% 
  filter(Microrregiao %in% microrregiao_sudeste) %>% 
  mutate(
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
  ) %>% select(-Microrregiao)

dados %>% summary()

require(rstanarm)

# Rodei somente com essa variaveis pois esta muito lento
my_prior <- normal(location = rep(0, 19), scale = rep(100, 19))
my_prior <- normal(location = c(-10, 0), scale = c(5, 2))

model <- stan_glm(suicidio_paf ~ ., family = binomial(), data = dados)

summary(model)
prior_summary(model)

posterior_interval(model, pars = "idade", prob = 0.95)
plot(model, "areas", pars = "idade", prob = 0.95)

install.packages("CARBayes")

# require(CARBayes)
# require(dplyr)


dados <-dados %>%
  group_by(idade, id_legal, trab_armado, sexo, raca, estado_civil, escolaridade) %>%
  summarise(N = n(), SPAF = sum(suicidio_paf))


#dados$suicidio_paf = ifelse(dados$suicidio_paf == "Sim",1,0)

model2 = 
  S.glm(SPAF ~ .,
      family = 'binomial', data = dados, 
      trials = dados$N, 
      n.sample = 500000, burnin = 200000,
      thin = 150, MALA = T)

save(model2, file = "modelo.Rdata")
