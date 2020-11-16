#A Escala de Maturidade Mental Colúmbia
#Escala fatorial de socialização EFS
#Hipótese psíquico social HPC
#Escala de ansiedade de beck BAI
#Escala de depressão de beck BDI
#Escala beck Ideação Suicida BSI 
#Escala de Autenticidade,Agressividade e Inibição – EdAAI

library(ggplot2)
library(dplyr)
BD = read.delim("F://GitHub//UFF//Analise_Multivariada_1//Trabalho_1//Lyncoln.csv", sep = " ")
BD = BD %>% 
  select(-Detento) %>% 
  arrange(Presidio) %>% 
  select(Presidio,everything())

# 1) ----------------------------------------------------------------------

#Media e variancia de detentos
descritivos = BD %>% 
  group_by(Presidio) %>% 
  summarize(#Media_Detento = mean(Detento),
            #Variancia_Detento = var(Detento),
            Media_Columbia = mean(Columbia),
            Variancia_Columbia = var(Columbia),
            Media_EFS = mean(EFS),
            Variancia_EFS = var(EFS),
            Media_HPC = mean(HPC),
            Variancia_HPC = var(HPC),
            Media_BAI = mean(BAI),
            Variancia_BAI = var(BAI),
            Media_BDI = mean(BDI),
            Variancia_BDI = var(BDI),
            Media_BSI = mean(BSI),
            Variancia_BSI = var(BSI),
            Media_EdAAI = mean(EdAAI),
            Variancia_EdAAI = var(EdAAI)
            )

#Boxplots
box_columbia = 
  ggplot(BD, aes(x = Presidio, y = Columbia, fill = Presidio)) +
  geom_boxplot(show.legend = FALSE)+
  coord_flip() + 
  ggtitle("Variação de número de columbia por presídios") +
  theme_bw()
box_EFS = 
  ggplot(BD, aes(x = Presidio, y = EFS, fill = Presidio)) +
  geom_boxplot(show.legend = FALSE)+
  coord_flip() + 
  ggtitle("Variação de número de EFS por presídios") +
  theme_bw()
box_HPC = 
  ggplot(BD, aes(x = Presidio, y = HPC, fill = Presidio)) +
  geom_boxplot(show.legend = FALSE)+
  coord_flip() + 
  ggtitle("Variação de número de HPC por presídios") +
  theme_bw()
box_BAI = 
  ggplot(BD, aes(x = Presidio, y = BAI, fill = Presidio)) +
  geom_boxplot(show.legend = FALSE)+
  coord_flip() + 
  ggtitle("Variação de número de BAI por presídios") +
  theme_bw()
box_BDI = 
  ggplot(BD, aes(x = Presidio, y = BDI, fill = Presidio)) +
  geom_boxplot(show.legend = FALSE)+
  coord_flip() + 
  ggtitle("Variação de número de BDI por presídios") +
  theme_bw()
box_BSI = 
  ggplot(BD, aes(x = Presidio, y = BSI, fill = Presidio)) +
  geom_boxplot(show.legend = FALSE)+
  coord_flip() + 
  ggtitle("Variação de número de BSI por presídios") +
  theme_bw()
box_EdAAI = 
  ggplot(BD, aes(x = Presidio, y = EdAAI, fill = Presidio)) +
  geom_boxplot(show.legend = FALSE)+
  coord_flip() + 
  ggtitle("Variação de número de EdAAI por presídios") +
  theme_bw()

boxs = gridExtra::grid.arrange(box_columbia,
                        box_EFS,
                        box_HPC,
                        box_BAI,
                        box_BDI,
                        box_BSI,
                        box_EdAAI,ncol = 2)



cor_bangu = ggcorrplot::ggcorrplot(cor(filter(BD,Presidio == "Bangu")[,-1]),
                                   lab = TRUE,type = "lower") + 
                                   labs(title = "Correlação Bangu")
cor_central = ggcorrplot::ggcorrplot(cor(filter(BD,Presidio == "Central")[,-1]),
                                   lab = TRUE,type = "lower") + 
                                   labs(title = "Correlação Central")
cor_mossoro = ggcorrplot::ggcorrplot(cor(filter(BD,Presidio == "Mossoro")[,-1]),
                                   lab = TRUE,type = "lower") + 
                                   labs(title = "Correlação Mossoro")
cor_papuda = ggcorrplot::ggcorrplot(cor(filter(BD,Presidio == "Papuda")[,-1]),
                                   lab = TRUE,type = "lower") + 
                                   labs(title = "Correlação Papuda")
cor_pedrinhas = ggcorrplot::ggcorrplot(cor(filter(BD,Presidio == "Pedrinhas")[,-1]),
                                   lab = TRUE,type = "lower") + 
                                   labs(title = "Correlação Pedrinhas")
cor_pereirao = ggcorrplot::ggcorrplot(cor(filter(BD,Presidio == "Pereirao")[,-1]),
                                   lab = TRUE,type = "lower") + 
                                   labs(title = "Correlação Pereirao")
                       

cors = gridExtra::grid.arrange(cor_bangu,
                        cor_central,
                        cor_mossoro,
                        cor_papuda,
                        cor_pedrinhas,
                        cor_pereirao,ncol = 2)


  
#Frequencia de detentos

freq = ggplot(BD,aes(x = Presidio,fill = Presidio))+
  geom_bar(show.legend = FALSE)+
  ylab("Frequência")+
  ggtitle("Quantidade de detentos por presídios")+
  theme_bw()


# 2) ----------------------------------------------------------------------

presidios = BD %>% 
  select(Presidio) %>% 
  distinct() %>% 
  pull() %>% 
  as.character()

p_valor = c()
for(i in presidios){
  p_valor = c(p_valor,mvShapiroTest::mvShapiro.Test(as.matrix(select(filter(BD,Presidio == i),-1)))$p.value)
}
normalidade = tibble(presidios,
                     p_valor) %>% 
  mutate(normal = if_else(p_valor > 0.05,"Sim","Não"))


#independencia

ind = function(data){
  R = cor(data); p <- ncol(data)
  n = nrow(data)
  est = -((n-1)-(2*p+5)/6)*log(det(R))
  return(est)
}

# H0: Var são ind x H1 Var são depend


#Estatística
ind(BD[,-1]) 

#Região de aceitação

qchisq(0.05, 7*8/2)

#Obtemos uma estatística de teste maior que a qchisq ,logo rejeitamos H0, ou seja
#Var são dependente


#Pereirão deu não normal, mas iremos considerar normalidade para realização dos outros testes,
#Mesmo sabendo que não é apropriado
# 3) ----------------------------------------------------------------------

#Primeiro temos que testar variâncias 2 a 2

testa_var = function(base){
  p_valor = c()
  primeiro = c()
  segundo = c()
  combinacoes = combn(as.vector(unique(base[,1])),2)
  for(i in 1:length(combinacoes[1,])){
    x1 = combinacoes[,i][1]
    x2 = combinacoes[,i][2]
    bd_aux = BD %>% filter(Presidio==x1 | Presidio ==x2)#
    bd_aux = arrange(bd_aux, Presidio)#
    teste = MVTests::BoxM(as.vector(bd_aux[,-1]),as.vector(bd_aux[,1]))
    p_valor = c(p_valor, teste$p.value)
    primeiro = c(primeiro, x1)
    segundo = c(segundo, x2)
  }
  resultado = tibble(Presidio1 = primeiro,
                    Presidio2 = segundo,
                    p_valor = p_valor)
  return(resultado)
}

teste_var_2x2 = testa_var(BD) %>% 
  mutate_if(is.numeric, function(x) ifelse(x<0.001,"<0.001", round(x,3)))

#Agora iremos testar as médias
quantidade = BD %>% filter(Presidio %in% c("Bangu","Central")) %>% group_by(Presidio) %>% summarize(n())


testa_med = function(base){
  p_valor = c() #P_valor do teste de variancia
  p_valor2 = c() #P_valor do teste de média
  variancias = c()
  medias = c()
  primeiro = c()
  segundo = c()
  combinacoes = combn(as.vector(unique(base[,1])),2)
  for(i in 1:length(combinacoes[1,])){
    x1 = combinacoes[,i][1]
    x2 = combinacoes[,i][2]
    bd_aux = BD %>% filter(Presidio==x1 | Presidio ==x2)#
    bd_aux = arrange(bd_aux, Presidio)#
    teste = MVTests::BoxM(as.vector(bd_aux[,-1]),as.vector(bd_aux[,1]))
    p_valor = c(p_valor, teste$p.value)
    primeiro = c(primeiro, x1)
    segundo = c(segundo, x2)
    
    
    #Agora irei testar média
    
    contagem = base %>%
      filter(Presidio %in% c(x1,x2)) %>%
      group_by(Presidio) %>% 
      summarize(n())
    cont_x1 = contagem[1,2]
    cont_x2 = contagem[2,2]
    
    if(teste$p.value > 0.05) {
      teste2 = MVTests::TwoSamplesHT2(bd_aux[,-1],c(rep(1,cont_x1),rep(2,cont_x2)),Homogenity = T)$p.value
      p_valor2 = c(p_valor2,
                   teste2)
                   
      variancias = c(variancias,"Igual")
    }
    else{
      teste2 = MVTests::TwoSamplesHT2(bd_aux[,-1],c(rep(1,cont_x1),rep(2,cont_x2)),Homogenity = F)$p.value
      p_valor2 = c(p_valor2, 
                   teste2)
      variancias = c(variancias,"Diferente")
      
    }
    if(teste2 > 0.05) medias = c(medias, "Igual")
    else medias = c(medias,"Diferente")
  }
  resultado = tibble(Presidio1 = primeiro,
                     Presidio2 = segundo,
                     p_valor_var = p_valor,
                     Variancias = variancias,
                     p_valor_medias = p_valor2,
                     Medias = medias)
  return(resultado)
}

#Nessa função eu estou testando primeiramente a igualdade 2x2 das variâncias,
#Se forem iguais, o argumento da função do pacote mvtest será homogenity=True,
#Se forem diferentes, o argumento da função será homogenity = False
#O teste irá utilizar uma estatística de acordo com o resultado da igualdade das variâncias

testa_med_2x2 = testa_med(BD)
testa_med_2x2 = testa_med_2x2 %>% 
  mutate_if(is.numeric, function(x) ifelse(x<0.001,"<0.001", round(x,3)))

#Testando função do pacote MVTests para variâncias iguais

BD_ban_cen = BD %>% 
  filter(Presidio %in% c("Bangu","Central")) %>% 
  arrange(Presidio)
qtd_ban_cen = BD_ban_cen %>%
  group_by(Presidio) %>% 
  summarize(n())

#Função do pacote MVTests para variâncias iguais

MVTests::TwoSamplesHT2(BD_ban_cen[,-1],
                       c(rep(1,qtd_ban_cen[1,2]),rep(2,qtd_ban_cen[2,2])))


#Função feita na mão para variâncias iguais

difmedia_VarIgual <- function(data, X1, X2, delta = 0){
  X1 <- filter(data, data$Presidio == X1)[, -1]
  X2 <- filter(data, data$Presidio == X2)[, -1]
  n1 <- nrow(X1); n2 <- nrow(X2); n <- n1 + n2; p <- ncol(X1)
  X1media <- colMeans(X1); S1 <- var(X1)
  X2media <- colMeans(X2); S2 <- var(X2)
  Sp <- ((n1-1)*S1 + (n2-1)*S2)/(n -2)
  Xd <- X1media - X2media
  Spast <- (1/n1 + 1/n2)*Sp
  T2 <- t(Xd - delta)%*%solve(Spast)%*%(Xd - delta)
  saida <- list(T2 = T2)
  return(saida)
}

difmedia_VarIgual(BD,"Bangu","Central")


#Testando função do pacote MVTests para variâncias diferentes


BD_ban_mos = BD %>% 
  filter(Presidio %in% c("Bangu","Mossoro")) %>% 
  arrange(Presidio)
qtd_ban_mos = BD_ban_mos %>%
  group_by(Presidio) %>% 
  summarize(n())


MVTests::TwoSamplesHT2(BD_ban_mos[,-1],
                       c(rep(1,qtd_ban_mos[1,2]),rep(2,qtd_ban_mos[2,2])),
                       Homogenity = FALSE)

difmedia_VarDif <- function(data, X1, X2, delta = 0){
  X1 <- filter(data, data$Presidio == X1)[, -1]
  X2 <- filter(data, data$Presidio == X2)[, -1]
  n1 <- nrow(X1); n2 <- nrow(X2); n <- n1 + n2; p <- ncol(X1)
  X1media <- colMeans(X1); S1 <- var(X1)
  X2media <- colMeans(X2); S2 <- var(X2)
  Xd <- X1media - X2media
  Spast <- S1/n1 + S2/n2
  T2 <- t(Xd - delta)%*%solve(Spast)%*%(Xd - delta)
  saida <- list(T2 = T2)
  return(saida)
}

difmedia_VarDif(BD,"Bangu","Mossoro")


# 4) ----------------------------------------------------------------------

#Mossoro x Geral

BD_mos = BD %>% 
  filter(Presidio=="Mossoro")
BD_geral = BD %>% 
  filter(Presidio!="Mossoro") %>% 
  mutate(Presidio = "Geral")

MVTests::BoxM(as.matrix(rbind(BD_mos,BD_geral)[,-1]),
              as.matrix(rbind(BD_mos,BD_geral)[,1]))


# 5) ----------------------------------------------------------------------

BD_papuda = BD %>% 
  filter(Presidio == "Papuda")
BD_papuda = BD_papuda[1:length(BD_mos[,1]),]
BD_mos

par <- MVTests::Mpaired(T1 = BD_papuda[,-1], T2 = BD_mos[,-1])
summary(par)

#Na mão

dif <- function(data, X1, X2, alfa = 0.5, delta = 0){
  D1 <- filter(data, data$Presidio == X1)[, -1]
  D2 <- filter(data, data$Presidio == X2)[, -1]
  D <- D1 - D2; p <- ncol(D)
  Dmedia <- colMeans(D)
  Sd <- var(D)
  n <- nrow(D)
  T2 <- n%*%t(Dmedia - delta)%*%solve(Sd)%*%(Dmedia - delta)
  saida <- list(T2 = T2, Sd = Sd )
  return(saida)
}

dif(rbind(BD_papuda,BD_mos),"Mossoro","Papuda")


# 6) ----------------------------------------------------------------------
BD_mpp = BD %>% 
  filter(Presidio %in% c("Mossoro","Papuda","Pereirao"))

MVTests::BoxM(as.matrix(BD_mpp[,-1]),
              as.matrix(BD_mpp[,1]))

MVTests::Manova(as.matrix(BD_mpp[,-1]),
                as.matrix(BD_mpp[,1]))
