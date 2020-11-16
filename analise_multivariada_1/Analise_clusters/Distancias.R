###DISTÂNCIA PARA O EXEMPLO DOS SLIDES

dado=matrix(c(28,31,42,38,25,41,9.6,8.4,2.4,18.2,3.9,6.4),6,2)
dado


dist_1=dist(dado)
dist_1
summary(dist_1)

dist_2=dist(dado,method="manhattan")
dist_2
summary(dist_2)


dist_3=dist(dado,method="minkowski",p=3)
dist_3
summary(dist_3)

dist_4=dist(dado,method="minkowski",p=7)
dist_4
summary(dist_4)

#DIFERENÇA ENTRE MINKOWSKI
summary(dist_3)-summary(dist_4)

#DADOS CATEGÓRICOS


# dados_cat=matrix(0,50,8)
# for(i in 1:50){
#  dados_cat[i,]=sample(c(0,1),8,replace=T) 
# 
# }
# write.table(dados_cat,"Dados_Categoricos.csv",sep=";",row.names=F)

dados_cat=Dados_Categoricos


dist(dados_cat,method="binary")


