#PACOTE CLUSTER

library("cluster", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
require(graphics)
require(utils)
library(mclust)

#GERAÇÃO DOS DADOS 

# x1=rpois(100,100)
#  x11=rpois(100,40)
#  x2=rpois(100,40)
#  x3=rpois(100,40)
#  x22=rpois(100,70)
#  x33=rpois(100,100)
# 
# 
# xx=rbind(x2,x3)
# xxx=rbind(x1,x11)
# xxxx=rbind(x22,x33)
# x=cbind(xx,xxx,xxxx)
# x=t(x)
# 


###########################################################################
###########################################################################
########################CLUSTERIZAÇÃO HIERÁRQUICA##########################
###########################################################################
###########################################################################

dados1=Dados_Vendas

# DETERMINAR O NÚMERO ÓTIMO DE CLUSTERS

fit <- Mclust(dados1)
summary(fit) 




#MÉTODO DISTÂNCIA MÍNIMA

#ENCONTRANDO OS CLUSTERS
dend1=hclust(dist(dados1,"minkowski",3), "single")

#DENDOGRAMAS
plot(dend1)

#VISUALIZAÇÃO DOS CLUSTERS NO DENDOGRAMA
rect.hclust(dend1, k = 3, border = "red")
ut=cutree(dend1,k=3)
ut



#########################################################################
#########################################################################
#MÉTODO DISTÂNCIA MÁXIMA

#ENCONTRANDO OS CLUSTERS
dend1=hclust(dist(dados1,"minkowski",3), "complete")

#DENDOGRAMAS
plot(dend1)

#VISUALIZAÇÃO DOS CLUSTERS NO DENDOGRAMA
rect.hclust(dend1, k = 3, border = "red")
ut=cutree(dend1,k=3)
ut


#######################################################################
#######################################################################

#MÉTODO DISTÂNCIA MÉDIA

#ENCONTRANDO OS CLUSTERS
dend1=hclust(dist(dados1,"minkowski",3), "average")

#DENDOGRAMAS
plot(dend1)

#VISUALIZAÇÃO DOS CLUSTERS NO DENDOGRAMA
rect.hclust(dend1, k = 3, border = "red")
ut=cutree(dend1,k=3)
ut

########################################################################
########################################################################

#MÉTODO DE WARD
dend1=hclust(dist(dados1,"minkowski",3), "ward.D")

#DENDOGRAMAS
plot(dend1)

#VISUALIZAÇÃO DOS CLUSTERS NO DENDOGRAMA
rect.hclust(dend1, k = 3, border = "red")
ut=cutree(dend1,k=3)
ut




#CLUSTERIZAÇÃO NÃO-HEIRÁRQUICA

#PACOTE cluster 
pam_dados1=pam(dados1,7)
pam_dados1
plot(pam_dados1)


#KMEANS
km_dados1<-kmeans(dados1,centers=5)$cluster

plot(km_dados1)

#######################################################################
##########################DADOS CATEGÓRICOS############################
#######################################################################

#GERANDO ALGUNS DADOS

dados_cat=matrix(0,50,8)
for(i in 1:50){
  dados_cat[i,]=sample(c(0,1),8,replace=T) 
  }

dados_cat

# DETERMINAR O NÚMERO ÓTIMO DE CLUSTERS

fit <- Mclust(dados_cat)
summary(fit) 





#CLUSTERIZAÇÃO HIERÁRQUICA

dend1=hclust(dist(dados_cat,"binary"), "ward.D")

#DENDOGRAMAS
plot(dend1)

#VISUALIZAÇÃO DOS CLUSTERS NO DENDOGRAMA
rect.hclust(dend1, k = 3, border = "red")
ut=cutree(dend1,k=3)
ut


###########################################################################
##########################################################################
########################CLUSTERIZAÇÃO NÃO-HIERÁRQUICA#####################
##########################################################################
##########################################################################


########################DADOS QUANTITATIVOS#########################
#PACOTE cluster 
pam_dados1=pam(dados1,7)
pam_dados1
pam_dados1$clustering
plot(pam_dados1)


#KMEANS
km_dados1<-kmeans(dados1,centers=5)
plot(kmeans(dados1,centers=5))
plot(km_dados1)

########################DADOS QUALITATIVOS#########################

#PACOTE cluster 
pam_dados_cat=pam(dados_cat,3)
pam_dados_cat$clustering
plot(pam_dados_cat)


#KMEANS
km_dados_cat<-kmeans(dados_cat,centers=3)
km_dados_cat$cluster
plot(km_dados_cat)
