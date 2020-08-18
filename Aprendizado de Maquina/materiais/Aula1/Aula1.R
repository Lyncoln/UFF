install.packages("kernlab")
library(kernlab)
library(dplyr)
data(spam)

par(mfrow=c(1,2))
hist(spam$your[spam$type=="nonspam"],prob=T,col="yellow",ylim=c(0,1),xlim=c(0,12),xlab="Frequencia (em %)", main="Não SPAM")
lines(density(spam$your[spam$type=="nonspam"]),col="blue",lwd=2)
hist(spam$your[spam$type=="spam"],prob=T,col="gray",ylim=c(0,1),xlab="Frequencia (em %)", main="SPAM")
lines(density(spam$your[spam$type=="spam"]),col="red",lwd=2)
par(mfrow=c(1,1))

plot(density(spam$your[spam$type=="nonspam"]),col="blue",lwd=2,xlab="Frequencia (em %)",main="Densidades da palavra your")
lines(density(spam$your[spam$type=="spam"]),col="red",lwd=2)
abline(v=0.5)

predition<-ifelse(spam$your>0.5,"spam","nonspam")

table(predition,spam$type)

table(predition,spam$type)/length(spam$type)

#taxa de acerto: 
0.4590306+0.2923278

