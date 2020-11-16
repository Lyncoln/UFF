#Arvore de decisao

library(caret)
library(rpart)
library(rpart.plot)

epiDisplay::tab1(golf$Play)

playTree<-rpart(Play~.,data=golf)
rpart.plot(playTree)

ctrl=rpart.control(minsplit=0)
playTree<-rpart(Play~.,data=golf,control=ctrl)
rpart.plot(playTree)

ctrl=rpart.control(minsplit=0,cp=0.1)
playTree<-rpart(Play~.,data=golf,control=ctrl)
rpart.plot(playTree)

ctrl=rpart.control(minsplit=0,cp=0,maxdepth=3)
playTree<-rpart(Play~.,data=golf,control=ctrl)
rpart.plot(playTree)
