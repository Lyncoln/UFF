# PreProcessamento ---------------------------------------------------------------
library(tidyverse);library(caret)
load("chd.rda")

dummie = dummyVars(TenYearCHD ~ education + sex,
                   data = chd,
                   fullRank = T)

pred_dummie = predict(dummie,
                      newdata = chd)

chd_dummie = cbind(chd,pred_dummie)

chd_dummie = select(chd_dummie, -c(education,sex))

chd_dummie = chd_dummie %>% 
  mutate(education.2 = as.factor(education.2)) %>% 
  mutate(education.3 = as.factor(education.3)) %>% 
  mutate(education.4 = as.factor(education.4)) %>% 
  mutate(sexM = as.factor(sexM)) 

set.seed(100)

input = preProcess(chd_dummie,
                   method = "knnImpute",
                   k = 5)

data_inputada = predict(input, chd_dummie)
data_inputada = na.omit(data_inputada)
data = data_inputada
