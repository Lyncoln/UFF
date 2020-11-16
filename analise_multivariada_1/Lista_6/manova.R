BD1 = BD %>% filter(Regiao %in% c("Litoral","Serra","Planalto")) %>% arrange(Regiao)
BD1
MVTests::BoxM(as.matrix(BD1[,-1]),as.matrix(BD1[,1]))
MVTests::TwoSamplesHT2(BD1[,-1],c(rep(1,40),rep(2,40))) %>% summary()
MVTests::Manova(as.matrix(BD1[,-1]),as.matrix(BD1[,1])) %>% summary()

