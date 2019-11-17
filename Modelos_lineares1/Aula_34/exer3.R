BD = readr::read_delim("dados", ";", escape_double = FALSE, 
                    trim_ws = TRUE)
BD = BD[,-1]
#Exer prático do slide da aula 34

#a)

modelo1 = lm(data = BD); summary(modelo1)
ris = rstandard(modelo1)
y_chapeu = fitted(modelo1)

plot(ris ~ BD$`Tarifa_(X1)`, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2),
                                                              lty = c(2,1,2),
                                                              col = c("red","black","red"))

plot(ris ~ BD$`Renda_(X2)`, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2),
                                                              lty = c(2,1,2),
                                                              col = c("red","black","red"))

plot(ris~y_chapeu, pch = 19, ylim = c(-3,3));abline(h=c(-2,0,2),
                                                    lty = c(2,1,2),
                                                    col = c("red","black","red"))

#Teste de white

tarifa_X = BD$`Tarifa_(X1)`
tarifa_X2 = tarifa_X^2
tarifa_renda_X = BD$`Tarifa_(X1)`*BD$`Renda_(X2)`
renda_X = BD$`Renda_(X2)`
renda_X2 = renda_X ^ 2

ei2 = resid(modelo1)^2

reg_aux = lm(ei2 ~ tarifa_X + tarifa_X2 + tarifa_renda_X + renda_X + renda_X2); summary(reg_aux)

wobs = dim(BD)[1] * summary(reg_aux)$r.squared ; wobs
wcritico = qchisq(0.05, 5, lower.tail = F); wcritico
pvalor = pchisq(wobs, 5, lower.tail = F); pvalor
#lmtest::bptest(modelo1, ~tarifa_X + tarifa_X2 + tarifa_renda_X + renda_X + renda_X2) # White
#lmtest::bptest(modelo1) pelo bptest o modelo é homocedastico porque ele não leva em conta osw
#produtos cruzados. O teste de white é mais rigoroso

# Rejeita-se H0 ao nivel de significancia de 5%, ou seja, 
#existe evidências de heterocidaticidade

######Corrigir a heterocedasticidade
#VAR(Erroi) = f(Xi) = sigma2 * Xi2
#VAR(Erroi*) = VAR(Erroi/Xi) = sigma2





