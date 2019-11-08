# UNIVERSIDADE FEDERAL FLUMINENSE (UFF)
# INSTITUTO DE MATEMÁTICA E ESTATÍSTICA (IME)
# DEPARTAMENTO DE ESTATÍSTICA (GET)
# DADOS DO EXEMPLO DE APLICAÇÃO - 34ª AULA DE MODELOS LINEARES I (AULA PRÁTICA - 2019.2)
# ASSUNTO: HETEROCEDASTICIDADE E MEDIDAS CORRETIVAS - DATA: 08/11/2019
# ENTRADA DE DADOS - NOTAS DE AULA DO PROF. DR. JOSÉ RODRIGO DE MORAES

ntrab_X=c(294,247,267,358,423,311,450,534,438,697,688,630,709,627,615,999,1022,1015,700,850,980,1025,1021,1200,1250,1500,1650)
nsuperv_Y=c(30,32,37,44,47,49,56,62,68,78,80,84,88,97,100,109,114,117,106,128,130,160,97,180,112,210,135)

banco = data.frame(ntrab_X, nsuperv_Y); banco
plot(ntrab_X, nsuperv_Y, pch = 19)
modelo1 = lm(nsuperv_Y~ntrab_X);summary(modelo1)
anova(modelo1)

ei = resid(modelo1); ei
Ychapeu = fitted(modelo1); Ychapeu

ris = rstandard(modelo1); ris

plot(ei ~Ychapeu, pch = 19); abline(h = 0) # Resíduo bruto não está padronizado, ylim muda
plot(ei ~ntrab_X, pch = 19); abline(h = 0)

plot(ris ~Ychapeu, pch = 19, ylim = c(-3,3)); abline(h = c(-2,0,2), lty = c(2,1,2), col = c("red","black","red"))

qqnorm(ris, pch = 19); abline(0,1)
shapiro.test(ris)


### Teste de White de heterocedasticidade
ntrab_X2 = ntrab_X^2
ei2 = ei^2
reg_aux = lm(ei2 ~ ntrab_X + ntrab_X2)
x = summary(reg_aux)
R2 = x$r.squared; round(R2,3)
wobs = dim(banco)[1] * R2 ; wobs
wcritico = qchisq(0.05, 2, lower.tail = F); wcritico

#Como wobs = 21.134 > wcritico = 5.991 reijeita-se H0 ao nível de significância de 5%
#ou seja, existe evidêndias de heterocedasticidade 

pvalor = pchisq(wobs, 2, lower.tail = F);round(pvalor,5)

lmtest::bptest(modelo1, ~ntrab_X + ntrab_X2) # Usamos o comando do teste de Breusch-Pagan
#para realizar o teste de white

#Corrigir a heterocedasticidade
#VAR(Erroi) = f(Xi) = sigma2 * Xi2
#VAR(Erroi*) = VAR(Erroi/Xi) = sigma2

nsuperv_Yt = nsuperv_Y / ntrab_X
ntrab_Xt = 1/ntrab_X

modelo2 = lm(nsuperv_Yt ~ntrab_Xt); summary(modelo2)

Ychapeut = fitted(modelo2)
rist = rstandard(modelo2)
qqnorm(rist, pch = 19); abline(0,1)
shapiro.test(rist)
plot(rist ~ Ychapeut, pch = 19, ylim = c(-3,3)); abline(h = c(-2,0,2) ,
                                                        lty = c(2,1,2),
                                                        col = c("red","black","red"))

ntrab_X2t = ntrab_Xt ^2
lmtest::bptest(modelo2, ~ntrab_Xt + ntrab_X2t)

#Yi_hat/Xi = B0_hat + B1_hat * Xi
#Yi_hat = B1_hat + B0_hat * Xi

