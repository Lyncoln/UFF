bd = foreign::read.spss("internacoes.sav")
bd = as.data.frame(bd)

degree_1 = 1
degree_2 = 98
quantile = qf(0.975, df1 = degree_1, df2 = degree_2)
test_stat = 35.922

rc_values = seq(quantile, 7, length = 100)
denisty_rc_values = df(rc_values,df1 = degree_1, df2 = degree_2)
ic_values = seq(0, quantile, length = 100)
denisty_ic_values = df(ic_values, df1 = degree_1, df2 = degree_2)

plot(
  function(x)
    df(x, 
       df1 = degree_1,
       df2 = degree_2),
  xlim = c(0, 7),
  ylab = '',
  xlab = 'Quantis',
  bty="n",
  yaxt='n',
  xaxt='n'
)
axis(side=1, at=round(c(0,quantile, 7), 2))

polygon(
  x = c(quantile, rc_values, 7),
  y = c(0, denisty_rc_values, 0),
  border = FALSE,
  col = 'red',
  density = 50
)

polygon(
  x = c(0, ic_values[-1] , quantile),
  y = c(0, denisty_ic_values[-1], 0),
  border = FALSE,
  col = '#94eaf7',
  density = 50
)

lines(
  x = c(quantile, quantile),
  y = c(0, denisty_rc_values[length(denisty_rc_values)]),
  lty = 2
)

lines(x=c(0, 7), y=c(0,0))

text(6, 0.09, expression(alpha), cex=1.7)
text(0.5, 0.15, expression(1 - alpha), cex=1.7)

par(xpd=TRUE)
text(2, -0.33, expression(alpha == 0.05))
legend(4, 1,legend = c('IC', 'RC'), 
       fill = c('#94eaf7', 'red'), 
       density = 50)
par(xpd=FALSE)
