degree_1 = 2
degree_2 = 50
quantile = qf(0.05, df1 = degree_1, df2 = degree_2, lower.tail = F)
test_stat = 4.9108

rc_values = seq(quantile, 7, length = 54)
denisty_rc_values = df(rc_values,df1 = degree_1, df2 = degree_2)
ic_values = seq(0, quantile, length = 54)
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

lines(
  x = c(quantile, quantile),
  y = c(0, denisty_rc_values[length(denisty_rc_values)]),
  lty = 2
)

lines(x=c(0, 7), y=c(0,0))

text(6, 0.09, expression(alpha), cex=1.7)
text(0.5, 0.15, expression(1 - alpha), cex=1.7)

par(xpd=TRUE)
points(test_stat, 0, pch = 19)
text(test_stat, 0, latex2exp::TeX("$F_{obs}$"), pos = 3)
text(2, -0.33, expression(alpha == 0.05))
legend(4, 1,legend = c('Região Crítica'), box.col = "white",
       fill = c('red'), 
       density = 50)
par(xpd=FALSE)
