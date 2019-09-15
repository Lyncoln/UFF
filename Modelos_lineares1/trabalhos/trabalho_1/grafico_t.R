degree = 98
quantile = qt(0.975, df = degree)
b0_test_stat = -0.195
b1_test_stat = 5.993
rc_values = seq(-4, -quantile, length = 100)
denisty_rc_values = dt(rc_values, df = 46)
ic_values = seq(-quantile, quantile, length = 100)
denisty_ic_values = dt(ic_values, df = 46)

plot(
  function(x)
    dt(x, df = 46),
  xlim = c(-4, 7),
  ylab = '',
  xlab = 'Quantis',
  bty="n",
  yaxt='n',
  xaxt='n'
)
axis(side=1, at=round(c(-4, -quantile, 0 ,quantile, 7), 2))

polygon(
  x = c(-4, rc_values, -quantile),
  y = c(0, denisty_rc_values, 0),
  border = FALSE,
  col = 'red',
  density = 50
)
polygon(
  x = c(quantile, sort(-1 * rc_values), 7),
  y = c(0, sort(denisty_rc_values, decreasing = TRUE), 0),
  border = FALSE,
  col = 'red',
  density = 50
)


lines(
  x = c(-quantile, -quantile),
  y = c(0, denisty_rc_values[length(denisty_rc_values)]),
  lty = 2
)
lines(
  x = c(quantile, quantile),
  y = c(0, denisty_rc_values[length(denisty_rc_values)]),
  lty = 2
)

lines(x=c(-4, 7), y=c(0,0))



text(3.2, 0.23, expression(t[obs]), cex=1.3)
points(b1_test_stat, 0, pch=16)
arrows(3.2, 0.2,b1_test_stat,0)

text(-3, 0.07, expression(frac(alpha, 2)), cex=1.3)
text(3, 0.07, expression(frac(alpha,2)), cex=1.3)
text(0, 0.15, expression(1 - alpha), cex=1.3)

par(xpd=TRUE)
text(4, -0.1, expression(alpha == 0.05))
legend(3.7,0.35,legend = c('Região Crítica'), box.col = "white",
       fill = c('red'), 
       density = 50)
par(xpd=FALSE)
