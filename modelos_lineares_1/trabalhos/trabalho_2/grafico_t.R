degree = 49
quantile = qt(0.975, df = degree)
b0_test_stat = 22.092
b1_test_stat = -0.886
b2_test_stat = 2.012
b3_test_stat = -2.891
b4_test_stat = -2.619

rc_values = seq(-4, -quantile, length = 54)
denisty_rc_values = dt(rc_values, df = 49)
ic_values = seq(-quantile, quantile, length = 54)
denisty_ic_values = dt(ic_values, df = 49)

plot(
  function(x)
    dt(x, df = 46),
  xlim = c(-4, 4),
  ylab = '',
  xlab = 'Quantis',
  bty="n",
  yaxt='n',
  xaxt='n'
)
axis(side=1, at=round(c(-4, -quantile, 0 ,quantile, 4), 2))

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

points(b1_test_stat, 0, pch=5, cex = 1.2, col = "darkblue")

points(b2_test_stat, 0, pch=2, cex = 1.2, col = "darkblue")

points(b3_test_stat, 0, pch=8, cex = 1.2, col = "darkblue")

points(b4_test_stat, 0, pch=4, cex = 1.2, col = "darkblue")

text(-3, 0.1, expression(frac(alpha, 2)), cex=1.3)
text(3, 0.1, expression(frac(alpha,2)), cex=1.3)
text(0, 0.15, expression(1 - alpha), cex=1.3)

par(xpd=TRUE)
text(-3.5, 0.3, expression(alpha == 0.05))


legend(2, 0.4,legend = c(latex2exp::TeX("$T_{obs; \\hat{\\beta}_1}$"),
                         latex2exp::TeX("$T_{obs; \\hat{\\beta}_2}$"),
                         latex2exp::TeX("$T_{obs; \\hat{\\beta}_3}$"),
                         latex2exp::TeX("$T_{obs; \\hat{\\beta}_4}}$")), 
       pch = c(5,2,8,4),
       col = 'darkblue',
       bty = 'n',
       cex = 0.85)

legend(1.93, 0.4597,legend = 'Região Crítica', 
       fill = 'red', 
       density = 50,
       bty = 'n',
       cex = 0.85)
par(xpd=FALSE)

