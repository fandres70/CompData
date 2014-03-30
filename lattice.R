package ? lattice
data(environmental)
xyplot(ozone ~ radiation, data=environmental, main="Ozone vs. radiation")
xyplot(ozone ~ temperature, data=environmental)


tempcut = equal.count(environmental$temperature, 4)
xyplot(ozone ~ radiation | tempcut, data=environmental, main="Ozone vs. radiation")
xyplot(ozone ~ radiation | tempcut, data=environmental, main="Ozone vs. radiation", layout=c(1, 4))
xyplot(ozone ~ radiation | tempcut, data=environmental, main="Ozone vs. radiation", 
       layout=c(1, 4), as.table=TRUE)

xyplot(ozone ~ radiation | tempcut, data=environmental, main="Ozone vs. radiation", 
       layout=c(1, 4), as.table=TRUE,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })

xyplot(ozone ~ radiation | tempcut, data=environmental, main="Ozone vs. radiation", 
       as.table=TRUE, pch = 20, xlab="Solar radiation", ylab="Ozone (ppb)",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y)
       })

windcut <- equal.count(environmental$wind, 4)
xyplot(ozone ~ radiation | tempcut * windcut, data=environmental, main="Ozone vs. radiation", 
       as.table=TRUE, pch = 20, xlab="Solar radiation", ylab="Ozone (ppb)",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y)
       })

histogram(~temperature | windcut, data=environmental, layout=c(1, 4), as.table=TRUE)
histogram(~ozone | windcut, data=environmental, layout=c(1, 4), as.table=TRUE)

