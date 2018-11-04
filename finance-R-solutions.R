library(tidyquant)
library(rJava)
library(ggplot2)
library(stats)

getSymbols('^GSPC', src='yahoo', auto.assign = TRUE)
allReturns(GSPC)
sp.returns <- periodReturn(GSPC, period='monthly', type='log')
sp.df <- as.data.frame(sp.returns) %>% rownames_to_column('date')

getSymbols('DNKN', src='yahoo', auto.assign = TRUE)
allReturns(DNKN)
dnkn.returns <- periodReturn(DNKN, period='monthly', type='log')
dnkn.df <- as.data.frame(dnkn.returns) %>% rownames_to_column('date')

getSymbols('MCD', src='yahoo', auto.assign = TRUE)
allReturns(MCD)
mcd.returns <- periodReturn(MCD, period='monthly', type='log')
mcd.df <- as.data.frame(mcd.returns[55:143,]) %>% rownames_to_column('date')

getSymbols('SBUX', src='yahoo', auto.assign = TRUE)
allReturns(SBUX)
sbux.returns <- periodReturn(SBUX, period = 'monthly', type = 'log')
sbux.df <- as.data.frame(sbux.returns[55:143,]) %>% rownames_to_column('date')

head(sbux.df)

er <- c(mean(sbux.df$monthly.returns), mean(dnkn.df$monthly.returns), mean(mcd.df$monthly.returns))

excess.return <- matrix(c((sbux.df$monthly.returns - er[1]),
                          (dnkn.df$monthly.returns - er[2]),
                          (mcd.df$monthly.returns - er[3])), ncol = 3)

varcovmatrix <- matrix(t(excess.return) %*% excess.return, ncol=3) * (1/(88- 1))

varcovmatrix

(mean(sbux.df$monthly.returns) * (1/3) 
  + mean(dnkn.df$monthly.returns) * (1/3) + 
    mean(mcd.df$monthly.returns) * (1/3))


sqrt(var(sbux.df$monthly.returns) * (1/3)^2 
     + var(dnkn.df$monthly.returns) * (1/3)^2 
     + var(mcd.df$monthly.returns) * (1/3)^2
     + 2 * (1/3)^2 * cov(sbux.df$monthly.returns, dnkn.df$monthly.returns)
     + 2 * (1/3)^2 * cov(sbux.df$monthly.returns, mcd.df$monthly.returns)
     + 2 * (1/3)^2 * cov(sbux.df$monthly.returns, mcd.df$monthly.returns))

install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")  

library(IntroCompFinR)

ef <- efficient.frontier(er, varcovmatrix, nport=100, shorts=TRUE)
r.free <- 0.0025


# compute the tangency portfolio
tan.port <- tangency.portfolio(er, varcovmatrix, r.free)


# compute global minimum variance portfolio
gmin.port <- globalMin.portfolio(er, varcovmatrix)

# plot
plot(ef, cex=.25)

points(gmin.port$sd, gmin.port$er, col="orange", pch=16, cex=1.5)
points(tan.port$sd, tan.port$er, col="blue", pch=16, cex=1.5)

text(gmin.port$sd, gmin.port$er, labels="Minimum Variance", pos=4, cex=.8)
text(tan.port$sd, tan.port$er, labels="Tangent Line", pos=4, cex=.8)

sr.tan <- (tan.port$er - r.free)/tan.port$sd

abline(a=r.free, b=sr.tan, col="blue", lwd=1.5)

gmin.port
tan.port

getSymbols('DNKN', src='yahoo', auto.assign = TRUE)
allReturns(DNKN)
dnkn.yrs <- periodReturn(DNKN, period='yearly', type='log')
dnkn.yrs.df <- as.data.frame(dnkn.yrs) %>% rownames_to_column('date')

getSymbols('^GSPC', src='yahoo', auto.assign = TRUE)
allReturns(GSPC)
sp.yrs <- periodReturn(GSPC, period='yearly', type='log')
sp.yrs.df <- as.data.frame(sp.yrs[5:12,]) %>% rownames_to_column('date')

plot(x=sp.yrs.df$yearly.returns, y=dnkn.yrs.df$yearly.returns)

capm.fit <- lm(dnkn.yrs.df$yearly.returns ~ sp.yrs.df$yearly.returns)

summary(capm.fit)

abline(capm.fit)

yr.r.free <- 0.03
mkt.er <- mean(sp.yrs.df$yearly.returns)
beta <- 1.23942

capm.dnkn <- yr.r.free + ((mkt.er - yr.r.free) * beta)
capm.dnkn
