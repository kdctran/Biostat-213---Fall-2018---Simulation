#install.packages("adaptMCMC")
library(adaptMCMC)
library(coda)

## log-pdf to sample from
log.pdf <- function(x) {
  -x[1]^2/2 - 1/2*(x[2]-2*x[1]^2+10)^2
}

## generate sample
set.seed(213)
sample1 <- MCMC(log.pdf, n = 10000, init = c(0, 0), scale = c(1, 0.5),
                adapt=FALSE)

##
x1 <- seq(-15, 15, length=100)
x2 <- seq(-15, 15, length=100)
banana.dist <- matrix(apply(expand.grid(x1, x2), 1, log.pdf), nrow=100)

#par(mfrow=c(1,1))
image(x1, x2, exp(banana.dist), col=cm.colors(60), 
      xlim = c(-10, 10), asp=1)
#contour(x1, x2, exp(d.banana), add=TRUE, col=gray(0.6))
points(sample$samples, pch=1)

tab <- sample$samples
par(mfrow=c(3,2))
plot(tab,col=1:10000)
plot(tab,type="l")
plot(ts(tab[,1]))
plot(ts(tab[,2]))
hist(tab[,1],40)
hist(tab[,2],40)
par(mfrow=c(1,1))

sample1.mcmc <- as.mcmc.list(sample1)