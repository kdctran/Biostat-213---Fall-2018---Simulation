#install.packages("FME")

library(FME)
library(coda)

## banana function
Banana <- function (x) {
  y1<-x[1]^2 / 2
  y2<- (-1/2) * (x[2] - 2*(2*y1 - 5))^2
  return(c(y1,y2))
}

## banana matix function
BananaM <- function (X) {
  y1<-x[1]^2 / 2
  y2<- (-1/2) * (x[2] - 2*(2*y1 - 5))^2
  return(cbind(y1,y2))
}


##-----------------------------------------------------------------
## Probability of a multinormally distributed value
##-----------------------------------------------------------------

pmultinorm <- function(value,mean,Cov) {
  diff <- value - mean
  ex   <- -0.5*t(diff) %*% solve(Cov) %*% diff
  rdet   <- sqrt(det(Cov))
  power  <- -length(diff)*0.5
  return((2.*pi)^power / rdet * exp(ex))
}


## 2-*log sum of squares
BananaSS <- function (p) {
  P <- Banana(p)
  Cov <- matrix(nr=2,data=c(1,0.9,0.9,1))
  -2*sum(log(pmultinorm(P,mean=0,Cov=Cov)))
}


## The Markov chain - simple Metropolis

N <- 100000
MCMC1 <- modMCMC(f=BananaSS, p=c(0,0.5), jump=diag(nrow=2,x=0.5),niter=N)
MCMC2 <- modMCMC(f=BananaSS, p=c(0,0.5), jump=diag(nrow=2,x=1),niter=N)
MCMC3 <- modMCMC(f=BananaSS, p=c(0,0.5), jump=diag(nrow=2,x=2),niter=N)
plot(MCMC,mfrow=NULL,main="MH")


plot(MCMC$pars,main="MH")
sampl1 <- mcmc(MCMC1$pars)
sampl2 <- mcmc(MCMC2$pars)
sampl3 <- mcmc(MCMC3$pars)
# summary(sampl1)
# acceptanceRate <- 1 - rejectionRate(sampl1)
# acceptanceRate
# plot(sampl1)

# burn-in discard
burn <- 40000
sampl1.burned <- mcmc(sampl1[burn:N, ])
sampl2.burned <- mcmc(sampl2[burn:N, ])
sampl3.burned <- mcmc(sampl3[burn:N, ])
# plot(sampl1.burned)
# autocorr.plot(sampl1.burned)
geweke.diag(sampl1.burned)
geweke.diag(sampl2.burned)
geweke.diag(sampl3.burned)

raftery.diag(sampl1.burned)
raftery.diag(sampl2.burned)
raftery.diag(sampl3.burned)

heidel.diag(sampl1.burned)
heidel.diag(sampl2.burned)
heidel.diag(sampl3.burned)

plotESSBurn(sampl1)
