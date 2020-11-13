## Exercises. These are all based on the nhtemp example in lecture 15, the
## code for which is provided below.
## 
## 1. Use optim to fit the model by maximum likelihood estimation.
## 2. Compute confidence intervals for the model parameters using
##    large sample MLE results. Hint: use the inverse of the hessian of the
##    negative log likelihood.
## 3. Use bootstrapping to obtain the confidence intervals (this will involve,
##    recomputing the MLEs for each bootstrap resample.)
## 4. Try out the MH loop from different starting values, to see the case
##    in which the chains take some time to converge.
## 5. Modify the code to only store every Mth simulated vector from the chain,
##    where M is an integer that can be set (try it out with M=10, and a larger
##    total number of chain steps, for example)
## 6. Modify the code so that the posterior means are computed on the parameters
##    after transforming back to the mu, sigma, nu scale (rather than taking
##    means and then transforming).

hist(nhtemp)

ll <- function(theta,temp) { 
  mu <- theta[1];sig <- exp(theta[2]); df = 1 + exp(theta[3])
  sum(dt((temp-mu)/sig,df=df,log=TRUE) - log(sig)) 
}


ns <- 10000
th <- matrix(0,3,ns)
th[,1] <- c(mean(nhtemp),log(sd(nhtemp)),log(6))
llth <- ll(th[,1],nhtemp)
lprior.th <- dnorm(th[3,1],mean=3,sd=2,log=TRUE)
p.sd <- c(.5,.1,1.2)
accept <- 0
for (i in 2:ns) {
  thp <- th[,i-1] + rnorm(3)*p.sd
  lprior.p <- dnorm(thp[3],mean=3,sd=2,log=TRUE) 
  llp <- ll(thp,nhtemp)
  if (runif(1) < exp(llp + lprior.p - llth - lprior.th)) {
    th[,i] <- thp;llth <- llp;lprior.th <- lprior.p
    accept <- accept  + 1
  } else {
    th[,i] <- th[,i-1]
  }
}
accept/ns


par(mfrow=c(3,1),mar=c(4,4,1,1))
plot(th[1,],type="l")
plot(th[2,],type="l")
plot(th[3,],type="l")


par(mfrow=c(2,3))
acf(th[1,]);acf(th[2,]);acf(th[3,]);
hist(th[1,]);hist(exp(th[2,]));hist(th[3,]);

pm <- rowMeans(th) ## posterior mean
## transform to original scale...
pm[2:3] <- exp(pm[2:3])
pm[3] <- pm[3] + 1
names(pm) <- c("mu","sig","df")
pm

## 95% Credible Intervals...
ci <- apply(th,1,quantile,prob=c(.025,.975))
ci[,2:3] <- exp(ci[,2:3]) ;ci[,3] <- ci[,3]+1
colnames(ci) <- c("mu","sig","df")
ci

