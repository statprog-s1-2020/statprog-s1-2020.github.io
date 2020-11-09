## the Following Cholesky based version of the ridge regression code has
## (at least) 3 bugs.
## 1. Find and correct the bugs, using the Rstudio debugger to help.
##    hint: once working there should be a nicely defined GCV minimum around
##          EDF = 230-240
## 2. Add any comments that might improve code readability.
## 3. Profile the code and figure out how to make it run faster by working out
##    where most of the computational time is being spent, and how it might be
##    reduced. Hint: ?chol2inv

fit.ridge <- function(y,XX,X,lsp) {
## fits model y = X b + e by ridge regression, with log penalty parameter lsp
## and computes GCV score.
  p <- ncol(XX)
  R <- chol(XX+exp(lsp)*diag(p))
  b.hat <- backsolve(t(R),forwardsolve(R,t(X)%*%y))
  RX <- forwardsolve(t(R),t(X))
  trA <- sum(RX^2) ## Effective degrees of freedom
  fv <- X %*% b.hat ## fitted values
  gcv <- sum(n*(y-fv)^2)/(n-trA)^2 
  list(b.hat=b.hat,fv=fv,gcv=gcv,edf=trA)
} ## fit.ridge

plot.gcv <- function(edf,lsp,gcv) {
## plot GCV against EDF and log s.p. showing minimum 
  par(mfrow=c(1,2),mar=c(4,4,1,1)) 
  plot(lsp,gcv,xlab=expression(log(lambda)),ylab="GCV",type="l")
  i.min <-which(gcv==min(gcv)) 
  points(lsp[i.min],gcv[i.min],col=2,pch=19)
  plot(edf,gcv,xlab="EDF",ylab="GCV",type="l")
  points(edf[i.min],gcv[i.min],col=2,pch=19)
}

ridge <- function(y,X,lsp=seq(-5,5,length=100)) {
## function for ridge regression of y on X with generalized
## cross validation lsp is the set of smoothing parameters to try
  edf <- gcv <- lsp*0 ## vectors for edf and gcv
  XX <- crossprod(X)
  for (i in 1:length(lsp)) { ## loop over log smoothing parameters
    fr <- fit.ridge(y,XX,X,lsp[i]) ## fit
    gcv[i] <- fr$gcv
    edf[i] <- fr$edf
  }
  plot.gcv(edf,lsp,gcv) ## plot results
  i.opt <- max(which(gcv==min(gcv))) ## locate minimum
  fit.ridge(y,XX,lsp[i.opt]) ## return fit at minimum
} ## ridge

## Simulate some test data...

set.seed(0)
n <- 300;
m <- 1000
X <- matrix(runif(n*m),m,n) ## model matrix
beta <- runif(n);beta[round(n/5):n] <- 0 ## param vector
mu <- X %*% beta ## mean of response
y <- mu + rnorm(m) ## simulate response

## fit model
fv <- ridge(y,X)


