setwd("D:/New Order/Fourth Semester/ECON 613/R-workdir")
rm(list=ls()) #remove all variable

library("margins")
set.seed(3)

# Exercise 5

ndraws = 10000

x1 = runif(ndraws, min = 1,max = 3)

x2 = rgamma(ndraws, shape = 3, scale = 2)

x3 = rbinom(ndraws, size = 1, prob = 0.3)

eps = rnorm(ndraws, mean = 2, sd = 1)

# now we create y and ydum

y = 0.5 + 1.2*x1 - 0.9*x2 + 0.1*x3 + eps

ydum = as.numeric(y > mean(y))

# Exercise 6 ##############################

cor(x1, y)

# Correlation is 0.2003121. It is different from 1.2 almost by 1.

# OLS calculations. 

ones = rep(1, ndraws)

xmat = matrix(c(ones, x1, x2, x3), ncol = 4)

library(MASS)

# checking matrix invertibility
print(det(t(xmat)%*%xmat))
print(ginv(t(xmat)%*%xmat))

betas = ginv(t(xmat)%*%xmat) %*% (t(xmat)%*%y)
# Coefficients of the regression
print(betas)

# Standard errors

resids = xmat %*% betas - y
sigmahat2 = (t(resids) %*% resids)/(ndraws - ncol(xmat))
print(sigmahat2)

# variance-covariance matrix of betas

varcov = c(sigmahat2) * ginv(t(xmat)%*%xmat)
print(varcov)

#calculating standard errors

stder = sqrt(diag(varcov))
print(stder)

# testing correctness ################

olsresult = matrix(c(betas, stder), ncol = 2)
print(olsresult)

testreg = lm(y ~ ones + x1 + x2 +x3)
summary(testreg)

# as seen, the results are the same

######################################

# Exercise 7 #########################

# Linear Probability Model ##########

betas_lpm = ginv(t(xmat)%*%xmat) %*% (t(xmat)%*%ydum)
# Coefficients of the regression
print(betas_lpm)

# testing ols result
testreg1 = lm(ydum ~ ones + x1 + x2 +x3)
summary(testreg1)

# Probit #############################

# likelihood function

flike_pr = function(par,x1,x2,x3,ydum)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  loglike           = ydum*log(pr) + (1-ydum)*log(1-pr)
  return(-sum(loglike))
}

# Checking correctness

testreg2 = glm(ydum~x1+x2+x3,family = binomial(link = "probit"))
summary(testreg2)

test_par = testreg2$coefficients
flike_pr(test_par,x1,x2,x3,ydum)
logLik(testreg2)

# we get same results

## Optimization

ntry = 100
out_pr = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-1,1)
  res_pr      = optim(start,fn=flike_pr,method="BFGS",control=list(trace=6,Report=1, maxit=1000),x1=x1,x2=x2,x3=x3,ydum=ydum, hessian = TRUE)
  out_pr[i0,] = res_pr$par
}

# Interpretation

# coefficients on (x1,x2,x3) are (1.1584, -0.8979, 0.00759). This means that x1 and x3 have positive effect on probability of observing ydum
# x2 reduces probability of observing ydum. Both x1 and x2 are significant. Likelihood is maxed at -2148

###### Logit ###################

flike_lgt = function(par,x1,x2,x3,ydum)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = exp(xbeta)/(1 + exp(xbeta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  loglike           = ydum*log(pr) + (1-ydum)*log(1-pr)
  return(-sum(loglike))
}

# Checking correctness

testreg3 = glm(ydum~x1+x2+x3,family = binomial(link = "logit"))
summary(testreg3)

test_par_lgt = testreg3$coefficients
flike_lgt(test_par_lgt,x1,x2,x3,ydum)
logLik(testreg3)

# OPtimization

out_lgt = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-1,1)
  res_lgt      = optim(start,fn=flike_lgt,method="BFGS",control=list(trace=6,Report=1, maxit=1000),x1=x1,x2=x2,x3=x3,ydum=ydum, hessian = TRUE)
  out_lgt[i0,] = res_lgt$par
}

# Interpretation.

# coefficients on (x1,x2,x3) are (2.08, -1.608, 0.141). They  have same signs as for probit and have same interpretation.
# Both x1 and x2 are significant. Likelihood is amxed at -2156


##### Exercise 8 ###########

## Probit, Marginal effect at Mean

summary(margins(testreg2))

mean_vec = apply(xmat,2,mean)

c1 = res_pr$par[1]
c2 = res_pr$par[2]
c3 = res_pr$par[3]
c4 = res_pr$par[4]
pr.exp = expression(pnorm(c1 + c2*xx1 + c3*xx2 + c4*xx3))
(pr_drv = deriv(pr.exp, c("xx1", "xx2", "xx3")))

xx1 = mean_vec[2]
xx2 = mean_vec[3]
xx3 = mean_vec[4]

# we get marginal effect at mean
eval(pr_drv)
#verifying that 
summary(margins(testreg2, at = list(x1 = mean_vec[2], x2 = mean_vec[3], x3 = mean_vec[4])))

# Standard errors calculation

c_pr = c(c1,c2,c3,c4)
x_meanvec = c(xx1, xx2, xx3)

grad_vec_pr = c(dnorm(c1 + c2*xx1 + c3*xx2 + c4*xx3)* c2, dnorm(c1 + c2*xx1 + c3*xx2 + c4*xx3)* c3, dnorm(c1 + c2*xx1 + c3*xx2 + c4*xx3)* c4)

xmat_red = matrix(c(x1, x2, x3), ncol = 3)
covmat = cov(xmat_red)

delta_pr = t(grad_vec_pr) %*% covmat %*% grad_vec_pr
se_marg_pr = sqrt(diag(delta_pr))

# Logit Marginal effect at mean

summary(margins(testreg3))

d1 = res_lgt$par[1]
d2 = res_lgt$par[2]
d3 = res_lgt$par[3]
d4 = res_lgt$par[4]
lgt.exp = expression(exp(d1 + d2*xx1 + d3*xx2 + d4*xx3)/(1+exp(d1 + d2*xx1 + d3*xx2 + d4*xx3)))
(lgt_drv = deriv(lgt.exp, c("xx1", "xx2", "xx3")))

xx1 = mean_vec[2]
xx2 = mean_vec[3]
xx3 = mean_vec[4]

# we get marginal effect at mean
eval(lgt_drv)
#verifying that
summary(margins(testreg3, at = list(x1 = mean_vec[2], x2 = mean_vec[3], x3 = mean_vec[4])))
