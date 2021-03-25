
rm(list=ls()) #remove all variable
setwd("D:/New Order/Fourth Semester/ECON 613/R-workdir")

library(bayesm)
library(tidyverse)
library(data.table)
library(stringr)
library(dplyr)
library(pracma)
library(snow)
library(nloptr)
library(boot)
set.seed(3)


data(margarine)

str(margarine)

mrg <- merge(margarine$choicePrice, margarine$demos, by = "hhid")

###### Exercise 1 #########################

### Part 1

prices <- mrg %>%
  select(PPk_Stk:PHse_Tub)

# Mean in prices 

apply(prices, 2, mean)

# Variance in prices

apply(prices, 2, var)

# Standard Deviation in prices

apply(prices, 2, sd)

# Quantiles

apply(prices, 2, quantile)

### Part 2

mkt_shr <- mrg %>%
  group_by(choice) %>%
  summarize(size=n())

# market share of each butter type
prop.table(mkt_shr$size)


### Part 3

# illustrates which consumer chose which type of margarine most frequently
dat_map = mrg %>% dplyr::select(hhid,choice,Income) %>% group_by(hhid, Income) %>% summarise(topchoice = Mode(choice))


###### Exercise 2

pricess = data.matrix(prices)

## model proposed is y = product specific constant + beta * x 

## Programming conditional logit

like_fun_cndlgt = function(param,mrg,pricess)
{
  income     =  mrg$Income
  ch         =  mrg$choice
  ni = nrow(pricess)
  nj = ncol(pricess)
  ut = mat.or.vec(ni,nj)
  
  for (j in 1:nj)
  {
    #conditional logit
    if (j != 1) {
      ut[,j] = param[j-1] + param[nj]*pricess[,j]
    }
    else {
      ut[,j] = param[nj]*pricess[,j]
    }

  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

npar  = 10
param = runif(npar)
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar) 

res_cndlgt   = nloptr(start,eval_f= like_fun_cndlgt, lb=lower,ub=upper,
                          opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                          mrg = mrg, pricess = pricess)

# another way to optimize
#ntry = 20
#out_cdlg = mat.or.vec(ntry,npar)
#for (i0 in 1:ntry)
#{
#  start    = runif(npar,-10,10)
#  res_cdlg      = optim(start,fn=like_fun_cndlgt,method="BFGS",control=list(trace=6,Report=1, maxit=1000),mrg = mrg, pricess = pricess, hessian = TRUE)
#  out_cdlg[i0,] = res_cdlg$par
#}

#print(res_cdlg$par)
print(res_cndlgt$solution)

# coefficient on price is negative which suggests that the higher the price the less likely people buy a product on average


###### Exercise 3

## proposed model is y = product specific constant + product specific income effect * x

like_fun_mltlgt = function(param,mrg)
{
  income     =  mrg$Income
  ch         =  mrg$choice
  fsize      =  mrg$Fam_Size
  fsize4     =  mrg$Fs3_4
  fsize5     =  mrg$Fs5.
  colg       =  mrg$college
  whcl       =  mrg$whtcollar
  rtr        =  mrg$retired
  ni = nrow(mrg)
  nj = length(param)/2
  ut = mat.or.vec(ni,nj+1)
  
  # multinomial logit
  pn1    = param[1:nj]
  pn2    = param[(nj+1):(2*nj)]
  
  for (j in 1:nj+1)
  {
    # multinomial logit
    
    if (j != 1) {
      ut[,j] = pn1[j-1] + income*pn2[j-1]
    }
    else {
      ut[,j] = 0
    }
    
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

npar_mlt  = 18
param_mlt = runif(npar_mlt)
lower_mlt  = rep(-100,npar_mlt)
upper_mlt  = rep(100,npar_mlt)
start_mlt  = runif(npar_mlt) 

res_mltlgt   = nloptr(start_mlt,eval_f= like_fun_mltlgt, lb=lower_mlt,ub=upper_mlt,
                      opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                      mrg = mrg)

# another way to optimize
#ntry = 20
#out_mllg = mat.or.vec(ntry,npar_mlt)
#for (i0 in 1:ntry)
#{
#  start    = runif(npar_mlt,-5,5)
#  res_mllg      = optim(start,fn=like_fun_mltlgt,method="BFGS",control=list(trace=6,Report=1, maxit=1000),mrg = mrg, hessian = TRUE)
#  out_mllg[i0,] = res_mllg$par
#}

#print(res_mllg$par)
print(res_mltlgt$solution)

## all the coefficients on family income are positive, which suggests that as income increases, people are more likely to choose
## types of margarine 2-10 compared to type 1 (reference category) 

## For my own convenience, Exercise 4 is after exercise 5


###### Exercise 5

like_fun_mxdlgt = function(param,mrg, pricess)
{
  income     =  mrg$Income
  ch         =  mrg$choice
  fsize      =  mrg$Fam_Size
  fsize4     =  mrg$Fs3_4
  fsize5     =  mrg$Fs5.
  colg       =  mrg$college
  whcl       =  mrg$whtcollar
  rtr        =  mrg$retired
  ni = nrow(mrg)
  nj = ncol(pricess)
  ut = mat.or.vec(ni,nj)
  
  pn1    = param[1:(nj-1)]
  pn2    = param[nj:(2*nj-2)] 
  
  for (j in 1:nj)
  {
    # multinomial logit
    if (j != 1) {
      ut[,j] = pn1[j-1] + pn2[j-1]*income +  param[2*nj-1]*pricess[,j]
    }
    else {
      ut[,j] = param[2*nj-1]*pricess[,j]
    }
    
  }
  prob   = exp(ut)            # exp(XB)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) 
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

#First, we run a mixed logit on a full data set
npar_mxd  = 19
param_mxd = runif(npar_mxd)
lower_mxd  = rep(-100,npar_mxd)
upper_mxd  = rep(100,npar_mxd)
start_mxd  = runif(npar_mxd) 

# I used derivative free optimization for this exercise, as it works faster than the optim on my computer
res_mxdlgt   = nloptr(start_mxd,eval_f= like_fun_mxdlgt, lb=lower_mxd,ub=upper_mxd,
                      opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=20000),
                      mrg = mrg, pricess = pricess)

print(res_mxdlgt$solution) # Beta F coefficients

# Second, we run a mixed logit on a reduced data set
mrg_omit <- subset(mrg, choice != 10)

cols.dont.want <- "PHse_Tub"
mrg_omit <- mrg_omit[, ! names(mrg_omit) %in% cols.dont.want, drop = F]

prices_omit <- mrg_omit %>%
  select(PPk_Stk:PFl_Tub)

pricess_omit <- data.matrix(prices_omit)

npar_mxd_omt  = 17
param_mxd = runif(npar_mxd_omt)
lower_mxd  = rep(-100,npar_mxd_omt)
upper_mxd  = rep(100,npar_mxd_omt)
start_mxd  = runif(npar_mxd_omt)

res_mxdlgt_omit   = nloptr(start_mxd,eval_f= like_fun_mxdlgt, lb=lower_mxd,ub=upper_mxd,
                      opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=20000),
                      mrg = mrg_omit, pricess = pricess_omit)

print(res_mxdlgt_omit$solution)  # Beta R coefficients
print(res_mxdlgt$solution) # Beta F coefficients

print(-2 * (res_mxdlgt$objective - res_mxdlgt_omit$objective))

# the value is large, which suggests that null hypothesis doesnot hold. So IIA is violated.







####### Exercise 4

## function that returns marginal effects of conditional logit for specific individual

margeff_cndlgt = function(param,mrg, pricess, indv)
{
  ch         =  mrg$choice
  ni = nrow(pricess)
  nj = ncol(pricess)
  ut = mat.or.vec(ni,nj)
  m = indv
  
  for (j in 1:nj)
  {
    #conditional logit
    if (j != 1) {
      ut[,j] = param[j-1] + param[nj]*pricess[,j]
    }
    else {
      ut[,j] = param[nj]*pricess[,j]
    }
    
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  
  mfx_cnd = mat.or.vec(nj,nj)
  for (j in 1:nj){
    
    for (k in 1:nj){
      if (j!=k){
        mfx_cnd[j,k] = - prob[m,j] * prob[m,k] * param[nj]
      }
      else{
        mfx_cnd[j,k] = prob[m,j] * (1-prob[m,k]) * param[nj]
      }
    }
    
  }
  return(mfx_cnd)
}

# marginal effects for individual 10
mfx_cnd_10 = margeff_cndlgt(res_cndlgt$solution, mrg, pricess, 10)

# marginal effects for individual 11
mfx_cnd_11 = margeff_cndlgt(res_cndlgt$solution, mrg, pricess, 11)


## function that returns marginal effects of multinomial logit for specific individual

margeff_mltlgt = function(param,mrg, indv){
  income     =  mrg$Income
  ch         =  mrg$choice
  ni = nrow(mrg)
  nj = length(param)/2
  ut = mat.or.vec(ni,nj+1)
  m = indv
  njj = nj+1
  
  # multinomial logit
  pn1    = param[1:nj]
  pn2    = param[(nj+1):(2*nj)]
  
  for (j in 1:njj)
  {
    # multinomial logit
    
    if (j != 1) {
      ut[,j] = pn1[j-1] + income*pn2[j-1]
    }
    else {
      ut[,j] = 0
    }
    
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  
  mfx_mlt = mat.or.vec(1, njj)
  b_bar = 0
  for (j in 2:njj){
    b_bar = prob[m,j]*pn2[j-1] + b_bar
  }

  for (j in 1:njj){
    if (j != 1) {
      mfx_mlt[j] = prob[m,j] * (pn2[j-1] - b_bar)
    }
    else {
      mfx_mlt[j] = prob[m,j] * ( - b_bar)
    }
    
  }
  return(mfx_mlt)
}

# marginal effects for individual 10
mfx_mlt_10 = margeff_mltlgt(res_mltlgt$solution, mrg, 10)

# marginal effects for individual 150
mfx_mlt_150 = margeff_mltlgt(res_mltlgt$solution, mrg, 150)