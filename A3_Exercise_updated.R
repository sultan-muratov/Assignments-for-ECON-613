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


population <-read.csv('population.csv')
crime <- read.csv('crime_long.csv')
officers <- read.csv('officers.csv')

### Exercise 2
  
total_crime = crime %>% dplyr::select(crime_month, crimes) %>% group_by(crime_month) %>% summarise(monthly_crime = sum(crimes))

plot(monthly_crime ~ I(1:nrow(total_crime)), data = total_crime, axes = FALSE, xlab = "crime_month", type = "b")
axis(side = 2)
axis(side = 1, at = 1:nrow(total_crime), labels = total_crime$crime_month)

####

crime_renamed <-crime %>% rename(month = crime_month)

crime_updated = crime_renamed %>% dplyr::select(month, district, crime_type, crimes) %>% group_by(month, district, crime_type) %>% summarise(crimes = sum(crimes))

pop_crime = merge(crime_updated, population, by = c("month" , "district"))

# pop-crime is our merged district/period dataset

pop_crime = pop_crime %>% mutate(sh_whit = tot_white/tot_pop, sh_blck = tot_black/tot_pop, sh_hisp = tot_hisp/tot_pop)

tot_crime = pop_crime %>% dplyr::select(month, district, crimes) %>% group_by(month, district) %>% summarise(total_crime = sum(crimes))

pop_crime = merge(pop_crime, tot_crime, by = c("month" , "district"))

# estimating violent and property crime per resident

vc_crime = subset(pop_crime, crime_type =="violent")
vc_crime = vc_crime %>% mutate(vcpr = crimes/tot_pop)

pr_crime = subset(pop_crime, crime_type =="property")
pr_crime = pr_crime %>% mutate(pcpr = crimes/tot_pop)

vc_crime = subset(vc_crime, select = c("month", "district", "vcpr"))
pr_crime = subset(pr_crime, select = c("month", "district", "pcpr"))

# adding vcpr and pcpr into the large dataset

pop_crime = merge(pop_crime, vc_crime, by = c("month" , "district"))
pop_crime = merge(pop_crime, pr_crime, by = c("month" , "district"))

# creating tcpr (total crime per resident)

pop_crime = pop_crime %>% mutate(tcpr = total_crime/tot_pop)

# we finished constructing this dataset



#### Exercise 3


# First, we reanme units by districts and create combined dataset

officers_rnm <-officers %>% rename(district = unit)

comb_dt = merge(officers_rnm, pop_crime, by = c("month", "district"))

lm_ex3 = lm(arrest~tenure + total_crime + p50_inc + sh_whit + sh_blck + sh_hisp, data = comb_dt)
summary(lm_ex3)


#### Exercise 4

lm_ex4 = lm(arrest~tenure + total_crime + p50_inc + sh_whit + sh_blck + sh_hisp + factor(district) + factor(month), data = comb_dt)
summary(lm_ex4)


#### Exercise 5

# First we need to compute some relevant variables for estimators

# we need to compute time averages of tenure and arrests for each officer

avg_tenure_arrests <- officers_rnm %>% dplyr::select(NUID, tenure, arrest) %>% group_by(NUID) %>% summarise(time_avg_tenure = mean(tenure), time_avg_arrest = mean(arrest))

comb_dt <-merge(comb_dt, avg_tenure_arrests, by = c("NUID"))

comb_dt <- comb_dt %>% arrange(month, district)

differences = officers_rnm %>% group_by(NUID) %>% mutate(diff_tenur = tenure-lag(tenure), diff_arrest = arrest - lag(arrest))

comb_dt <-merge(comb_dt, differences, by = c("NUID"))

comb_dt <- comb_dt %>% arrange(month, district)



## now we try to estimate within estimator for beta

within_dta = subset(comb_dt, select = c("NUID", "month", "district", "tenure", "arrest", "time_avg_tenure", "time_avg_arrest"))

within_dta = within_dta %>% mutate(within_tenure = tenure-time_avg_tenure, within_arrest = arrest - time_avg_arrest)

within_dta = unique(within_dta)

within_dta <- within_dta %>% arrange(NUID, month)

## I couldnot implement it because I am confused with the dimentionality of the data

## I try to implement between estimator

btw_dta = subset(comb_dt, select = c("NUID", "time_avg_tenure", "time_avg_arrest"))
btw_dta = unique(btw_dta)
btw_dta <- btw_dta %>% arrange(NUID)

gmm_btw = function(param, dta){
  tenure = dta$time_avg_tenure
  arrest = dta$time_avg_arrest
  sum = 0
  for(i in 1:nrow(dta)){
    if (i != 1) {
      sum = (arrest[i] - param[nrow(data)]*tenure[i]-param[i-1])^2 + sum
    }
    else {
      sum = (arrest[i] - param[nrow(dta)] * tenure[i])^2 + sum
    }
    
  }
  return(-sum)
}

ntry = 20
npar = 13028
out_btw = mat.or.vec(ntry,npar)
for (i0 in 1:ntry)
{
  start    = runif(npar,-5,5)
  res_btw      = optim(start,fn=gmm_btw,method="BFGS",control=list(trace=6,Report=1, maxit=1000),dta =btw_dta, hessian = TRUE)
  out_btw[i0,] = res_btw$par
}
