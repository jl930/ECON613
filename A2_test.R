
#Problem Set A2


#Question 1
#install.packages("bayesm")
library(bayesm)
data(margarine)
head(margarine)
library(mlogit)
library(stargazer)

cat(" Table of Choice Variable ", fill=TRUE)
print(table(margarine$choicePrice[,2]))
#=============
#Excercise 1
#=============

cat(" Means of Prices", fill=TRUE)
mat=apply(as.matrix(margarine$choicePrice[,3:12]), 2, mean)
stargazer(mat)
print(mat)

dispersion=apply(as.matrix(margarine$choicePrice[,3:12]), 2, sd)
print(dispersion)

market<-table(margarine$choicePrice[,2])
prop.table(market)
CP<-data.frame(margarine$choicePrice)
name<-names(CP)
CP$index<-CP$choice+2
for (i in 1: nrow(CP)) {
  CP$name[i]<-name[CP$index[i]]
}
for (i in 1: nrow(CP)) {
  CP$average[i]<-mat[CP$choice[i]]
}

for (i in 1: nrow(CP)) {
  
  CP$chprice[i]<-CP[i,CP$index[i]]
}


market_below<-table(CP$choice[which(CP$chprice<CP$average)])
k<-prop.table(market_below)
m<-as.numeric(k)
market_over<-table(CP$choice[which(CP$chprice>=CP$average)])
prop.table(market_over)
price<-as.matrix(CP[,3:12])
like_fun=function(param,CP)
{
  ni=nrow(CP)
  nj=length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  beta    = param[10]
  cons<-append(0,param[1:9])
  for (j in 1:nj)
  {ut[,j] =beta*price[,j]+cons[j]
  }
  prob= exp(ut)
  sprob=rowSums(prob)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,CP$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

library(nloptr)
npar=10
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar)
conditional_logit   = nloptr(start,eval_f=like_fun, lb=lower,ub=upper,
                          opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10,"maxeval"=5000),
                          CP=CP)

conditional_logit$solution
param<-conditional_logit$solution

ni=nrow(CP)
nj=length(unique(CP$choice))
ut = mat.or.vec(ni,nj)
beta    = param[10]
cons<-append(0,param[1:9])
for (j in 1:nj)
{ut[,j] =beta*price[,j]+cons[j]
}
prob= exp(ut)
sprob=rowSums(prob)
prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
me_con<-mat.or.vec(10,10)
for (i in 1:ni)
{
  probc[i] = prob[i,CP$choice[i]]
}

me_con<-array(0,dim=c(10,10,4470))
for (i in 1:4470) {
  for (j in 1:10)
  {
    for (k in 1:10){
      if(k==j) {me_con[j,k,i]<- prob[i,j]*(1-prob[i,k])*beta}
       
      else {me_con[j,k,i]<- prob[i,j]*prob[i,k]*beta*(-1)}
    }
    
  }
}

apply(me_con,c(1,2),mean)
  
  
  

#=============
#Exercise 3
#=============

#I am going to use the multinomial logit model

library(dplyr)
Demo<-data.frame(margarine$demos)
In<-left_join(CP,Demo,by="hhid")
like_fun1=function(param,CP)
{
  
  ni=nrow(CP)
  nj=length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  cons<-append(0,param[1:9])
  beta <- append(0,param[10:18])
  for (j in 1:nj)
  {
    ut[,j] =CP$Income*beta[j]+cons[j]
  }
  prob= exp(ut)
  sprob=rowSums(prob)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,CP$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  like = sum(log(probc))
  return(-like)
}
library(nloptr)
npar=18
lower  = rep(-5,npar)
upper  = rep(5,npar)
start  = runif(npar)

mul2  = nloptr(start,eval_f=like_fun1, lb=lower,ub=upper,
                            opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-8,"maxeval"=10000),
                            CP=In)

mul2$solution

ni=nrow(In)
nj=length(unique(In$choice))
ut = mat.or.vec(ni,nj)
param<-mul2$solution
cons<-append(0,param[1:9])
beta <- append(0,param[10:18])
for (j in 1:nj)
{
  ut[,j] =In$Income*beta[j]+cons[j]
}
prob= exp(ut)
sprob=rowSums(prob)
prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
probc = NULL
for (i in 1:ni)
{
  probc[i] = prob[i,CP$choice[i]]
}
beta<-as.matrix(beta)
beta_hat<-mat.or.vec(4470,0)
for (i in 1:4470){
  beta_hat[i]<-prob[i,]%*%beta
}

me_mul<-mat.or.vec(4470,10)
for (i in 1:4470){for (j in 1:10){
  me_mul[i,j]=prob[i,j]*(beta[j]-beta_hat[i])
  
}}
#marginal effect of multinomial model
apply(as.matrix(me_mul), 2, mean)

#=============
#Exercise 5
#=============


like_mix=function(param,CP)
{
  
  ni=nrow(CP)
  nj=length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  cons<-append(0,param[1:9])
  beta1 <- append(0,param[10:18])
  beta2<-param[19]
  for (j in 1:nj)
  {
    ut[,j] =CP$Income*beta1[j]+cons[j]+beta2*price[,j]
  }
  prob= exp(ut)
  sprob=rowSums(prob)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,CP$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  like = sum(log(probc))
  return(-like)
}


npar=19
lower  = rep(-7,npar)
upper  = rep(2,npar)
start  = runif(npar)

mix2<-nloptr(start,eval_f=like_mix, lb=lower,ub=upper,
             opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
             CP=In)

mix2$solution
L_f<-mix2$objective




#=====================
# If we drop choice 10
#=====================
new<-In[-which(In$choice==10),]
price<-new[,3:11]
like_mix1=function(param,CP)
{
  
  ni=nrow(CP)
  nj=length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  cons<-append(0,param[1:8])
  beta1 <- append(0,param[9:16])
  beta2<-param[17]
  for (j in 1:nj)
  {
    ut[,j] =CP$Income*beta1[j]+cons[j]+beta2*price[,j]
  }
  prob= exp(ut)
  sprob=rowSums(prob)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,CP$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  like = sum(log(probc))
  return(-like)
}
npar=17
lower  = rep(-7,npar)
upper  = rep(2,npar)
start  = runif(npar)
mix2_new<-nloptr(start,eval_f=like_mix1, lb=lower,ub=upper,
                 opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                 CP=new)

mix2_new$solution
mix2_new$objective
L_r<-mix2_new$objective


MTT<--2*(L_f-L_r)
