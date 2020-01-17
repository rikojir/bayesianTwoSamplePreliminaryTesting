require(BayesFactor) # library for JZS Bayes factor
require(tidyverse) # for dplyr tools
require(logspline) # for rankSumGibbsSampler

set.seed(42)
s=1000 # simulation size
alpha_pre=c(0.1,0.05,0.01,1) # pretest levels
n=c(10,20,30,40) # sample sizes

library(parallel) # for parallelization
numCores <- 4 # set number of available cores of machine

# Simulation function, parallelized for alpha_pre values
sim = function(alpha){
  SW1U = pValTtest1U = JZSBF1U = matrix(0,nrow=s,ncol=length(n))
  for(i in 1:length(n)){
    for(j in 1:s){
      if(alpha!=1){ # if alpha=1, no pretest, move on with t.test and JZS BF test
        while(SW1U[j,i]<alpha){
          xDat=runif(n[i],0,1)
          yDat=runif(n[i],0,1)
          SW1U[j,i]=min(shapiro.test(xDat)$p.value,shapiro.test(yDat)$p.value)
        }
      } else{
        xDat=runif(n[i],0,1)
        yDat=runif(n[i],0,1)
      }
    pValTtest1U[j,i]=t.test(xDat,yDat,conf.level = 0.95,paired=FALSE)$p.value
    JZSBF1U[j,i]=extractBF(ttestBF(x=xDat,y=yDat,paired=FALSE))$bf
    }
  }
  result = list(pValTtest1U,JZSBF1U)
  result
}

# system.time(
#   results <- lapply(alpha_pre, sim)
# )
system.time(
  results5 <- mclapply(alpha_pre, sim, mc.cores = numCores) # parallel execution for all alpha_pre levels
)

# t-test p-values and JZS BFs
alphaTtestAlphaPre1U=alphaJZSBFAlphaPre1U=numeric(length(n)) # p-values & BFs for alpha_pre = 0.100
alphaTtestAlphaPre05U=alphaJZSBFAlphaPre05U=numeric(length(n)) # p-values & BFs for alpha_pre = 0.05
alphaTtestAlphaPre01U=alphaJZSBFAlphaPre01U=numeric(length(n)) # p-values & BFs for alpha_pre = 0.01
alphaTtestAlphaPreNoPreU=alphaJZSBFAlphaPreNoPreU=numeric(length(n)) # p-values & BFs for alpha_pre = 1 ( no pretest)

# alpha = 0.1
for(j in 1:length(n)){
      alphaTtestAlphaPre1U[j]=as.numeric(tbl_df(results5[[1]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPre1U[j]=as.numeric(tbl_df(results5[[1]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# alpha = 0.05
for(j in 1:length(n)){
      alphaTtestAlphaPre05U[j]=as.numeric(tbl_df(results5[[2]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPre05U[j]=as.numeric(tbl_df(results5[[2]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# alpha = 0.01
for(j in 1:length(n)){
      alphaTtestAlphaPre01U[j]=as.numeric(tbl_df(results5[[3]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPre01U[j]=as.numeric(tbl_df(results5[[3]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# no pretest
for(j in 1:length(n)){
      alphaTtestAlphaPreNoPreU[j]=as.numeric(tbl_df(results5[[4]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPreNoPreU[j]=as.numeric(tbl_df(results5[[4]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

print("SIM5")
print("alphaTtestAlphaPre1U\n")
alphaTtestAlphaPre1U
print("alphaJZSBFAlphaPre1U\n")
alphaJZSBFAlphaPre1U
print("alphaTtestAlphaPre05U\n")
alphaTtestAlphaPre05U
print("alphaJZSBFAlphaPre05U\n")
alphaJZSBFAlphaPre05U
print("alphaTtestAlphaPre01U\n")
alphaTtestAlphaPre01U
print("alphaJZSBFAlphaPre01U\n")
alphaJZSBFAlphaPre01U
print("alphaTtestAlphaPreNoPreU\n")
alphaTtestAlphaPreNoPreU
print("alphaJZSBFAlphaPreNoPreU\n")
alphaJZSBFAlphaPreNoPreU
