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
  SW1E = pValTtest1E = JZSBF1E = matrix(-1,nrow=s,ncol=length(n))
  for(i in 1:length(n)){
    for(j in 1:s){
      if(alpha!=1){ # if alpha=1, no pretest, move on with t.test and JZS BF test
        if((alpha==.1 && n[i]<40) | alpha==.05 | alpha==.01 | alpha==.005){
          while(SW1E[j,i]<alpha){
            xDat=rexp(n[i],1)
            yDat=rexp(n[i],1)
            SW1E[j,i]=min(shapiro.test(xDat)$p.value,shapiro.test(yDat)$p.value)
          }
          pValTtest1E[j,i]=t.test(xDat,yDat,conf.level = 0.95,paired=FALSE)$p.value
          JZSBF1E[j,i]=extractBF(ttestBF(x=xDat,y=yDat,paired=FALSE))$bf
        }
      } else{
        xDat=rexp(n[i],1)
        yDat=rexp(n[i],1)
        pValTtest1E[j,i]=t.test(xDat,yDat,conf.level = 0.95,paired=FALSE)$p.value
        JZSBF1E[j,i]=extractBF(ttestBF(x=xDat,y=yDat,paired=FALSE))$bf
      }
    }
  }
  result = list(pValTtest1E,JZSBF1E)
  result
}

# system.time(
#   results <- lapply(alpha_pre, sim)
# )
system.time(
  results3 <- mclapply(alpha_pre, sim, mc.cores = numCores) # parallel execution for all alpha_pre levels
)

# t-test p-values and JZS BFs
alphaTtestAlphaPre1E=alphaJZSBFAlphaPre1E=numeric(length(n)) # p-values & BFs for alpha_pre = 0.100
alphaTtestAlphaPre05E=alphaJZSBFAlphaPre05E=numeric(length(n)) # p-values & BFs for alpha_pre = 0.05
alphaTtestAlphaPre01E=alphaJZSBFAlphaPre01E=numeric(length(n)) # p-values & BFs for alpha_pre = 0.01
alphaTtestAlphaPreNoPreE=alphaJZSBFAlphaPreNoPreE=numeric(length(n)) # p-values & BFs for alpha_pre = 1 ( no pretest)

# alpha = 0.1
for(j in 1:(length(n)-1)){ # only 3 values, because N/A for n=40
      alphaTtestAlphaPre1E[j]=as.numeric(tbl_df(results3[[1]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:(length(n)-1)){
      alphaJZSBFAlphaPre1E[j]=as.numeric(tbl_df(results3[[1]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# alpha = 0.05
for(j in 1:length(n)){
      alphaTtestAlphaPre05E[j]=as.numeric(tbl_df(results3[[2]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPre05E[j]=as.numeric(tbl_df(results3[[2]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# alpha = 0.01
for(j in 1:length(n)){
      alphaTtestAlphaPre01E[j]=as.numeric(tbl_df(results3[[3]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPre01E[j]=as.numeric(tbl_df(results3[[3]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# no pretest
for(j in 1:length(n)){
      alphaTtestAlphaPreNoPreE[j]=as.numeric(tbl_df(results3[[4]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPreNoPreE[j]=as.numeric(tbl_df(results3[[4]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

print("SIM3")
print("alphaTtestAlphaPre1E\n")
alphaTtestAlphaPre1E
print("alphaJZSBFAlphaPre1E\n")
alphaJZSBFAlphaPre1E
print("alphaTtestAlphaPre05E\n")
alphaTtestAlphaPre05E
print("alphaJZSBFAlphaPre05E\n")
alphaJZSBFAlphaPre05E
print("alphaTtestAlphaPre01E\n")
alphaTtestAlphaPre01E
print("alphaJZSBFAlphaPre01E\n")
alphaJZSBFAlphaPre01E
print("alphaTtestAlphaPreNoPreE\n")
alphaTtestAlphaPreNoPreE
print("alphaJZSBFAlphaPreNoPreE\n")
alphaJZSBFAlphaPreNoPreE
