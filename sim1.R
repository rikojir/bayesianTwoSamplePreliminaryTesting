set.seed(42)
require(BayesFactor) # library for JZS Bayes factor
require(dplyr) # for dplyr tools
require(logspline) # for rankSumGibbsSampler

s=10000 # simulation size
alpha_pre=c(0.1,0.05,0.01,1) # pretest levels
n=c(10,20,30,40) # sample sizes

library(parallel) # for parallelization
numCores = 4

# Simulation function, parallelized for alpha_pre values
sim = function(alpha){
  SW1N = pValTtest1N = JZSBF1N = matrix(0,nrow=s,ncol=length(n))
  for(i in 1:length(n)){
    for(j in 1:s){
      if(alpha!=1){ # if alpha=1, no pretest, move on with t.test and JZS BF test
        while(SW1N[j,i]<alpha){
          xDat=rnorm(n[i],0,1)
          yDat=rnorm(n[i],0,1)
          SW1N[j,i]=min(shapiro.test(xDat)$p.value,shapiro.test(yDat)$p.value)
        }
      } else{
        xDat=rnorm(n[i],0,1)
        yDat=rnorm(n[i],0,1)
      }
    pValTtest1N[j,i]=t.test(xDat,yDat,conf.level = 0.95,paired=FALSE)$p.value
    JZSBF1N[j,i]=extractBF(ttestBF(x=xDat,y=yDat,paired=FALSE))$bf
    }
  }
  result = list(pValTtest1N,JZSBF1N)
  result
}
# system.time(
#   results <- lapply(alpha_pre, sim)
# )
system.time(
  results1 <- mclapply(alpha_pre, sim, mc.cores = numCores) # parallel execution for all alpha_pre levels
)

# t-test p-values and JZS BFs
alphaTtestAlphaPre1N=alphaJZSBFAlphaPre1N=numeric(length(n)) # p-values & BFs for alpha_pre = 0.100
alphaTtestAlphaPre05N=alphaJZSBFAlphaPre05N=numeric(length(n)) # p-values & BFs for alpha_pre = 0.05
alphaTtestAlphaPre01N=alphaJZSBFAlphaPre01N=numeric(length(n)) # p-values & BFs for alpha_pre = 0.01
alphaTtestAlphaPreNoPreN=alphaJZSBFAlphaPreNoPreN=numeric(length(n)) # p-values & BFs for alpha_pre = 1 ( no pretest)

# alpha = 0.1
for(j in 1:length(n)){
      alphaTtestAlphaPre1N[j]=as.numeric(tbl_df(results1[[1]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPre1N[j]=as.numeric(tbl_df(results1[[1]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# alpha = 0.05
for(j in 1:length(n)){
      alphaTtestAlphaPre05N[j]=as.numeric(tbl_df(results1[[2]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPre05N[j]=as.numeric(tbl_df(results1[[2]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# alpha = 0.01
for(j in 1:length(n)){
      alphaTtestAlphaPre01N[j]=as.numeric(tbl_df(results1[[3]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPre01N[j]=as.numeric(tbl_df(results1[[3]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# no pretest
for(j in 1:length(n)){
      alphaTtestAlphaPreNoPreN[j]=as.numeric(tbl_df(results1[[4]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaJZSBFAlphaPreNoPreN[j]=as.numeric(tbl_df(results1[[4]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

print("SIM1")
print("alphaTtestAlphaPre1N:\n")
alphaTtestAlphaPre1N
print("alphaJZSBFAlphaPre1N:\n")
alphaJZSBFAlphaPre1N
print("alphaTtestAlphaPre05N:\n")
alphaTtestAlphaPre05N
print("alphaJZSBFAlphaPre05N:\n")
alphaJZSBFAlphaPre05N
print("alphaTtestAlphaPre01N:\n")
alphaTtestAlphaPre01N
print("alphaJZSBFAlphaPre01N:\n")
alphaJZSBFAlphaPre01N
print("alphaTtestAlphaPreNoPreN:\n")
alphaTtestAlphaPreNoPreN
print("alphaJZSBFAlphaPreNoPreN:\n")
alphaJZSBFAlphaPreNoPreN
