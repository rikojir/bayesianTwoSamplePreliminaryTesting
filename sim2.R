rankSumGibbsSampler <- function(xVals,yVals, nSamples = 10000, progBar = TRUE, cauchyPriorParameter = 1/sqrt(2), 
                                nGibbsIterations = 10, decorrelate = TRUE){
  
  if (progBar) {
    myBar <- txtProgressBar(min = 1, max = nSamples, initial = 1, char = "*",style=3,width=50)
  }
  
  n1 <- length(xVals)
  n2 <- length(yVals)
  
  allRanks <- rank(c(xVals,yVals))
  xRanks <- allRanks[1:n1]
  yRanks <- allRanks[(n1+1):(n1+n2)]
  
  currentVals <- sort(rnorm((n1+n2)))[allRanks] # initial values
  
  deltaSamples <- gSamples <- acceptDecor <- numeric(nSamples)
  sampledX <- matrix(nrow = nSamples, ncol = n1)
  sampledY <- matrix(nrow = nSamples, ncol = n2)
  
  oldDeltaProp <- 0
  
  for (j in 1:nSamples) {
    
    for (i in sample(1:(n1+n2))) {
      
      currentRank <- allRanks[i]
      
      currentBounds <- upperLowerTruncation(ranks=allRanks, values=currentVals, currentRank=currentRank)
      if (i <= n1) {
        oldDeltaProp <- -0.5*oldDeltaProp
      } else if (i > n1) {
        oldDeltaProp <- 0.5*oldDeltaProp
      }
      
      currentVals[i] <- myTruncNormSim(currentBounds[["under"]], currentBounds[["upper"]], mu=oldDeltaProp, sd=1)
      
    }
    
    xVals <- currentVals[1:n1]
    yVals <- currentVals[(n1+1):(n1+n2)]
    
    if(decorrelate) {
      d <- decorrelateStepTwoSample(xVals, yVals, oldDeltaProp, 0.5)
      xVals <- d[[1]]
      yVals <- d[[2]]
      acceptDecor[j] <- d[[3]]
    }
    
    gibbsResult <- sampleGibbsTwoSample(x = xVals, y = yVals, n1 = n1, n2 = n2, nIter = nGibbsIterations,
                                        rscale = cauchyPriorParameter)
    
    deltaSamples[j] <- oldDeltaProp <- gibbsResult[1]
    gSamples[j] <- gibbsResult[2]
    
    if (progBar) setTxtProgressBar(myBar,j) 
    sampledX[j,] <- xVals
    sampledY[j,] <- yVals
  }
  resultsList <- list(deltaSamples = deltaSamples, gSamples = gSamples,
                      sampledX = sampledX, sampledY = sampledY, acceptDecor = acceptDecor)
  return(resultsList)
}

sampleGibbsTwoSample <- function(x, y, n1, n2, nIter = 10, rscale = 1/sqrt(2)) {
  
  meanx <- mean(x)
  meany <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sigmaSq <- 1 # Arbitrary number for sigma
  g <- 1
  
  for(i in 1:nIter){   
    #sample mu
    varMu <- (4 * g * sigmaSq) / ( 4 + g * (n1 + n2) )
    meanMu <- (2 * g * (n2 * meany - n1 * meanx)) / ((g * (n1 + n2) + 4))
    mu <- rnorm(1, meanMu, sqrt(varMu)) 
    # sample g
    betaG <- (mu^2 + sigmaSq * rscale^2) / (2*sigmaSq)
    g <- 1/rgamma(1, 1, betaG)
    # convert to delta
    delta <- mu / sqrt(sigmaSq)
  }
  
  return(c(delta, mu, g))
  
}


upperLowerTruncation <- function(ranks, values, currentRank, n, ranksAreIndices = FALSE) {
  
  if (currentRank == min(ranks)) {
    under <- -Inf
  } else {
    under <- max(values[ranks < currentRank])
  }
  
  if (currentRank == max(ranks)) {
    upper <- Inf
  } else {
    upper <- min(values[ranks > currentRank])
  }
  
  return(list(under=under, upper=upper))
}

myTruncNormSim <- function(lBound = -Inf, uBound = Inf, mu = 0, sd = 1){
  
  lBoundUni <- pnorm(lBound, mean = mu, sd = sd)
  uBoundUni <- pnorm(uBound, mean = mu, sd = sd)  
  mySample <- qnorm(runif(1, lBoundUni, uBoundUni), mean = mu, sd = sd)
  
  return(mySample)
}



decorrelateStepTwoSample <- function(x, y, muProp, sigmaProp = 1) {
  
  thisZ <- rnorm(1, 0, sigmaProp)
  
  newX <- x + thisZ
  newY <- y + thisZ
  
  denom <- sum(dnorm(x, (muProp-thisZ) * -0.5, log = TRUE) + dnorm(y, (muProp-thisZ) * 0.5, log = TRUE))
  num <- sum(dnorm(newX, muProp * -0.5, log = TRUE) + dnorm(newY, muProp * 0.5, log = TRUE))
  
  if(runif(1) < exp(num - denom) ) {
    return(list(x = newX, y = newY, accept = TRUE))
  } else {
    return(list(x = x, y = y, accept = FALSE))
  }
  
}


giveBayesFactorZeroOne <- function(deltaSamples, paired = TRUE, cauchyScale  = 1/sqrt(2), testValue = 0, corTest = FALSE){
  if(corTest == FALSE) {
    
    postDens <- logspline(deltaSamples)
    densZeroPoint <- dlogspline(testValue, postDens)
    densPrior <- dcauchy(testValue, scale = cauchyScale)
    
  } else if (corTest == TRUE) {
    
    postDens <- logspline(deltaSamples)
    densZeroPoint <- dlogspline(testValue, postDens)
    densPrior <- 0.5
    
  }
  
  return(densZeroPoint/densPrior)
}

library(BayesFactor)
library(dplyr)
library(logspline)

set.seed(42)
s=1000
alpha_pre=c(0.1,0.05,0.01,1)
n=c(10,20,30,40)

library(parallel)
numCores <- 4

# Simulation function, parallelized for alpha_pre values
sim = function(alpha){
  SWD1N = pValWilcoxD1N = rankSumBFD1N = matrix(1,nrow=s,ncol=length(n))
  for(i in 1:length(n)){
    for(j in 1:s){
      if(alpha != 1){ # pretest case
        while(SWD1N[j,i]>alpha){
          xDat=rnorm(n[i],0,1)
          yDat=rnorm(n[i],0,1)
          SWD1N[j,i]=min(shapiro.test(xDat)$p.value,shapiro.test(yDat)$p.value)
        }
      } else { # no pretest case
        xDat=rnorm(n[i],0,1)
        yDat=rnorm(n[i],0,1)
      }
    pValWilcoxD1N[j,i]=wilcox.test(xDat,yDat,conf.level = 0.95,paired=FALSE)$p.value
    bfDF=rankSumGibbsSampler(xVals=xDat,yVals=yDat,nSamples = 2500, progBar = FALSE)
    rankSumBFD1N[j,i]=1/giveBayesFactorZeroOne(bfDF$deltaSamples) 
    # yields BF_01 for null hypothesis delta = 0 against alternative delta = cauchy(0,1/sqrt(2))
    # thus, we invert it to get BF_10 as we always want the BF_10 (as BF_10 >= 3 means a type I error)
    }
  }
  result = list(pValWilcoxD1N,rankSumBFD1N)
  result
}

# system.time(
#   results <- lapply(alpha_pre, sim)
# )
system.time(
  results2 <- mclapply(alpha_pre, sim, mc.cores = numCores)
)


# t-test p-values and JZS BFs
alphaWilcoxAlphaPre1N=alphaRankSumBFAlphaPre1N=numeric(length(n)) # p-values & BFs for alpha_pre = 0.100
alphaWilcoxAlphaPre05N=alphaRankSumBFAlphaPre05N=numeric(length(n)) # p-values & BFs for alpha_pre = 0.05
alphaWilcoxAlphaPre01N=alphaRankSumBFAlphaPre01N=numeric(length(n)) # p-values & BFs for alpha_pre = 0.01
alphaWilcoxAlphaPreNoPreN=alphaRankSumBFAlphaPreNoPreN=numeric(length(n)) # p-values & BFs for alpha_pre = 1 ( no pretest)

# Wilcox p-values and rankSumGibbsSampler BFs p-values
# alpha = 0.100
for(j in 1:length(n)){
      alphaWilcoxAlphaPre1N[j]=as.numeric(tbl_df(results2[[1]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaRankSumBFAlphaPre1N[j]=as.numeric(tbl_df(results2[[1]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# alpha = 0.05
for(j in 1:length(n)){
      alphaWilcoxAlphaPre05N[j]=as.numeric(tbl_df(results2[[2]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaRankSumBFAlphaPre05N[j]=as.numeric(tbl_df(results2[[2]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# alpha = 0.01
for(j in 1:length(n)){
      alphaWilcoxAlphaPre01N[j]=as.numeric(tbl_df(results2[[3]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaRankSumBFAlphaPre01N[j]=as.numeric(tbl_df(results2[[3]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

# no pretest
for(j in 1:length(n)){
      alphaWilcoxAlphaPreNoPreN[j]=as.numeric(tbl_df(results2[[4]][[1]][,j]) %>% filter(value < 0.05) %>% summarise(length(value))/s)
}
for(j in 1:length(n)){
      alphaRankSumBFAlphaPreNoPreN[j]=as.numeric(tbl_df(results2[[4]][[2]][,j]) %>% filter(value >= 3) %>% summarise(length(value))/s)
}

print("alphaWilcoxAlphaPre1N\n")
alphaWilcoxAlphaPre1N
print("alphaRankSumBFAlphaPre1N\n")
alphaRankSumBFAlphaPre1N
print("alphaWilcoxAlphaPre05N\n")
alphaWilcoxAlphaPre05N
print("alphaRankSumBFAlphaPre05N\n")
alphaRankSumBFAlphaPre05N
print("alphaWilcoxAlphaPre01N\n")
alphaWilcoxAlphaPre01N
print("alphaRankSumBFAlphaPre01N\n")
alphaRankSumBFAlphaPre01N
print("alphaWilcoxAlphaPreNoPreN\n")
alphaWilcoxAlphaPreNoPreN
print("alphaRankSumBFAlphaPreNoPreN\n")
alphaRankSumBFAlphaPreNoPreN
