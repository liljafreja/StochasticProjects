## The Inverse Transform Method

# The probability integral transform states that if X is a continuous random variable with
# cumulative distribution function Fx, then the random varible Y = Fx(X) has a uniform distribution on [0,1].
# If Y has a uniform distribution on [0,1] and if X has a cumulative distribution Fx,
# then the random variable Fx^(-1)(Y) has the same distribution as X.

# Given a continuous uniform varialbe U in [0,1] and an invertible cumulative distribution function Fx,
# the random variable X = Fx^(-1) (U) has distribution Fx.
#       1. Generate a random number u from the standard uniform distribution in [0,1]
#       2. Compute the value x such that Fx(x) = u.
#       3. Take x to be the random num drawn from the distribution Fx.

# Example: suppose X is uniformly distributed on [0,1], then the CDF is given by
# F(x) = 0 : x<0
#        x : 0<=x<1
#        1 : x>=1

# The method is very simple, so I'll describe it in simple words. First, take cumulative distribution function FX
# of some distribution that you want to sample from. The function takes as input some value x and tells you what is the probability of obtaining X≤x
# So FX(x)=Pr(X≤x)=p inverse of such function function, F−1X would take p as input and return x. Notice that p's are uniformly distributed -- this could be used for sampling from any FX
# if you know F−1X

## a) Suppose that X takes values 0 and 1, with equal probability. then the CDF is given by
# F(x) = 0    : x<0
#        1/2  : 0<=x<1
#        1    : x>=1

## Problem 1 -----------------------------------------------------------------------------
# Let X ∼ Bernoulli(1, p) for some p ∈ [0, 1].
# Sampling using the rbinom(·,1,p) method.
N <- 10^5
X <- rbinom(n = N,
            size = 1,
            prob = 0.5)

# Sampling using the inverse transform method.
inverseTransformMethod <- function() {
  # n : number of samples
  # p : probability of getting 0s
  n <- 10^5
  p <- 0.5
  # returns a pseudorandom value drawn from the standard uniform distribution on the open interval (0,1)
  oneZeroVec <- runif(n,min=0,max=1)
  cumDistFunc <- plot.stepfun(oneZeroVec)
  #is.list(cumDistFunc)
  s <- c()
  
  for (i in 1:length(cumDistFunc$t)) {
    if (cumDistFunc$t[i] < p){
      s[i] <- 0
    } else { 
      s <- append(s,1) 
    }
  }
#  cumDistFunc
#  sample
  return(s)
}

# Compare samples using histograms for both samples.
par(mfrow=c(2,1))
hist(X)
hist(inverseTransformMethod)

## Problem 2 -----------------------------------------------------------------------------
expRandVar <- function(n,lambda){
  oneZeroVec <- runif(n,min=0,max=1)
  s <- c()
  
  for(i in 1:n){
    x <- (-1/lambda)*log2(1-oneZeroVec[i])
    s <- append(s,x);
  }
  return(s)
}

hist(expRandVar(10^5,2))
par(new=TRUE) #plot superposition
plot(density(expRandVar(10^5,2)))

## Problem 3 -----------------------------------------------------------------------------
#Now simulate a normally distributed random variable in the following way: Simulate a random
#vector X = (X1,X2) in R2 using polar coordinates, where the radius is the square root of an
#exponentially distributed random variable with parameter 1 , and its angle is uniformly distributed 2
#on [0, 2π]. Then X1 ∼ Normal(0, 1). Compare a corresponding histogram of your simulation with the density of Normal(0, 1) 
# in a single plot (choose suitable scalings).

circleUniDist <- function(n){
  # n : number of points
  oneZeroVec <- runif(n,min=0,max=1)
  angle <- (oneZeroVec*1000)%%360
  radius <- sqrt(exp(1/2))
  s <- c(radius, angle)
  return(s)
}
hist(circleUniDist(10^5))
hist(rnorm(10^5, mean=0, sd=1))
par(new=TRUE) #plot superposition
plot(density(rnorm(10^5, mean=0, sd=1)))


### Problem 4 -----------------------------------------------------------------------------
# The linear congruential generator is a pseudo random number generator.
# It is not truely random but deterministic in nature, however it attemps to mimic
# the behaviour of true randomness. 
# The LGC works by taking an initial value, multiplying it by a number and adding another number to it,
# then reducing modular another vlaue. 

# Xi = (a·Xi−1+b) mod m, a,b,m∈N
# m > 1 is the modulus
# a {1,2,...,m-1} is the multiplier
# b {0,1,...,m-1} is the constant
# X0 {0,1,...,m-1} is the seed aka initial value
# NOTE: In a run of an arbitrary length, the number of distinct values along the Xi-1 cannot exceed m !

LCG <- function(a,b,m,n,SEED){
  X <- c() # empty vector to contain results
  Xi <- SEED # initialisation of seed
  for (j in 1:n) {
    Xi <- (a*Xi + b) %% m
    X[j] <- Xi
  }
  return(X)
}

## Problem 5 -----------------------------------------------------------------------------
#  Function calls:
#       > LCG(15,8,5,3,0)
#       [1] 3 2 5 4 7 6 1 0 3 2 5 4 7 6 1
#       > LCG(15,8,5,3,1)
#       [1] 0 3 2 5 4 7 6 1 0 3 2 5 4 7 6
#       > LCG(15,8,5,3,2)
#       [1] 5 4 7 6 1 0 3 2 5 4 7 6 1 0 3

# The possible states are 6,1,0,3,2,5,4,7 of period 8 which repeat depending on the initial value 
# in the same order - which makes the output not completely random. The generator covers all the numbers
# in the interval 0 - 8 which makes it a full period.
# In  order to obtain the maximal period, three condition must be satisfied:
#   1. a-1 is divisible by all the prime factors of m - pick a-1 and m as multiples of 2.
#   2. if m is a multiple of 4, so is a-1 a multiple of 4.
#   3. The modulus m and the constant c are coprime.

# c)
LCGextended <- function(a,b,m,n,SEED){
  X <- c() # empty vector to contain results
  Xi <- SEED # initialisation of seed
  for (j in 1:n) {
    Xi <- (a*Xi + b) %% m
    X[j] <- Xi
  }
  X <- X/m # dividing the vector of the results by m will produce a vector [0,1[
  return(X)
}

library(rgl)
k<-10^3
k2<-k-2
k1<-k-1
#plot the first sequence
s<-LCGextended(16807,0,(2^31)-1,k, 5)
scatter3D(s[1:k2],s[2:k1],s[3:k])
library("plot3Drgl")
plotrgl()
#plot the second sequence
s2<-LCGextended(65539,0,(2^31),k, 5)
scatter3D(s2[1:k2],s2[2:k1],s2[3:k])
plotrgl()
#I don't see the difference