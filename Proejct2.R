## Problem 1 -----------------------------------------------------------------------------

# X ~ Bernoulli(1, p) using the rbinom function.
N <- 10^5
X <- rbinom(n = N,
            size = 1,
            prob = 0.5)

# Sampling using the inverse transform method.
bernoulliInverse <- function() {
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
B<-bernoulliInverse() #random variable generated using the inverse transform method  
# Compare samples using histograms for both samples.
par(mfrow=c(2,1))
hist(X)
hist(B)

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
E<-expRandVar(10^5,2) #E is an exponentially distributed random variable using the inverse transform method
plot(hist(E),xlim=c(0.0,8.0),ylim=c(0,50000),main = 'Histogram of the Random Variable using Inverse transform method vs. the density of the exponential distribution')#histogram of the exponentially distributed random variable using the inverse transform method
par(new=TRUE, ann=FALSE) #plot superposition
plot(density(rexp(10^5,rate=2)),ann=FALSE, xlim=c(0.0,8.0),axes=FALSE)#plotting the density of the exponential distribution using the Rlab function rexp

## Problem 3 -----------------------------------------------------------------------------
NormDist <- function(n){
  # n : number of points
  oneZeroVec <- runif(n,min=0,max=1)
  angle <- (oneZeroVec*pi)
  radius <- sqrt(rexp(n,rate=1/2))
  X <- radius * cos(angle)
  return(X)
}
hist(NormDist(10^5),xlim=c(-4.0,4.0), main = 'Histogram of the Random Variable X1 vs. the density of Normal(0,1)') #plotting the 
par(new=TRUE) #plot superposition 
plot(density(rnorm(10^5, mean=0, sd=1)),xlim=c(-4.0,4.0),axes=FALSE,ann=FALSE) #plotting the probability density function of the normal distribution with mean=0 and variance=1


### Problem 4 -----------------------------------------------------------------------------
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
k<-10^5 #number of points
k2<-k-2
k1<-k-1
#plot the first sequence
s<-LCGextended(16807,0,(2^31)-1,k, 5) #pseudo random numbers generation using the first setting 
scatter3D(s[1:k2],s[2:k1],s[3:k])
library(plot3Drgl)
plotrgl()
#plot the second sequence
s2<-LCGextended(65539,0,(2^31),k, 5)
scatter3D(s2[1:k2],s2[2:k1],s2[3:k])  #pseudo random numbers generation using the second setting 
plotrgl()
#we notice that the first parameters result in random numbers , while for the second set of parameters, we notice 
#the distribution of the generated numbers on a fixed number of planes in the space, which means that the generated numbers are not that random
#according to this, the first setting is better suited for random numbers generation
