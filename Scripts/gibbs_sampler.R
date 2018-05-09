# Data

trueMu <- 10
trueVar <- 1.5
n <- 23
y <- rnorm(n, trueMu, sqrt(trueVar))



# Prior hyper-parameters

v <- 3
s <- 6

mu0 <- 0
varMu <- 10000  # Flat prior



# Parameters of the sampler

nIter <- 10000 # 10000 iterations



# Storage for the samples

mu <- rep(NA, nIter)
varY <- rep(NA, nIter)



# Initialize the mean and variance

meanY <- mean(y)
mu[1] <- meanY


DF <- v + n
RSS <- sum((y - mu[1])^2)
tmpS <- s + RSS
varY[1] <- tmpS / rchisq(n = 1, df = DF)


# Gibbs Sampler

for(i in 2:nIter){
    
    # Sampling mu given data and variance
    
    condVar_ <- 1 / (n / varY[i-1] + 1/varMu)
    
    w <- (n/varY[i-1]) * condVar_
    
    condMean_ <- w * meanY + (1 - w) * mu0
    
    mu[i] <- rnorm(n = 1, mean = condMean_, sd = sqrt(condVar_))
    
    
    # Sampling variance given data and mu
    
    RSS <- sum((y - mu[i])^2)
    
    tmpS <- s + RSS
    
    varY[i] <- tmpS / rchisq(n = 1, df = DF)
}

plot(mu)







