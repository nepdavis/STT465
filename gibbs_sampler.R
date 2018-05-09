data <- read.table("gout_new.txt", header = F)

colnames(data) <- c("sex", "race", "age", "serum_urate", "gout")

data$sex <- factor(data$sex, levels = c("M", "F"))
data$race <- as.factor(data$race)

dF <- ifelse(data$sex == "F", 1, 0)
dW <- ifelse(data$race == "W", 1, 0)

varB <- 1000
b0 <- 0
varE <- 1.8

X <- cbind(1, dF, dW, data$age)
y <- data$serum_urate

nIter <- 10000


# Objects to store samples
p <- ncol(X)
B <- matrix(nrow = nIter, ncol = p)

SSx <- colSums(X^2)

B[1,] <- 0
B[1,1] <- mean(y)

for(i in 2:nIter){
    
    for(j in 1:p){
        
        C <- SSx[j] / varE + 1/varB
        
        yTilde <- y - X[,-j] %*% B[-j]
        
        rhs <- sum(X[,j] * yTilde) / varE + b0/varB
        
        condMean <- rhs/C
        
        condVar <- 1 / C
        
        B[i,j] <- rnorm(n = 1, mean = condMean, sd = sqrt(condVar))
    }
}



