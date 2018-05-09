DATA=read.table('gout.txt',header=F)
colnames(DATA)=c('sex','race','age','serum_urate','gout')

DATA$sex=factor(DATA$sex,levels=c('M','F'))
DATA$race=as.factor(DATA$race) 

dF=ifelse(DATA$sex=='F',1,0) # a dummy variable for female
dW=ifelse(DATA$race=='W',1,0) # a dummy variable for male

# Incidence matrix for intercept and effects of sex, race and age
X=cbind(1,dF,dW,DATA$age) 
head(X)
y=DATA$serum_urate

## Hyper-parameters
varB=1000
b0=0
df0 <- 4
S0 <- var(y) * .8*(df0-2)
n <- nrow(X)

## Number of iterations
nIter=10000

## Objects to store samples
p=ncol(X)
B=matrix(nrow=nIter,ncol=p,0)
varE <- rep(NA, nIter)
SSx=colSums(X^2)

## Initialize
B[1,]=0
B[1,1]=mean(y)
b=B[1,]
varE[1] <- var(y)

for(i in 2:nIter){
    for(j in 1:p){
        C=SSx[j]/varE[i-1]+1/varB
        yStar=y-X[,-j]%*%b[-j]
        rhs=sum(X[,j]*yStar)/varE[i-1]  + b0/varB
        condMean=rhs/C
        condVar=1/C
        b[j]=rnorm(n=1,mean=condMean,sd=sqrt(condVar))
        B[i,j]=b[j]  
    }
    
    eHat <- y - X %*% b
    RSS <- sum(eHat^2)
    
    DF <- n + df0
    S <- RSS + S0
    
    varE[i] <- S / rchisq(df = DF, n = 1)
    
    print(i)
}


plot(varE)
