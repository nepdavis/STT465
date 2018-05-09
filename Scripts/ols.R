data <- read.table("gout.txt", header = T)

data$sex <- as.numeric(data$sex == "F")
data$gout <- as.factor(data$gout == "Y")

model <- lm(data$serum_urate ~ data$sex)
summary(model)

X <- cbind(1, data$sex)
Y <- data$serum_urate

XY <- t(X) %*% Y

XtX <- t(X) %*% X

solve(XtX, XY)

bHat <- solve(XtX, XY)  # Same as in model summary
eHat <- Y - X %*% bHat

varEHat <- sum(eHat^2/(nrow(data)-2))

XXInv <- solve(XtX)

SE <- sqrt(diag(XXInv)*varEHat)  # Same as in model summary



### New model

new_model <- lm(serum_urate ~ sex + race + age, data = data)
summary(new_model)

X <- model.matrix(~sex+race+age, data = data)
Y <- data$serum_urate

XY <- t(X) %*% Y

XtX <- t(X) %*% X

solve(XtX, XY)

bHat <- solve(XtX, XY)  # Same as in model summary
eHat <- Y - X %*% bHat

varEHat <- sum(eHat^2/(nrow(data)-2))

XXInv <- solve(XtX)

SE <- sqrt(diag(XXInv)*varEHat)  # Same as in model summary
