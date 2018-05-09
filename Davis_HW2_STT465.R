y_a <- c(12, 9, 12, 14, 13, 13, 15, 8, 15, 6)
y_b <- c(11, 11, 10, 9, 9, 8, 7, 10, 6, 8, 8, 9, 7)

A_prior <- c(120, 10)
B_prior <- c(12, 1)

N <- seq(1,20,.001)

posterior_a <- dgamma(x = N, shape = A_prior[1] + sum(y_a), rate = A_prior[2] + length(y_a))

posterior_b <- dgamma(x = N, shape = B_prior[1] + sum(y_b), rate = B_prior[2] + length(y_b))

conf_a <- qgamma( c(.025, .975), A_prior[1] + sum(y_a), rate = A_prior[2] + length(y_a))

conf_b <- qgamma( c(.025, .975), B_prior[1] + sum(y_b), rate = B_prior[2] + length(y_b))

mean_a <- (A_prior[1] + sum(y_a)) / (A_prior[2] + length(y_a))

mean_b <- (B_prior[1] + sum(y_b)) / (B_prior[2] + length(y_b))

var_a <- (A_prior[1] + sum(y_a)) / (A_prior[2] + length(y_a))^2

var_b <- (B_prior[1] + sum(y_b)) / (B_prior[2] + length(y_b))^2

####### Part B

n0 <- seq(1,50)





######################################################## 3.7






