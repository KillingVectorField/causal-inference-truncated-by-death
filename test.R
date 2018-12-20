require(mieSACE)
? mie
#test
n <- 500
set.seed(1)
X1 <- rnorm(n)
X2 <- rnorm(n)
X3 <- rnorm(n)
X4 <- rnorm(n)
Z <- rbinom(n, 1, 0.5)
Y <- rnorm(n, X1 + X2 + X3 + X4 + Z)
A <- cbind(rnorm(n), rnorm(n))
#S <- rbinom(n, 1, 0.5)
#S <- rbinom(n,1,exp(A) / (1 + exp(A)))
beta <- rep(1, ncol(A))
S <- rbinom(n, 1, ifelse(Z, exp(A %*% beta) / (1 + exp(A %*% beta)), exp(2 * A %*% beta) / (1 + exp(2 * A %*% beta))))

##naive OLS
summary(lm(Y ~ Z + X1 + X2 + X3 + X4, subset = as.logical(S)))

time = proc.time()
t <- mie(Z, S, Y, cbind(X1, X2, X3, X4), A, optim.method = "BFGS", need.variance = TRUE)
proc.time() - time


#boot.result <- boot.ci(t, print.progress = FALSE)
#boot.result$ci
#boot.result$boot.sd
sqrt(t$sace.var)
#t$beta
#t$gamma
t$sace
t$mu_0_LL
sqrt(t$mu_0_LL.var)
t$mu_1_LL
sqrt(t$mu_1_LL.var)

t$beta_gamma.convergence

selectSV(Z, S, cbind(X1, X2, X3, X4), A)
