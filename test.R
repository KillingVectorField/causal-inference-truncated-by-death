require(TruncCause)
? mie
#test
n <- 200
set.seed(1)
X1 <- rnorm(n)
X2 <- rnorm(n)
X3 <- rnorm(n)
X4 <- rnorm(n)
Z <- rbinom(n, 1, 0.5)
Y <- rnorm(n,X1+X2+X3+X4+Z)
A <- rnorm(n)
#S <- rbinom(n, 1, 0.5)
#S <- rbinom(n,1,exp(A) / (1 + exp(A)))
S <- rbinom(n, 1, ifelse(Z, exp(A) / (1 + exp(A)), (exp(A) / (1 + exp(A))) ^ 2))

time = proc.time()
t <- mie(Z, S, Y, cbind(X1, X2, X3, X4), A, optim.method = "BFGS", need.variance = TRUE)
proc.time() - time

boot.ci(t, print.progress = FALSE)$ci
t$sace.var
t$beta
t$gamma
t$sace
t$mu_0_LL
t$mu_0_LL.var
t$mu_1_LL
t$mu_1_LL.var

t$beta_gamma.convergence
t$sace

print(t)
summary(t)