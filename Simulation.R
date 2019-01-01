delta1 <- 1
delta2 <- 1
u <- rep(0.5, 3)
set.seed(1000)
beta <- c(4, rep(delta2, 3), 2) / 2
gamma <- c(0, -3 * delta2, delta2, delta2, 2) / 2
expit <- function(x) { exp(x) / (1 + exp(x)) }
size <- 5000
X1 <- sample(c(1, -1), size, prob = c(0.5, 0.5), replace = T)
mu <- c(1, -1)
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
X23 <- MASS::mvrnorm(size, mu, sigma)
X <- cbind(X1, X23)
A <- rbinom(size, 1, expit(X %*% u))
Z <- rbinom(size, 1, expit(delta1 * (X %*% u + A)))
S1 <- rbinom(size, 1, expit(2 + X %*% rep(delta2, 3) / 2 + A))
S0 <- rbinom(size, 1, expit(X %*% c(-3 * delta2, delta2, delta2) / 2 + A))
S0[S1 == 0] <- 0
S <- S1 * Z + S0 * (1 - Z)
Y1LL <- rnorm(size, X %*% u, 0.5 ^ 2)
Y0LL <- rnorm(size, X %*% u - 1, 0.5 ^ 2)
Y1LD <- rnorm(size, X %*% u + 1, 0.5 ^ 2)
Y1 <- S0 * Y1LL + (1 - S0) * Y1LD
Y0 <- Y0LL
Y <- Y1 * Z + Y0 * (1 - Z)
Y[S == 0] <- NA


summary(lm(Y ~ 1 + Z + X + A, subset = as.logical(S)))$coef

require(tbd)
sace <- sace(Z, S, Y, X, A, hessian = FALSE)
sace

#sace.boot <- boot.ci(sace, print.progress = FALSE)
#sace.boot$boot.sd
#sace.boot$boot.ci

A2 <- rnorm(size, X %*% u, 1)
#selectSV(Z, S, X, cbind(A, A2))
sace(Z, S, Y, X, A2, hessian = FALSE)