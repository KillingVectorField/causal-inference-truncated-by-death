#' @title Select the substitution variable whose impact on survival is the most significant.
#'
#' @description Provided with a bunch of substitution variables that are all a  priori believed to satisfy the assumptions, i.e. exclusion restriction and substitution relavance, \code{selctSV} chooses the one that most significantly impact survival(\eqn{S}). \eqn{A} whose coefficent has the smallest P-value (against null) will be chosen.
#'
#' @note 
#' Outcome \code{Y} is not needed here. See \link[TruncCause]{mie}.for the meaning of \code{Z}, \code{S}, \code{X}, \code{A}
#'
#' @param Z a logical vector. Exposure indicator.
#' @param S a logical vector. Survival indicator.
#' @param X an optional numeric matrix or vector. Baseline covariates. 
#' @param A.candidates a numeric matrix. Each column represents a possible substitution variable.
#' @param subset an optional vector specifying a subset of obervations to be used.
#' @param optim.method The method to be used for maximum likelihood optimization. See \link[stats]{optim}.
#' @param max.step integer. Maximum iterating steps of maximum likelihood optimization.
#' @export
#' @return a list with 2 elements:
#' \item{selected.A}{column name of the selected substitution variable.} 
#' \item{P.values}{P-values (againt null hypothesis) of every substitution variable's coefficient.}
#' @author Zhixuan Shao <shaozhixuansh@pku.edu.cn>

selectSV <- function(Z, S, X, A.candidates, subset, optim.method = "BFGS", max.step = 1000) {
    ### 0. Checks arguments ###################################

    ## 0.1 Checks data type ###################################

    if (!is.vector(Z)) stop("Z should be a vector.")
    Z <- as.logical(Z)
    if (!is.vector(S)) stop("S should be a vector.")
    S <- as.logical(S)
    if (missing(X)) X <- NULL
    else {
        X <- as.matrix(X)
        if (!is.matrix(X)) stop("X must be either a vector or a matrix.")
        }
    if (missing(A.candidates)) stop("A is not provided")
    else {
        if (!is.matrix(A.candidates)) stop("A.candidates must be a matrix.")
        }

    ## 0.2 Length check ###################################

    if (missing(subset)) {
        # check if Z,S,Y,A,X have same length
        n <- length(Z)
        if (length(S) != n) stop("S must have the same length as Z's.")
        if ((!is.null(X)) & (nrow(X) != n)) stop("X must have the same row number as Z's length.")
        if (nrow(A.candidates) != n) stop("A must have the same row number as Z's length.")
        }

    ## 0.3 Subset ##################################

    else {
        # subset is provided
        Z <- Z[subset]
        S <- S[subset]
        A.candidates <- A.candidates[subset,]
        if (!is.null(X)) X <- X[subset,]
        n <- length(Z)
    }

    ## 0.4 Missing values ###############################

    if (sum(is.na(Z))) stop("Z should not have missing values.")
    if (sum(is.na(S))) stop("S should not have missing values.")
    if ((!is.null(X)) & (sum(is.na(X)))) stop("X should not have missing values.")
    if (sum(is.na(A))) stop("A should not have missing values.")

    ### 1. Sets up data ###########################
    A.names <- colnames(A.candidates)
    if (is.null(A.names)) { A.names <- paste("A", c(1:ncol(A)), sep = "") }

    s1 = S == 1;
    s0_z1 = S == 0 & Z == 1
    s1_z0 = S == 1 & Z == 0;
    s0_z0 = S == 0 & Z == 0
    sz = cbind(s1, s0_z1, s1_z0, s0_z0)

    expit <- function(x) exp(x) / (1 + exp(x))

    nLL_beta <- function(beta, gamma, W, sz) {
        ebeta = expit(W %*% beta)
        egamma = expit(W %*% gamma)
        loglike = sum(sz[, 1] * log(ebeta) + sz[, 2] * log(1 - ebeta) + sz[, 3] * log(egamma) + sz[, 4] * log(1 - ebeta * egamma))
        return(loglike)
    }

    beta_gamma.gr <- function(beta, gamma, W, sz) {
        ebeta = as.vector(expit(W %*% beta))
        egamma = as.vector(expit(W %*% gamma))
        loglike.partial.beta <- colSums((sz[, 1] * (1 - ebeta) + sz[, 2] * (-ebeta) + sz[, 4] / (1 - ebeta * egamma) * (-egamma) * (ebeta) * (1 - ebeta)) * W)
        loglike.partial.gamma <- colSums((sz[, 3] * (1 - egamma) + sz[, 4] / (1 - ebeta * egamma) * (-ebeta) * (egamma) * (1 - egamma)) * W)
        return(c(loglike.partial.beta, loglike.partial.gamma))
    }

    P.values <- rep(NA, ncol(A))
    for (i in 1:ncol(A)) {
        A <- A.candidates[, i]
        W <- cbind(rep(1, n), X, A)
        d <- ncol(W)
        opt3 <- optim(c(rep(0, d), rep(0, d)),
                  function(beta_gamma, W, sz) nLL_beta(beta_gamma[1:d], beta_gamma[-(1:d)], W, sz),
                  W = W, sz = sz,
                  gr = function(beta_gamma, W, sz) beta_gamma.gr(beta_gamma[1:d], beta_gamma[-(1:d)], W, sz),
                  method = optim.method,
                  hessian = TRUE,
                  control = list(fnscale = -1, maxit = 2 * max.step))
        if (opt3$convergence != 0) { warning(paste("Optimization of beta and gamma didn't converge in", max.step, "steps !")) }
        beta <- opt3$par[1:d]
        gamma <- opt3$par[-(1:d)]
        require(numDeriv)
        beta_gamma.var <- try(solve(-opt3$hessian))
        if ('try-error' %in% class(beta_gamma.var)) {
            warning(paste("Failed to estimate variance of the", i, "th candidate's coefficient!"))
            P.values[i] <- Inf
            next
        }
        pos_definite <- try(prod(diag(beta_gamma.var) >= 0))
        if ('try-error' %in% class(pos_definite) | is.na(pos_definite) | (!pos_definite)) {
            warning(paste("Failed to estimate variance of the", i, "th candidate's coefficient!"))
            P.values[i] <- Inf
            next
        }
        beta.var <- beta_gamma.var[1:d, 1:d]
        gamma.var <- beta_gamma.var[-(1:d), - (1:d)]
        P.values[i] <- 2 * (1 - max(pnorm(abs(beta[d] / sqrt(beta.var[d, d]))), pnorm(abs(gamma[d] / sqrt(gamma.var[d, d])))))
    }
    return(list(selected.A = A.names[which.min(P.values)], P.values = P.values))
}