#' @title Print results of \code{mie}
#'
#' @description \code{print.mie} prints estimation of the SACE (survivor average causal effect).
#'
#' @param object an object of class \code{mie}.
#' @param ... additional arguments.
#' @method print mie
#' @export
#' @return the input object is returned silently.
#' @author Zhixuan Shao <shaozhixuansh@pku.edu.cn>

print.mie <- function(object, ...) {
    if (!inherits(object, "mie")) stop("Object must be of class 'mie'")

    cat('Call:\n')
    print(object$CALL)
    cat('\n')
    cat("sample size:", object$n, "\n")
    cat("average potential outcomes among control group:", object$mu_0_LL,' ')
    if (object$need.variance) { cat("(s.e. ", sqrt(object$mu_0_LL.var), ")", sep = "") }
    cat('\n')
    cat("average potential outcomes among treatment group:", object$mu_1_LL,' ')
    if (object$need.variance) { cat("(s.e. ", sqrt(object$mu_1_LL.var), ")", sep = "") }
    cat('\n')
    cat("SACE (survivor average causal effect):", object$sace,' ')
    if (object$need.variance) { cat("(s.e. ", sqrt(object$sace.var), ")", sep = "") }
    cat('\n')
}