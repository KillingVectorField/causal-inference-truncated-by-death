#' @title Print results of \code{sace}
#'
#' @description \code{print.sace} prints estimation of the SACE (survivor average causal effect).
#'
#' @param object an object of class \code{sace}.
#' @param ... additional arguments.
#' @method print sace
#' @export
#' @return the input object is returned silently.
#' @author Zhixuan Shao <shaozhixuansh@pku.edu.cn>

print.sace <- function(object, ...) {
    if (!inherits(object, "sace")) stop("Object must be of class 'sace'")

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