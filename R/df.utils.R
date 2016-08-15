## Function to bin variables in a data frame

#' Bin a variable in a data frame
#' ## The data frame is expanded by the bin center and bin width.  "var"
## is lazily evaluated.  "bins" can be a single number, in which case
## pretty() is used to get that many bins spanning the range of "var"
#' @param df Data frame.  Evaluation occurs with \code{df} as the
#'     environment.
#' @param var Expression.  The expression is lazily evaluated.  The
#'     result of the evaluation is then binned.
#' @param bins A single number, a vector of numbers, or an expression.
#'     If a single number, \code{pretty(n = bins)} is used on the
#'     result of evaluating \code{var} to determine bins; if a vector,
#'     they are taken as bin edges; if an expression, it is lazily
#'     evaluated and the result used as bin edges.
#' @return A data frame that contains \code{df} and two new variables,
#'     \code{x_bin} indicating which bin the observation falls into
#'     and \code{x_width} indicating the width of that bin. 
#' @export
bin <- function(df, var, bins) {
    ## library(lazyeval)
    x <- lazyeval::lazy(var)
    bins <- lazyeval::lazy(bins)
    bin_(df, x, as.character(x$expr), bins)
}

bin_ <- function(df, var, varname, bins) {
    ## if (require(lazyeval))
    ##     x <- lazyeval::lazy_eval(var, df)
    ## else x <- var
    x <- lazyeval::lazy_eval(var, df)
    
    bins <- lazyeval::lazy_eval(bins, df)
    
    if (length(bins) == 1) {
        bins <- pretty(range(x, na.rm = TRUE), bins)
    }
    ## min.x <- min(x)
    ## if (min.x == min(bins)) {
    ##     ## adjust so nobody falls off the edge
    ##     x[which.min(x)] <- min.x + diff(bins)[1] * 1e-3 
    ## }
    x.bin <- as.integer(cut(x, bins))
    x.width <- diff(bins)[x.bin]
    x.pos <- stats::filter(bins, c(0.5, 0.5))[x.bin]
    xx <- data.frame(x.width, x.pos)
    
    names(xx) <- sprintf(c("%s_width", "%s_bin"), varname)
    cbind(df, xx)
}
