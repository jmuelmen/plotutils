## Function to bin variables in a data frame

#' Bin a variable in a data frame
#' 
#' The data frame is expanded by the bin center and bin width.  "var"
#' is lazily evaluated.  "bins" can be a single number, in which case
#' \code{pretty()} is used to get that many bins spanning the range of
#' "var".  
#' 
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
    diff.bins <- diff(bins)
    ## x.bin <- as.integer(cut(replace(x, x == bins[1], x + 1e-6 * diff.bins[1]),
    ##                         ## make lowest bin edge inclusive
    ##                         bins))
    x.bin <- replace(as.integer(cut(x, bins)),
                     ## make lowest bin edge inclusive
                     x == bins[1], 1)
    x.width <- diff.bins[x.bin]
    x.pos <- stats::filter(bins, c(0.5, 0.5))[x.bin]
    xx <- data.frame(x.width, x.pos)
    
    names(xx) <- sprintf(c("%s_width", "%s_bin"), varname)
    cbind(df, xx)
}

#' @param as_factor (for \code{discretize()}.) Logical or character.
#'     Return discretized values as numeric (\code{as_factor =
#'     FALSE}), or as factor (\code{as_factor = TRUE}) in the style of
#'     \code{cut()}, or as an ordered factor (\code{as_factor =
#'     "ordered"}).
#' @param equal_contents (for \code{discretize()}.) Logical.  If
#'     binning is specified as a number of bins, select bin boundaries
#'     that result in \code{bins} bins of equal contents
#' @return A data frame with "var" replaced by its discretized version
#' @describeIn bin The functions \code{bin()} and \code{discretize()}
#'     differ in their return values: \code{bin()} adds two new
#'     columns, bin center and bin width; \code{discretize()} replaces
#'     "var" with its discretized version, either as numeric, factor,
#'     or ordered factor.
#' @export
discretize <- function(df, var, bins, as_factor = FALSE, equal_contents = FALSE) {
    x <- substitute(var)
    bins <- lazyeval::lazy(bins)
    discretize_(df, x, as.character(x), bins, as_factor, equal_contents)
}

discretize_ <- function(df, var, varname, bins, as_factor, equal_contents) {
    ## if (require(lazyeval))
    ##     x <- lazyeval::lazy_eval(var, df)
    ## else x <- var
    x <- lazyeval::lazy_eval(var, df)
    
    bins <- lazyeval::lazy_eval(bins, df)
    
    if (length(bins) == 1) {
        if (!equal_contents) {
            bins <- pretty(range(x, na.rm = TRUE), bins)
        } else {
            bins <- unique(quantile(x, seq(0, 1, length.out = bins + 1)))
        }
    }
    if (class(as_factor) == "character" ||
        class(as_factor) == "logical" && as_factor) {
        ## return factor a la cut()
        x.bin <- cut(x, bins, ordered_result =
                                  class(as_factor) == "character" &&
                                  as_factor == "ordered")
        x.pos <- replace(x.bin, 
                         ## make lowest bin edge inclusive
                         x == bins[1], levels(x.bin)[1])
    } else {
        ## return midpoint of bins
        x.bin <- replace(as.integer(cut(x, bins)),
                         ## make lowest bin edge inclusive
                         x == bins[1], 1)
        x.pos <- stats::filter(bins, c(0.5, 0.5))[x.bin]
    }
    df[, varname] <- x.pos
    df
}
