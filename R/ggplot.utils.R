######################################################################
## ggplot helper functions for geographical plots
######################################################################

#' Geographic scales (with degree labels)
#' @param facet Boolean.  Will the plots be faceted?  On faceted
#'     plots, the rightmost degree label is omitted to prevent
#'     overplotting.
#' @return A \code{scale_x/y_continuous} that can be added to a \code{ggplot}
#' @name scale_geo
NULL

#' @describeIn scale_geo longitude (x) axis
#' @export
scale_x_geo <- function(facet = FALSE) {
    ggplot2::scale_x_continuous("", expand = c(0,0), 
                               breaks = if (facet) seq(-180, 120, 60) else seq(-180, 180, 60),
                               labels = if (facet) c(bquote(180*degree),
                                                     as.expression(sapply(seq(-120, -60, by = 60), function(x) { x <- -x; bquote(.(x)*degree * W) } )),
                                                     0,
                                                     as.expression(sapply(seq(60, 120, by = 60), function(x) { bquote(.(x)*degree * E) } )))
                                        else c(bquote(180*degree),
                                               as.expression(sapply(seq(-120, -60, by = 60), function(x) { x <- -x; bquote(.(x)*degree * W) } )),
                                               0,
                                               as.expression(sapply(seq(60, 120, by = 60), function(x) { bquote(.(x)*degree * E) } )),
                                               bquote(180*degree)))
}

#' @describeIn scale_geo latitude (y) axis
#' @export
scale_y_geo <- function() {
    ggplot2::scale_y_continuous("", expand = c(0,0), breaks = -2:2 * 30, 
                               labels = c(as.expression(sapply(c(-60, -30), function(x) { x <- -x; bquote(.(x)*degree * S) } )),
                                          0,
                                          as.expression(sapply(c(30, 60), function(x) { bquote(.(x)*degree * N) } )))) 
}

#' @describeIn scale_geo latitude (x) axis for zonal-mean plots
#' @export
scale_x_geo_zonmean <- function() {
    ggplot2::scale_x_continuous("", expand = c(0,0), breaks = -1:1 * 60, 
                               labels = c(as.expression(sapply(c(-60), function(x) { x <- -x; bquote(.(x)*degree * S) } )),
                                          0,
                                          as.expression(sapply(c(60), function(x) { bquote(.(x)*degree * N) } ))))
}
