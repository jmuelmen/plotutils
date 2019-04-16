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

using.tizk <- function() any(grepl("tikz", names(dev.cur())))

#' @describeIn scale_geo longitude (x) axis
#' @export
scale_x_geo <- function(facet = FALSE) {
    ggplot2::scale_x_continuous(
        "", expand = c(0,0), 
        breaks = if (!using.tizk()) {
                     if (facet) seq(-180, 120, 60) else seq(-180, 180, 60)
                 } else {
                     seq(-120, 120, 120)
                 },
        labels = if (!using.tizk()) {
                     if (facet) {
                         c(bquote(180*degree),
                           as.expression(sapply(seq(-120, -60, by = 60), function(x) { x <- -x; bquote(.(x)*degree * W) } )),
                           0,
                           as.expression(sapply(seq(60, 120, by = 60), function(x) { bquote(.(x)*degree * E) } )))
                     } else {
                         c(bquote(180*degree),
                           as.expression(sapply(seq(-120, -60, by = 60), function(x) { x <- -x; bquote(.(x)*degree * W) } )),
                           0,
                           as.expression(sapply(seq(60, 120, by = 60), function(x) { bquote(.(x)*degree * E) } )),
                           bquote(180*degree))
                     }
                 } else {
                     c(sapply(c(-120), function(x) { sprintf("$%d\\degree$W", -x) }),
                       0,
                       sapply(c(120), function(x) { sprintf("$%d\\degree$E", x) }))
                 }
    )
}

#' @describeIn scale_geo latitude (y) axis
#' @export
scale_y_geo <- function() {
    ggplot2::scale_y_continuous(
                 "", expand = c(0,0),
                 breaks = if (!using.tizk()) {
                              -2:2 * 30
                          } else {
                              -1:1 * 60
                          },
        labels = if (!using.tizk()) {
                     c(as.expression(sapply(c(-60, -30), function(x) { x <- -x; bquote(.(x)*degree * S) } )),
                       0,
                       as.expression(sapply(c(30, 60), function(x) { bquote(.(x)*degree * N) } )))
                 } else {
                     c(as.expression(sapply(c(-60), function(x) { sprintf("$%d\\degree$S", -x) })),
                       0,
                       as.expression(sapply(c(60), function(x) { sprintf("$%d\\degree$N", x) })))
                 }
    )
}

#' @describeIn scale_geo latitude (x) axis for zonal-mean plots
#' @export
scale_x_geo_zonmean <- function() {
    ggplot2::scale_x_continuous(
        "", expand = c(0,0), breaks = -1:1 * 60, 
        labels = if (!using.tizk()) {
                     c(as.expression(sapply(c(-60), function(x) { x <- -x; bquote(.(x)*degree * S) } )),
                       0,
                       as.expression(sapply(c(60), function(x) { bquote(.(x)*degree * N) } )))
                 } else {
                     c(as.expression(sapply(c(-60), function(x) { sprintf("$%d\\degree$S", -x) })),
                       0,
                       as.expression(sapply(c(60), function(x) { sprintf("$%d\\degree$N", x) })))
                 }
    )
}

#' World map outline 
#'
#' This functions returns the outline of the continents and major
#' islands, in lat/lon projection, WGS84 datum, 1:110M resolution,
#' from Natural Earth, as a \code{\link{geom_polygon}} that can be
#' added to a \code{\link{ggplot}}.
#' 
#' @param col Line color
#' @param fill Fill color
#' @param lwd Line width
#' @param highres Logical.  High resolution (1:10m) or default (1:110m)
#' @param ... Other arguments passed to \code{\link{geom_polygon}}
#' @return A \code{geom_polygon} which can be added to a \code{ggplot}
#'
#' @export
#'
#' @examples
#' expand.grid(lon = -180:180, lat = -90:90) %>%
#'     mutate(fill = cos(pi * lat / 180)) %>%
#'     ggplot(aes(x = lon, y = lat, fill = fill)) +
#'     geom_raster() +
#'     geom_world_polygon() +
#'     scale_x_geo() + scale_y_geo()
geom_world_polygon <- function(col = "black", fill = NA, lwd = 0.5, highres = FALSE, ...) {
    ggplot2::geom_polygon(ggplot2::aes(x=long, y=lat, group=group),
                          data = if (highres) {
                                     rbind(ggplot2::fortify(shp.highres) %>%
                                           dplyr::mutate(id = sprintf("land.%s", id),
                                                         group = sprintf("land.%s", group)),
                                           ggplot2::fortify(shp.highres.lakes) %>%
                                           dplyr::mutate(id = sprintf("lakes.%s", id),
                                                         group = sprintf("lakes.%s", group)))
                                 } else {
                                     rbind(ggplot2::fortify(shp) %>%
                                           dplyr::mutate(id = sprintf("land.%s", id),
                                                         group = sprintf("land.%s", group)),
                                           ggplot2::fortify(shp.lakes) %>%
                                           dplyr::mutate(id = sprintf("lakes.%s", id),
                                                         group = sprintf("lakes.%s", group)))
                                 },
                          col = col, fill = fill, lwd = lwd, ...)
}
