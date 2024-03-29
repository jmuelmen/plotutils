#' Generally useful helper functions for plotting
#'
#' Enhancements for ggplotting in two categories: tools for preparing
#' data frames for plotting, and tools for making plots more appealing
#'
#' Three \code{scale_*} functions are provided:
#' \itemize{
#' \item \code{\link{scale_x_geo}}
#' \item scale_x_geo_zonmean
#' \item scale_y_geo
#' }
#' #' These functions return axes with degree labels that can be added to
#' lat/lon (or lat) \code{\link{ggplot}}s
#' 
#' \code{\link{bin}} allows you to bin a variable in a data frame
#' \code{\link{dplyr}}-style (i.e., using expressions that are lazily
#' evaluated with the data frame as the environment).  Use this
#' function to prepare your data frame for a grouped
#' \code{\link{summarize}} for a one- or two-dimensional
#' \code{\link{ggplot}}.
#'
#' \code{\link{geom_world_polygon}} returns a Natural Earth world map
#' as a \code{\link{geom_polygon}} that can be added to a geographical
#' \code{\link{ggplot}}
"_PACKAGE"
