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

#' Interpolate lon/lat data frame onto another lon/lat grid
#' 
#' Arguments are two data frames that represent (data on) a reular
#' lon/lat grid, i.e., that are of the form \code{df.src <- mutate(expand.grid(lon = lon, lat = lat), ...)}
#' 
#' @param df.src Data frame.  Specifies the data to be interpolated.
#'     Must contain the variables \code{lon} and \code{lat}.
#' @param df.dest Data frame.  Specifies the lon/lat grid to be used
#'     for interpolation.  Must contain the variables \code{lon} and
#'     \code{lat}.
#' @param fill Numeric.  Fill NA rows with this value, as NA is not
#'     allowed in the interpolation functions.
#' @return A data frame that contains the data from df.src
#'     interpolated onto the df.dest grid.
#' @export
remap <- function(df.src, df.dest, fill = 0) {
    lon.src <- unique(df.src$lon)
    lat.src <- unique(df.src$lat)
    df.dest <- expand.grid(lon = unique(df.dest$lon),
                           lat = unique(df.dest$lat)) %>%
        dplyr::arrange(lon, lat)

    if (!requireNamespace("akima"))
        return(NULL)

    df.template <- expand.grid(lon = unique(df.src$lon),
                               lat = unique(df.src$lat)) %>%
        dplyr::arrange(lon, lat)
    
    df.src %>%
        dplyr::full_join(df.template, by = c("lon", "lat")) %>%
        dplyr::arrange(lon, lat) %>% 
        tidyr::gather(plotutils_remap_variables_key,
                      plotutils_remap_variables_value,
                      -c(lon, lat)) %>%
        dplyr::mutate(plotutils_remap_variables_value =
                          replace(plotutils_remap_variables_value,
                                  is.na(plotutils_remap_variables_value),
                                  fill)) %>%
        plyr::ddply(~ plotutils_remap_variables_key, function(x) {
            ## if (any(is.na(x$plotutils_remap_variables_value)))
            ##     return(NULL)
            print(x$plotutils_remap_variables_key[1])
            akima::interpp(df.template$lon, df.template$lat,
                           matrix(x$plotutils_remap_variables_value,
                                  length(lon.src), length(lat.src),
                                  byrow = FALSE),
                           df.dest$lon, df.dest$lat, linear = TRUE) %>%
                transform() %>%
                dplyr::rename(lon = x,
                              lat = y,
                              plotutils_remap_variables_value = z)
        }) %>%
        tidyr::spread(plotutils_remap_variables_key,
                      plotutils_remap_variables_value)
}

#' Interpolate "unstructured" data frame onto lon/lat grid
#' 
#' Arguments are two data frames.  The source is on an "unstructured"
#' grid, i.e., one not rectangular in lon/lat space.  The destination
#' is a lon/lat grid, i.e., of the form \code{df.src <-
#' mutate(expand.grid(lon = lon, lat = lat), ...)}
#' 
#' @param df.src Data frame.  Specifies the data to be interpolated.
#'     Must contain the variables \code{lon} and \code{lat}.
#' @param df.dest Data frame.  Specifies the lon/lat grid to be used
#'     for interpolation.  Must contain the variables \code{lon} and
#'     \code{lat}.
#' @param fill Numeric.  Fill NA rows with this value, as NA is not
#'     allowed in the interpolation functions.
#' @return A data frame that contains the data from df.src
#'     interpolated onto the df.dest grid.
#' @export
remap_unstructured <- function(df.src, df.dest, fill = 0) {
    lon.src <- unique(df.src$lon)
    lat.src <- unique(df.src$lat)
    df.dest <- expand.grid(lon = unique(df.dest$lon),
                           lat = unique(df.dest$lat)) %>%
        dplyr::arrange(lon, lat)

    if (!requireNamespace("akima"))
        return(NULL)

    df.src %>%
        dplyr::arrange(lon, lat) %>% 
        tidyr::gather(plotutils_remap_variables_key,
                      plotutils_remap_variables_value,
                      -c(lon, lat)) %>%
        dplyr::mutate(plotutils_remap_variables_value =
                          replace(plotutils_remap_variables_value,
                                  is.na(plotutils_remap_variables_value),
                                  fill)) %>%
        plyr::ddply(~ plotutils_remap_variables_key, function(x) {
            ## if (any(is.na(x$plotutils_remap_variables_value)))
            ##     return(NULL)
            print(x$plotutils_remap_variables_key[1])
            akima::interpp(x$lon, x$lat,
                           x$plotutils_remap_variables_value,
                           df.dest$lon, df.dest$lat, linear = TRUE, duplicate = "mean") %>%
                transform() %>%
                dplyr::rename(lon = x,
                              lat = y,
                              plotutils_remap_variables_value = z)
        }) %>%
        tidyr::spread(plotutils_remap_variables_key,
                      plotutils_remap_variables_value)
}

#' Great-circle distances
#'
#' Calculate great-circle distances based on the haversin formula
#'
#' @param lon1 Vector of longitudes (in degrees) 
#' @param lon2 Vector of longitudes (in degrees) 
#' @param lat1 Vector of latitudes (in degrees) 
#' @param lat2 Vector of latitudes (in degrees)
#' @return Vector of distances (in km) between points
#'
#' @export
#'
#' @examples
#' 
dist.gc <- function(lon1, lon2, lat1, lat2) {
    haversin <- function(theta) sin(0.5 * theta)^2
    re <- 6371

    lon1 <- lon1 / 180 * pi
    lon2 <- lon2 / 180 * pi
    lat1 <- lat1 / 180 * pi
    lat2 <- lat2 / 180 * pi

    theta <- sqrt(haversin(lat1 - lat2) +
                  cos(lat1) * cos(lat2) * haversin(lon1 - lon2))
    dist <- 2 * re * asin(pmin(pmax(theta, 0), 1))
    dist
}

#' Read a list of variables from a NetCDF file and return them as a data.frame
#'
#' @param nc ncdf4 NetCDF object
#' @param vars Vector of variable names
#' @param spread Spread variables into columns?
#' @param na.rm Remove NA input rows?
#' @param mask (Optional) logical vector; FALSE means discard the corresponding data value
#' @return data.frame of NetCDF contents as vectors with variable
#'     names as column names
#'
#' @export
#'
#' @examples
#' 
nc.to.df <- function(nc, vars, spread = TRUE, na.rm = FALSE, mask) {
    if (missing(mask)) {
        mask <- NULL
    }
    df <- plyr::ldply(vars, function(var) {
        gc()
        ## get values
        x <- ncdf4::ncvar_get(nc, var)
        ## get dimensions
        dim.names <- plyr::laply(nc$var[[var]]$dim, function(i) {
            i$name
        })
        ## str(dim.names)
        dim.vals <- plyr::llply(nc$var[[var]]$dim, function(i) {
            i$vals
        })
        names(dim.vals) <- dim.names
        ## str(dim.vals)
        ## expand dimensions, attach values, tag by var name
        df <- dplyr::mutate(expand.grid(dim.vals, KEEP.OUT.ATTRS = FALSE),
                      x = as.vector(x),
                      name = var)
        ## if requested, mask input
        if (!is.null(mask)) {
            df <- df[mask,]
        }
        ## if requested, remove NA inputs
        if (na.rm) {
            df %<>% dplyr::filter(!is.na(x))
        } 
        return(df)
    })
    ## spread the values into the named columns
    if (spread) {
        df %<>% tidyr::pivot_wider(names_from = name, values_from = x)
    } 
    return(df)
}
