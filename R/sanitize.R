#' Sanitize numbers for tikzDevice
#'
#' Adapted from \code{xtable}; regexps better handle conversion to
#' exponent notation
#' 
#' @param str Character string to sanitize
#' @param type Better choose "latex" for this one, or you will be
#'     disappointed
#' @param math.style.negative Boolean that controls whether negative
#'     signs are typeset as negative signs (TRUE is the right answer
#'     here)
#' @param math.style.exponents Controls whether exponential notation
#'     is set in LaTeX math mode (TRUE) or ensurmath mode
#'     ("ensuremath") or UTF-8 ("UTF8")
#' @return A sanitized string that will look nice and mathy when
#'     typeset with LaTeX
#' @export
sanitize.numbers <- function (str, type, math.style.negative = FALSE, math.style.exponents = FALSE) {
    if (type == "latex") {
        result <- str
        if (math.style.negative) {
            for (i in 1:length(str)) {
                result[i] <- gsub("-", "$-$", result[i], fixed = TRUE)
            }
        }
        if (is.logical(math.style.exponents) && !math.style.exponents) {
        }
        else if (is.logical(math.style.exponents) && math.style.exponents || 
                 math.style.exponents == "$$") {
            for (i in 1:length(str)) {
                result[i] <- gsub("^\\$?(-?)\\$?([0-9.]+)[eE]\\$?(-?)\\+?\\$?0*(\\d+)$", 
                                  "$\\1\\2 \\\\times 10^{\\3\\4}$", result[i])
                result[i] <- gsub("^\\$?(-?)\\$?1 \\\\times 10", 
                                  "$\\110", result[i])
            }
        }
        else if (math.style.exponents == "ensuremath") {
            for (i in 1:length(str)) {
                result[i] <- gsub("([0-9.]+)[eE](-?)\\+?0*(\\d+)", 
                                  "\\\\ensuremath{\\1 \\\\times 10^{\\2\\3}}", 
                                  result[i])
            }
        }
        else if (math.style.exponents == "UTF8" || math.style.exponents == 
                 "UTF-8") {
            for (i in 1:length(str)) {
                if (all(grepl("^\\$?(-?)\\$?([0-9.]+)[eE]\\$?(-?)\\+?\\$?0*(\\d+)$", 
                              result[i]))) {
                    temp <- strsplit(result[i], "eE", result[i])
                    result[i] <- paste0(temp[1], "×10", chartr("-1234567890", 
                                                               "⁻¹²³⁴⁵⁴6⁴7⁴8⁴9⁰", temp[2]))
                }
            }
        }
        return(result)
    }
    else {
        return(str)
    }
}
