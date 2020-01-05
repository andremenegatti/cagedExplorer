#' Helper function to adjust number formatting
#'
#' @param x A numeric or character vector.
#' @param ... Arguments passed to \code{formatC}.
#' @return A character object.
#'
#' @examples
#' format_number(1000000)
#' format_number(c('15.72', '1,000.42'))
format_number <- function(x, ...) {
  formatC(x, digits = 0, big.mark = '.',
          decimal.mark = ',', format = 'f', ...)
}
