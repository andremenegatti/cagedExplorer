#' Classify elements from a vector into groups, according to break points
#'
#' @param x A numeric vector to be sorted into groups
#' @param breaks A numeric vector with the break points
#'
#' @return A numeric vector with the same length of \code{x},
#'
#' @examples
#' x <- c(995.35053, 1307.03364, 275.26068, 1008.82226, 534.79647, 613.39754, 1983.56067, 712.05917, 1369.12803,
#'        369.23409, 547.17646, -421.07642, 704.41723, 187.49952, 1194.94762, 805.22835, 1106.85654, 335.69029,
#'        488.50666, 452.49957, 106.18092, 821.41591, 450.92849, 797.47272, 297.16704, 438.47160, 1018.22918,
#'        700.39850, 289.99789, 196.93818, 844.31660, 891.60562, -45.03063, 961.54748, 109.38660, 686.45020,
#'        796.71201, 249.80338, 522.54733, 385.94572, 353.18030, 188.26056, -301.37340)
#'
#' breaks <- c(-540, -168,  -45, 0, 401.4255, 816.6361, 1650, 2201.2766)
#'
#' group_by_breaks(x, breaks)
group_by_breaks <- function(x, breaks = NA) {
  if (identical(breaks, NA)) {
    breaks <- get_breaks(x)
  }
  x <- round(x, digits = 4)
  y <- rep(NA, length(x))
  for (j in 1:length(x)) {
    for (i in 1:(length(breaks) - 1)) {
      if (x[j] >= breaks[i] & x[j] < breaks[i + 1]) {
        y[j] <- i
      }
    }
  }
  if (sum(is.na(y)) == 1) {
    y[which(is.na(y))] <- length(breaks) - 1
  }
  y
}
