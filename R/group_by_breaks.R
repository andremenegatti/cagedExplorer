group_by_breaks <- function(x, breaks = NA) {
  if (identical(breaks, NA)) {
    breaks <- get_breaks(x)
  }
  y <- rep(NA, length(x))
  for (j in 1:length(x)) {
    for (i in 1:(length(breaks) - 1)) {
      if (x[j] >= t[i] & x[j] < breaks[i + 1]) {
        y[j] <- i
      }
    }
  }
  if (sum(is.na(y)) == 1) {
    y[which(is.na(y))] <- length(breaks) - 1
  }
  y
}
