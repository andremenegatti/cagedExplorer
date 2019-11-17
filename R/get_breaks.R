get_breaks <- function(x, n = 9) {

  x <- sort(x)

  worst <- min(x)
  best <- max(x)

  x2 <- x[x != worst & x != best]

  if (!0 %in% x2) x2 <- c(x2, 0)

  positive <- x2[x2 > 0]
  negative <- x2[x2 <= 0]

  positive
  negative

  if (n %% 2 == 0) {
    n2 <- n - 4
    n_positive <- n2 / 2
    n_negative <- n2 / 2
    if (length(positive) > length(n_negative)) {
      n_positive <- n_positive + 1
    } else {
      n_negative <- n_negative + 1
    }
  } else {
    n2 <- n - 3
    n_positive <- n2 / 2
    n_negative <- n2 / 2
  }

  int_positive <- classInt::classIntervals(positive, n = n_positive, style = 'quantile')$brks
  int_negative <- classInt::classIntervals(negative, n = n_negative, style = 'quantile')$brks

  int_negative[1] <- int_negative[1] - 1
  int_positive[length(int_positive)] <- int_positive[length(int_positive)] + 1

  my_breaks <- c(worst, int_negative, int_positive, best)

  my_breaks

}
