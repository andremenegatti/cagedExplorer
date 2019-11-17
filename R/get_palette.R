get_palette <- function(map_breaks) {

  reds_full <- c("#67001F", "#B2182B", "#D6604D",
                 "#F4A582", "#FDDBC7", "#F7F7F7",
                 "#D1E5F0", "#92C5DE", "#4393C3",
                 "#2166AC", "#053061")

  blues_full <- c("#F7FBFF", "#DEEBF7", "#C6DBEF",
                  "#9ECAE1", "#6BAED6", "#4292C6",
                  "#2171B5", "#08519C", "#08306B")

  n_negative <- sum(map_breaks < 0)
  n_positive <- sum(map_breaks > 0)

  palette_saldo_reg_gov <- c(reds_full[2:(1 + n_negative)],
                             blues_full[2:(n_positive - 1)],
                             blues_full[n_positive + 2],
                             blues_full[n_positive + 4])

  palette_saldo_reg_gov

}
