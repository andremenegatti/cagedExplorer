#' Plots a sequential-scale map of Sao Paulo's Government Regions
#'
#' Wrapper around \pkg{tmap} calls to draw a map of Government Regions of the
#' State of Sao Paulo, using a sequential color scheme.
#'
#' @param df A dataframe containing one line per Government Region, and a
#'   variable named \code{regiao_governo}.
#' @param var_plot \code{character} with the name of the variable to be plotted.
#' @param map_title Optional character string with map title.
#' @param map_breaks Optional numeric vector with breaks.
#' @param map_palette Character string with the name of a sequential color
#'   palette or character vector with color HEX codes.
#'
#' @return A \pkg{tmap} plot.
map_region_sequential <-
  function(df, var_plot,
           map_title = str_c('Regiões Gov. de SP - ', var_plot, ' - Acumulado 12 meses'),
           map_breaks = NA, map_palette = 'Blues') {

  if (is.na(map_breaks)) {
    col_index <- which(names(df) == var_plot)

    fill_variable <- df[, col_index] %>%
      as.data.frame() %>%
      select(-geometry) %>%
      unlist()

    best <- max(fill_variable)
    others <- fill_variable[fill_variable != best]

    others_breaks <- classInt::classIntervals(others, style = 'quantile', n = 5)$brks
    others_breaks[length(others_breaks)] <- others_breaks[length(others_breaks)] + 1

    map_breaks <- c(others_breaks, best)

  }

  tmap::tm_shape(df) +
    tmap::tm_style("beaver",
             legend.format = list(fun = format_number,
                                  text.separator = " a ")) +
    tmap::tm_fill(var_plot,
            palette = map_palette,
            style = 'fixed',
            breaks = map_breaks,
            alpha = 1,
            id = "regiao_governo") +
    tmap::tm_layout(main.title.size = 1.2,
              fontfamily = 'serif',
              main.title.fontface = 'bold',
              scale = 1.1,
              bg.color = "white",
              inner.margins = c(.1, .1, .1, .1),
              main.title = map_title) +
    tmap::tm_compass(north = 0,
               type = "8star",
               size = 2,
               position = c("right", "bottom")) +
    tmap::tm_scale_bar(text.size = 0.6,
                 text.color = NA,
                 lwd = 1,
                 color.dark = "black",
                 color.light = "white") +
    tmap::tm_legend(legend.position = c(0.01,0.08)) +
    tmap::tm_borders(col = "black", lwd = 0.3)
}
