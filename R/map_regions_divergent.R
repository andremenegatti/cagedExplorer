#' Plots divergent-scale map of Sao Paulo's Government Regions
#'
#' Wrapper around \pkg{tmap} calls to draw a map of Government Regions of the
#' State of Sao Paulo, using a divergent color scheme.
#'
#' @param df A dataframe containing one line per Government Region, and a
#'   variable named \code{regiao_governo}.
#' @param var_plot \code{character} with the name of the variable to be plotted.
#' @param map_title Optional character string with map title.
#' @param map_breaks Optional numeric vector with breaks.
#' @param map_palette Character string with the name of a divergent color
#'   palette or character vector with color HEX codes.
#'
#' @return A \pkg{tmap} plot.
map_regions_divergent <-
  function(df, var_plot,
           map_title = str_c('Regiões Gov. de SP - ', var_plot, ' - Acumulado 12 meses'),
           map_breaks = NA, map_palette = NA) {

  col_index <- which(names(df) == var_plot)

  fill_variable <- df[, col_index] %>%
    as.data.frame() %>%
    select(-geometry) %>%
    unlist()

  if (NA %in% map_breaks) {
    map_breaks <- get_breaks(fill_variable, n = 9)
  }

  if (NA %in% map_palette) {
    map_palette <- get_palette(map_breaks)
  }

  tmap::tm_shape(df) +
    tmap::tm_style("beaver",
             legend.format = list(fun = format_number,
                                  text.separator = " a ")) +
    tmap::tm_fill(var_plot,
            palette = map_palette,
            style = 'fixed',
            breaks = map_breaks,
            midpoint = 0,
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
