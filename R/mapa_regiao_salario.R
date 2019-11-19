mapa_regiao_salario <- function(df,
                              var_saldo,
                              map_title = str_c('Regiões Gov. de SP - ', var_saldo, ' - Acumulado 12 meses'),
                              map_breaks = NA,
                              map_palette = 'Blues'
                              )
  {

  if (is.na(map_breaks)) {
    col_index <- which(names(df) == var_saldo)

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

  tm_shape(df) +
    tm_style("beaver",
             legend.format = list(fun = format_map_legend,
                                  text.separator = " a ")) +
    tm_fill(var_saldo,
            palette = map_palette,
            style = 'fixed',
            breaks = map_breaks,
            alpha = 1,
            id = "regiao_governo") +
    tm_layout(main.title.size = 1.2,
              fontfamily = 'serif',
              main.title.fontface = 'bold',
              scale = 1.1,
              bg.color = "white",
              inner.margins = c(.1, .1, .1, .1),
              main.title = map_title) +
    tm_compass(north = 0,
               type = "8star",
               size = 2,
               position = c("right", "bottom")) +
    tm_scale_bar(text.size = 0.6,
                 text.color = NA,
                 lwd = 1,
                 color.dark = "black",
                 color.light = "white") +
    tm_legend(legend.position = c(0.01,0.08)) +
    tm_borders(col = "black", lwd = 0.3)

}
