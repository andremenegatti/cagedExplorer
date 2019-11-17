mapa_regiao_saldo <- function(df,
                              var_saldo,
                              map_title = str_c('Regiões Gov. de SP - ', var_saldo, ' - Acumulado 12 meses'),
                              map_breaks = NA,
                              map_palette = NA
) {

  col_index <- which(names(df) == var_saldo)

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

  tm_shape(df) +
    tm_style("beaver",
             legend.format = list(fun = format_map_legend,
                                  text.separator = " a ")) +
    tm_fill(var_saldo,
            palette = map_palette,
            style = 'fixed',
            breaks = map_breaks,
            midpoint = 0,
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
