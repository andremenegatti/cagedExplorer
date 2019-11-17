mapa_regiao_salario <- function(df,
                              var_saldo,
                              map_title = str_c('Regiões Gov. de SP - ', var_saldo, ' - Acumulado 12 meses')
) {

  tm_shape(df) +
    tm_style("beaver",
             legend.format = list(fun = format_map_legend,
                                  text.separator = " a ")) +
    tm_fill(var_saldo,
            palette = 'Blues',
            style = 'quantile',
            n = 9,
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
