dotplot_regions <- function(df, var_plot, breaks, palette,
                    labs_x = NA, labs_y = NA,
                    labs_title = NA, labs_subtitle = NA,
                    labs_caption= NA) {

  var_plot <- enquo(var_plot)

  if (is.na(labs_x)) {
    labs_x = deparse(var_plot) %>%
      str_remove('^~') %>%
      str_remove_all('`')
  }

  if (is.na(labs_y)) {
    labs_y = 'Região de Governo'
  }

  df <- df %>%
    mutate(regiao_governo = fct_reorder(regiao_governo, !! var_plot)) %>%
    mutate(grupo_saldo = group_by_breaks(!! var_plot, breaks = breaks) %>%
             as.factor() %>% fct_reorder(!! var_plot))

  plot <- ggplot(data = df) +
    geom_point(aes(x = !! var_plot, y = regiao_governo, fill = grupo_saldo),
               size = 5, shape = 21, col = 'gray') +
    geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
    scale_fill_manual(values = palette) +
    custom_theme() +
    theme(legend.position = 'none') +
    labs(x = labs_x, y = labs_y)

  if (!is.na(labs_title)) {
    plot <- plot + labs(title = labs_title)
  }

  if (!is.na(labs_subtitle)) {
    plot <- plot + labs(subtitle = labs_subtitle)
  }

  if (!is.na(labs_caption)) {
    plot <- plot + labs(caption = labs_caption)
  }

  plot
}
