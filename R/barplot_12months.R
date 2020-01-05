#' Barplot showing the evolution of job creation through 12-month periods
#'
#' Builds a barplot depicting the evolution of job creation over time. Each bar
#' indicates the jobs created/destroyed in a 12-month period. Colors
#' represent economic sectors (IBGE's "big-five"). Labels joined by dashed
#' lines show the net job creation of each 12-month period.
#'
#' @param df A dataframe with CAGED micro-data.
#' @param mes_inicio Optional integer ranging from 1 to 12.
#' @param mes_fim Optional integer ranging from 1 to 12.
#' @param labs_title Optional character string with plot title.
#' @param labs_subtitle Optional character string with plot subtitle.
#' @param labs_caption Optional character string with plot caption.
#' @param labs_x Optional character string with x-axis title.
#' @param labs_y Optional character string with y-axis title.
#'
#' @return A \pkg{ggplot2} plot.
barplot_12months <-
  function(df,
           mes_inicio = NA,
           mes_fim = NA,
           labs_title = str_c('Empregos no Estado de SP - Evolução do Saldo Acumulado ', mes_inicio, '-', mes_fim),
           labs_subtitle = 'Contribuição de cada setor',
           labs_caption = 'Fonte: Elaboração própria a partir de dados do CAGED.',
           labs_x = 'Período',
           labs_y = 'Postos de trabalho (milhares)') {

    if (is.na(mes_inicio)) {
      numero_mes_inicio <- df %>%
        select(ano, mes) %>%
        distinct() %>%
        arrange(ano, mes) %>%
        slice(1) %>%
        select(mes) %>%
        unlist()
      mes_inicio <- meses$abrev[numero_mes_inicio]
    }

    if ((is.na(mes_fim))) {
      numero_mes_fim <- df %>%
        select(ano, mes) %>%
        distinct() %>%
        arrange(ano, mes) %>%
        tail(1) %>%
        select(mes) %>%
        unlist()
      mes_fim <- meses$abrev[numero_mes_fim]
    }

    acumulado_12meses_estado <- df %>%
      filter(!is.na(periodo)) %>%
      compute_job_creation(periodo)

    acumulado_12meses_estado_setor <- df %>%
      filter(!is.na(periodo)) %>%
      compute_job_creation(periodo, setor) %>%
      drop_na()

    plot_acumulado <-
      acumulado_12meses_estado_setor %>%
      ggplot() +
      geom_col(aes(x = periodo, y = saldo/1000, fill = setor), position = 'stack', alpha = 0.7) +
      custom_theme() +
      geom_hline(yintercept = 0, col = 'darkred') +
      labs(title = labs_title,
           subtitle = labs_subtitle,
           caption = labs_caption,
           x = labs_x,
           y = labs_y) +
      scale_fill_brewer(palette = "Accent", name = "Setor") +
      geom_line(data = acumulado_12meses_estado, aes(x = periodo, y = saldo/1000, group = 1), col = 'black', linetype = 'dotted') +
      geom_label(data = acumulado_12meses_estado, aes(x = periodo, y = saldo/1000, label = str_replace(saldo/1000, '\\.', ',')), family = 'serif', size = 3)

    plot_acumulado

  }
