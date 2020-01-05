#' Barplot showing the monthly evolution of job creation
#'
#' Builds a barplot depicting the evolution of job creation over time, by month.
#' Each bar indicates the jobs created/destroyed in a month. Colors represent
#' economic sectors (IBGE's "big-five"). Labels joined by dashed lines show
#' monthly net job creation.
#'
#' @param df A dataframe with CAGED micro-data.
#' @param inicio Optional string indicating the starting month, if one does not
#'   to use all available data. Recommended format: YYYY-MM.
#' @param fim Optional string indicating the final month, if one does not to use
#'   all available data. Recommended format: YYYY-MM.
#' @param incluir_ano_x \code{logical}. If \code{TRUE}, x-axis labels include
#'   the year of each month. Recommended when plotting months of more than a
#'   single year. Defaults to \code{FALSE}.
#' @param labs_title Optional character string with plot title.
#' @param labs_subtitle Optional character string with plot subtitle.
#' @param labs_caption Optional character string with plot caption.
#' @param labs_x Optional character string with x-axis title.
#' @param labs_y Optional character string with y-axis title.
#'
#' @return A \pkg{ggplot2} plot.
barplot_monthly <- function(df,
                              inicio = NA,
                              fim = NA,
                              incluir_ano_x = FALSE,
                              labs_title = 'Empregos no Estado de SP - Evolução mensal',
                              labs_subtitle = 'Contribuição de cada setor',
                              labs_caption = 'Fonte: Elaboração própria a partir de dados do CAGED.',
                              labs_x = 'Mês',
                              labs_y = 'Postos de trabalho (milhares)') {


  if (is.na(inicio) & (is.na(fim))) {
    message(" 'inicio' e 'fim' não especificados: incluindo todos os meses...")
  } else if (is.na(inicio) & !is.na(fim)) {
    ano_fim <- str_extract(fim, '\\d{4}') %>% as.integer()
    mes_fim <- str_remove(fim, as.character(ano_fim)) %>%
      str_extract('\\d{2}') %>% as.integer()
    df <- df %>%
      filter(ano <= ano_fim) %>%
      filter(!(ano == ano_fim & mes > mes_fim))
  } else if (!is.na(inicio) & is.na(fim)) {
    ano_inicio <- str_extract(inicio, '\\d{4}') %>% as.integer()
    mes_inicio <- str_remove(inicio, as.character(ano_inicio)) %>%
      str_extract('\\d{2}') %>% as.integer()
    df <- df %>%
      filter(ano >= ano_inicio) %>%
      filter(!(ano == ano_inicio & mes < mes_inicio))
  } else {
    ano_fim <- str_extract(fim, '\\d{4}') %>% as.integer()
    mes_fim <- str_remove(fim, as.character(ano_fim)) %>%
      str_extract('\\d{2}') %>% as.integer()
    ano_inicio <- str_extract(inicio, '\\d{4}') %>% as.integer()
    mes_inicio <- str_remove(inicio, as.character(ano_inicio)) %>%
      str_extract('\\d{2}') %>% as.integer()
    df <- df %>%
      filter(ano <= ano_fim, ano >= ano_inicio) %>%
      filter(!(ano == ano_fim & mes > mes_fim)) %>%
      filter(!(ano == ano_inicio & mes < mes_inicio))
  }

  evolucao_mensal <- df %>%
    compute_job_creation(ano, mes) %>%
    arrange(ano, mes) %>%
    mutate(ano_mes = str_c(ano, str_pad(mes, width = 2, side = 'left', pad = '0'), sep = '/'))

  evolucao_mensal_setor <- df %>%
    compute_job_creation(ano, mes, setor) %>%
    drop_na() %>%
    arrange(ano, mes) %>%
    mutate(ano_mes = str_c(ano, str_pad(mes, width = 2, side = 'left', pad = '0'), sep = '/'))


  if (!incluir_ano_x) {
    evolucao_mensal_setor <- evolucao_mensal_setor %>%
      mutate(label_mes = meses$abrev[mes])
    evolucao_mensal <- evolucao_mensal %>%
      mutate(label_mes = meses$abrev[mes])
  } else {
    evolucao_mensal_setor <- evolucao_mensal_setor %>%
      mutate(label_mes = str_c(meses$abrev[mes], str_remove(ano, '\\d{2}'), sep = '/'))
    evolucao_mensal <- evolucao_mensal %>%
      mutate(label_mes = str_c(meses$abrev[mes], str_remove(ano, '\\d{2}'), sep = '/'))
  }

  barplot_monthly <-
    ggplot(data = evolucao_mensal_setor) +
    geom_col(aes(x = as.factor(ano_mes),
                 y = saldo/1000,
                 fill = setor),
             position = 'stack',
             alpha = 0.9) +
    custom_theme() +
    geom_hline(yintercept = 0,
               col = 'darkred') +
    labs(title = labs_title,
         subtitle = labs_subtitle,
         caption = labs_caption,
         x = labs_x,
         y = labs_y) +
    scale_fill_brewer(palette = "Accent", name = "Setor") +
    scale_x_discrete(labels = evolucao_mensal$label_mes) +
    geom_line(data = evolucao_mensal, aes(x = ano_mes, y = saldo/1000, group = 1), col = 'black', linetype = 'dotted') +
    geom_label(data = evolucao_mensal, aes(x = ano_mes, y = saldo/1000, label = str_replace(saldo/1000, '\\.', ',')), family = 'serif', size = 3)


  barplot_monthly

}


