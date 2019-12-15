barplot_mensal_sp <- function(df,
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
    calcular_saldo(ano, mes) %>%
    arrange(ano, mes) %>%
    mutate(ano_mes = str_c(ano, str_pad(mes, width = 2, side = 'left', pad = '0'), sep = '/'))

  evolucao_mensal_setor <- df %>%
    calcular_saldo(ano, mes, setor) %>%
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

  barplot_mensal_sp <-
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


  barplot_mensal_sp

}


