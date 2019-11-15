get_periodos_12meses <-
  function(df, inicio, fim) {

    ano_inicio <- str_extract(inicio, '\\d{4}') %>% as.integer()

    ano_fim <- str_extract(fim, '\\d{4}') %>% as.integer()

    mes_inicio <- str_remove(inicio, as.character(ano_inicio)) %>%
      str_extract('\\d{2}') %>% as.integer()

    mes_fim <- str_remove(fim, as.character(ano_fim)) %>%
      str_extract('\\d{2}') %>% as.integer()

    n_periodos <- ano_fim - ano_inicio

    df_periodos <- df %>%
      select(ano, mes) %>%
      distinct() %>%
      filter(ano >= ano_inicio, ano <= ano_fim) %>%
      filter(!(ano == ano_inicio & mes < mes_inicio)) %>%
      filter(!(ano == ano_fim & mes > mes_fim)) %>%
      arrange(ano, mes) %>%
      mutate(periodo = rep(1:n_periodos, each = 12))

    sigla_mes_inicio <- meses$abrev[meses$numero == mes_inicio]
    sigla_mes_fim <- meses$abrev[meses$numero == mes_fim]

    factor_labels <- map_chr(.x = ano_inicio:(ano_fim - 1),
                             .f = ~ str_c(sigla_mes_inicio, '/', .x,
                                          ' a ',
                                          sigla_mes_fim, '/', (.x + 1)
                                          )
                             )

    # return(list(df_periodos, factor_labels))

    df %>%
      inner_join(df_periodos, by = c("ano", "mes")) %>%
      mutate(periodo = factor(periodo,
                              labels = factor_labels))

  }
