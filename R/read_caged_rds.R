read_caged_rds <- function(data_folder,
                           inicio,
                           fim,
                           periodos_12meses = TRUE,
                           nested = FALSE,
                           stringAsFactors = TRUE) {

  ano_inicio <- str_extract(inicio, '\\d{4}') %>% as.integer()

  ano_fim <- str_extract(fim, '\\d{4}') %>% as.integer()

  mes_inicio <- str_remove(inicio, as.character(ano_inicio)) %>%
    str_extract('\\d{2}') %>% as.integer()

  mes_fim <- str_remove(fim, as.character(ano_fim)) %>%
    str_extract('\\d{2}') %>% as.integer()

  df_ref <- crossing(ano = as.character(ano_inicio:ano_fim),
                     mes = str_pad(1:12, width = 2, side = 'left', pad = '0')) %>%
    filter(!(ano == as.character(ano_fim) & as.numeric(mes) > mes_fim)) %>%
    filter(!(ano == as.character(ano_inicio) & as.numeric(mes) < mes_inicio))

  if (periodos_12meses & (nrow(df_ref) %% 12) != 0) {
    stop("'periodos_12meses = TRUE: favor definir período contendo múltiplo de 12 meses'")
  }

  caged_list <- list()
  for (i in 1:nrow(df_ref)) {
    filename_caged = str_c(data_folder, '/CAGED_', df_ref$ano[i], '_', df_ref$mes[i], '.rds')
    message(str_c(i, '\n ', filename_caged, '\n'))
    caged_list[[i]] <- readRDS(filename_caged)
  }

  df_ref <- df_ref %>%
    mutate(ano = as.integer(ano),
           mes = as.integer(mes))

  if (periodos_12meses) df_ref <- get_periodos_12meses(df_ref,
                                                       inicio = inicio,
                                                       fim = fim)

  df_ref$dados_caged <- caged_list

  if (!nested) df_ref <- unnest(df_ref)

  if (stringAsFactors) {
    df_ref <- df_ref %>%
      mutate_if(.predicate = is.character, .funs = as.factor)
  }

  df_ref

}
