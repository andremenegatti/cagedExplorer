criar_tabela_municipio <- function(df_ano_atual, df_ano_anterior, mes_atual, municipio, colunas = NA, csv_output = NA) {

  # Checando argumentos
  if (class(colunas) != 'logical') {
    if (class(colunas) != 'character' | length(colunas) != 5) {
      stop("'colunas' must be a 5-element character vector")
    }
  }

  if (!is.na(csv_output) & !str_detect(csv_output, '\\.csv')) {
    stop("'csv_output' must be a valid csv file path")
  }

  if (!is.numeric(mes_atual)) {
    stop("'mes_atual' must be an integer between 1 and 12")
  } else {
    if (mes_atual < 1 | mes_atual > 12) {
      stop("'mes_atual' must be an integer between 1 and 12")
    }
  }

  # Separando dados do mes
  df_mes_ano_atual <- filter(df_ano_atual, mes == mes_atual)
  df_mes_ano_anterior <- filter(df_ano_anterior, mes == mes_atual)

  # Calculando saldos mensais
  saldo_mes_ano_atual <- df_mes_ano_atual %>%
    filter(municipio_clean == mun) %>%
    calcular_saldo(setor, .drop = FALSE) %>%
    select(setor, saldo_mes_atual = saldo)

  saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
    filter(municipio_clean == mun) %>%
    calcular_saldo(setor, .drop = FALSE) %>%
    select(setor, saldo_mes_anterior = saldo)

  # Calculando saldos acumulados - 12 meses
  saldo_acum_ano_atual <- df_ano_atual %>%
    filter(municipio_clean == mun) %>%
    calcular_saldo(setor, .drop = FALSE) %>%
    select(setor, saldo_acum_atual = saldo)

  saldo_acum_ano_anterior <- df_ano_anterior %>%
    filter(municipio_clean == mun) %>%
    calcular_saldo(setor, .drop = FALSE) %>%
    select(setor, saldo_acum_anterior = saldo)

  # Juntando resultados em uma tabela unica
  tabela_municipio <- saldo_mes_ano_anterior %>%
    inner_join(saldo_acum_ano_anterior, by = 'setor') %>%
    inner_join(saldo_mes_ano_atual, by = 'setor') %>%
    inner_join(saldo_acum_ano_atual, by = 'setor')

  # Calculando total de cada coluna
  totais <- tabela_municipio %>%
    select(-setor) %>%
    apply(2, sum)

  # Criando DF com totais calculados acima
  df_totais <- tibble(setor = 'Total',
                      saldo_mes_anterior = totais[1],
                      saldo_acum_anterior = totais[2],
                      saldo_mes_atual = totais[3],
                      saldo_acum_atual = totais[4])

  # Juntando totais na tabela
  tabela_municipio <- tabela_municipio %>%
    mutate_if(is.factor, as.character) %>%
    bind_rows(df_totais)

  # Mudando nomes
  if (class(colunas) == 'character') colnames(tabela_municipio) <- colunas

  # Salvando csv
  if (!is.na(csv_output)) {
    write.csv(tabela_municipio, csv_output, row.names = FALSE)
  }

  tabela_municipio

}
