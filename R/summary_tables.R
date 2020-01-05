#' Creates summary table for a given city
#'
#' @param df_ano_atual A dataframe.
#' @param df_ano_anterior A dataframe.
#' @param mes_atual An integer between 1 and 12.
#' @param municipio A character string with the city name.
#' @param colunas A character vector with the column names of the output
#'   dataframe.
#' @param csv_output A character string with the csv output path. Optional.
#'
#' @return A dataframe.
criar_tabela_municipio <- function(df_ano_atual, df_ano_anterior,
                                   mes_atual, municipio, colunas = NA,
                                   csv_output = NA) {

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
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_mes_atual = saldo)

  saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
    filter(municipio_clean == mun) %>%
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_mes_anterior = saldo)

  # Calculando saldos acumulados - 12 meses
  saldo_acum_ano_atual <- df_ano_atual %>%
    filter(municipio_clean == mun) %>%
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_acum_atual = saldo)

  saldo_acum_ano_anterior <- df_ano_anterior %>%
    filter(municipio_clean == mun) %>%
    compute_job_creation(setor, .drop = FALSE) %>%
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





#' Create a summary table for a given Admnistrative Region
#'
#' @param df_ano_atual A dataframe.
#' @param df_ano_anterior A dataframe.
#' @param mes_atual An integer between 1 and 12.
#' @param regiao_adm A character string with the region name.
#' @param colunas A character vector with the column names of the output
#'   dataframe.
#' @param csv_output A character string with the csv output path. Optional.
#'
#' @return A dataframe.
criar_tabela_regiao_adm <- function(df_ano_atual, df_ano_anterior,
                                    mes_atual, regiao_adm = 'Ribeirão Preto',
                                    colunas = NA, csv_output = NA) {

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
    filter(regiao_administrativa == regiao_adm) %>%
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_mes_atual = saldo)

  saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
    filter(regiao_administrativa == regiao_adm) %>%
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_mes_anterior = saldo)

  # Calculando saldos acumulados - 12 meses
  saldo_acum_ano_atual <- df_ano_atual %>%
    filter(regiao_administrativa == regiao_adm) %>%
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_acum_atual = saldo)

  saldo_acum_ano_anterior <- df_ano_anterior %>%
    filter(regiao_administrativa == regiao_adm) %>%
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_acum_anterior = saldo)

  # Juntando resultados em uma tabela unica
  tabela_regiao <- saldo_mes_ano_anterior %>%
    inner_join(saldo_acum_ano_anterior, by = 'setor') %>%
    inner_join(saldo_mes_ano_atual, by = 'setor') %>%
    inner_join(saldo_acum_ano_atual, by = 'setor')

  # Calculando total de cada coluna
  totais <- tabela_regiao %>%
    select(-setor) %>%
    apply(2, sum)

  # Criando DF com totais calculados acima
  df_totais <- tibble(setor = 'Total',
                      saldo_mes_anterior = totais[1],
                      saldo_acum_anterior = totais[2],
                      saldo_mes_atual = totais[3],
                      saldo_acum_atual = totais[4])

  # Juntando totais na tabela
  tabela_regiao <- tabela_regiao %>%
    mutate_if(is.factor, as.character) %>%
    bind_rows(df_totais)

  # Mudando nomes
  if (class(colunas) == 'character') colnames(tabela_regiao) <- colunas

  # Salvando csv
  if (!is.na(csv_output)) {
    write.csv(tabela_regiao, csv_output, row.names = FALSE)
  }

  tabela_regiao

}



#' Create a summary table for the State of Sao Paulo
#'
#' @param df_ano_atual A dataframe.
#' @param df_ano_anterior A dataframe.
#' @param mes_atual An integer between 1 and 12.
#' @param colunas A character vector with the column names of the output
#'   dataframe.
#' @param csv_output A character string with the csv output path. Optional.
#'
#' @return A dataframe.
criar_tabela_estado <- function(df_ano_atual, df_ano_anterior,
                                mes_atual, colunas = NA, csv_output = NA) {

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
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_mes_atual = saldo)

  saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_mes_anterior = saldo)

  # Calculando saldos acumulados - 12 meses
  saldo_acum_ano_atual <- df_ano_atual %>%
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_acum_atual = saldo)

  saldo_acum_ano_anterior <- df_ano_anterior %>%
    compute_job_creation(setor, .drop = FALSE) %>%
    select(setor, saldo_acum_anterior = saldo)

  # Juntando resultados em uma tabela unica
  tabela_estado <- saldo_mes_ano_anterior %>%
    inner_join(saldo_acum_ano_anterior, by = 'setor') %>%
    inner_join(saldo_mes_ano_atual, by = 'setor') %>%
    inner_join(saldo_acum_ano_atual, by = 'setor')

  # Calculando total de cada coluna
  totais <- tabela_estado %>%
    select(-setor) %>%
    apply(2, sum)

  # Criando DF com totais calculados acima
  df_totais <- tibble(setor = 'Total',
                      saldo_mes_anterior = totais[1],
                      saldo_acum_anterior = totais[2],
                      saldo_mes_atual = totais[3],
                      saldo_acum_atual = totais[4])

  # Juntando totais na tabela
  tabela_estado <- tabela_estado %>%
    mutate_if(is.factor, as.character) %>%
    bind_rows(df_totais)

  # Mudando nomes
  if (class(colunas) == 'character') colnames(tabela_estado) <- colunas

  # Salvando csv
  if (!is.na(csv_output)) {
    write.csv(tabela_estado, csv_output, row.names = FALSE)
  }

  tabela_estado

}
