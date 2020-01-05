#' Reads CAGED data from txt file
#'
#' Uses \code{read_delim()} to read a flat file containing monthly data from
#' CAGED, as provided by the Brazilian Ministry of Labor.
#'
#' @param txt_file Character string indicating the txt file path.
#' @param pretty Logical. If \code{TRUE}, uses well formatted column names
#'   instead of those in the txt file.
#' @param ... Additinal arguments to \code{read_delim}.
#'
#' @return A dataframe with data from the txt file.
#'
#' @examples
#' \dontrun{
#' caged_out2018 <- read_caged('CAGEDEST_102018.txt')
#' }
read_caged <- function(txt_file, pretty = TRUE, ...) {

  df_caged <-
    read_delim(file = txt_file,
               delim = ";",
               trim_ws = TRUE,
               escape_double = FALSE,
               locale = locale(
                 decimal_mark = ",",
                 grouping_mark = ".",
                 encoding = "ISO-8859-1"
               ),
               col_types = cols_only(
                 `Município` = col_integer(),
                 `CNAE 2.0 Classe` = col_integer(),
                 `Faixa Empr Início Jan` = col_integer(),
                 `Grau Instrução` = col_integer(),
                 `Qtd Hora Contrat` = col_integer(),
                 `IBGE Subsetor` = col_integer(),
                 `Idade` = col_integer(),
                 `Raça Cor` = col_integer(),
                 `Salário Mensal` = col_double(),
                 `Saldo Mov` = col_integer(),
                 `Sexo` = col_integer(),
                 `Tempo Emprego` = col_double(),
                 `UF` = col_integer()
               ), ...
    )

  if (pretty) {
    df_caged <- df_caged %>%
      select(codigo_municipio = `Município`,
             codigo_uf = `UF`,
             movimento = `Saldo Mov`,
             codigo_subsetor_ibge = `IBGE Subsetor`,
             codigo_cnae = `CNAE 2.0 Classe`,
             codigo_faixa_tamanho_estab = `Faixa Empr Início Jan`,
             salario = `Salário Mensal`,
             codigo_grau_instrucao = `Grau Instrução`,
             horas_contrato = `Qtd Hora Contrat`,
             tempo_emprego = `Tempo Emprego`,
             codigo_raca_cor = `Raça Cor`,
             codigo_sexo = `Sexo`,
             idade = `Idade`)
  }

  df_caged

}


#' Reads one or multiple .rds files with preprocessed CAGED data
#'
#' Loops through rds files containing dataframes with CAGED preprocessed data
#' from a given month, reads them, and organizes data in a single dataframe.
#'
#' \strong{Note:} rds files must be located inside the directory passed in the
#' \code{data_folder} argument, and must be named in the following way:
#' \emph{CAGED_YYYY_MM.rds}.
#'
#' @param data_folder A character string indicating the directory where the rds
#'   files are located.
#' @param start A character string indicating the month and year of the
#'   \emph{first} month. Recommended format: YYYY-MM.
#' @param end A character string indicating the month and year of the
#'   \emph{last} month. Recommended format: YYYY-MM.
#' @param periodos_12meses Logical. Can be set to \code{TRUE} only if multiples
#'   of 12 rds files are read. If \code{TRUE}, the resulting dataframe contains
#'   a column indicating the 12-month period each observation belongs to.
#' @param nested Logical. Defaults to \code{FALSE}. If \code{TRUE}, the function
#'   returns a nested dataframe with a single row per rds file.
#' @param stringAsFactors Logical.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#' caged2012 <-
#' read_caged_rds(data_folder = 'caged_clean',
#'                start = '2012-01', end = '2012-12')
#' }
read_caged_rds <- function(data_folder,
                           start,
                           end,
                           periodos_12meses = TRUE,
                           nested = FALSE,
                           stringAsFactors = TRUE) {

  ano_inicio <- str_extract(start, '\\d{4}') %>% as.integer()

  ano_fim <- str_extract(end, '\\d{4}') %>% as.integer()

  mes_inicio <- str_remove(start, as.character(ano_inicio)) %>%
    str_extract('\\d{2}') %>% as.integer()

  mes_fim <- str_remove(end, as.character(ano_fim)) %>%
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

  if (periodos_12meses) df_ref <- add_12month_periods(df_ref,
                                                       start = start,
                                                       end = end)

  df_ref$dados_caged <- caged_list

  if (!nested) df_ref <- unnest(df_ref)

  if (stringAsFactors) {
    df_ref <- df_ref %>%
      mutate_if(.predicate = is.character, .funs = as.factor)
  }

  df_ref

}
