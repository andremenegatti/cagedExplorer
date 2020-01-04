#' Prepares CAGED micro-data to data analysis focused on the State of Sao Paulo
#'
#' Filters CAGED microdata to observations belonging to the State of Sao Paulo,
#' inludes additional variables containing municipality-level information and
#' decodes numeric-encoded categoric variables.
#'
#' @param df A dataframe similar to the output of \code{read_caged()}.
#'
#' @return A dataframe with additional data.
#'
#' @examples
#' \dontrun{
#' read_caged('CAGEDEST_102018.txt') %>%
#' prepare_caged()
#' }
prepare_caged <- function(df) {
  df %>%
    filter(codigo_uf == 35) %>%
    mutate(setor = get_ibge_sector(codigo_subsetor_ibge)) %>%
    join_municipios_sp() %>%
    decode_variables() %>%
    drop_codes()
}


#' Returns IBGE's 5 'big sectors' from numeric vector of IBGE' subsectors
#'
#' Relationship between economic subsectors and sectors: \itemize{ \item
#' \strong{[1, 14]}: Indústria \item \strong{15}: Construção Civil \item
#' \strong{[16, 17]}: Comércio \item \strong{[18, 24]}: Serviços \item
#' \strong{25}: Agricultura }
#'
#' @param x A numeric vector, with integers ranging from 1 to 25.
#' @param return_factor Logical. If \code{TRUE}, returns a factor. Else, returns
#'   a character vector.
#'
#' @return A factor or character vector with the corresponding sector.
#'
#' @examples
#' get_ibge_sector(x = 1:25, return_factor = FALSE)
get_ibge_sector <- function(x = codigo_subsetor_ibge, return_factor = TRUE) {

  if (!is.numeric(x)) {
    stop('Vector must be numeric')
  }

  grande_setor <-
    case_when(x %in% 1:14 ~ 'Indústria',
              x %in% 16:17 ~ 'Comércio',
              x %in% 18:24 ~ 'Serviços',
              x == 15 ~ 'Construção civil',
              x == 25 ~ 'Agropecuária')

  if (return_factor) {
    return(as.factor(grande_setor))
  } else {
    return(grande_setor)
  }

}


#' Adds municipality-level data to a dataframe containing CAGED microdata
#'
#' Includes new variables with data regarding cities from the State of Sao
#' Paulo. New data comes from dataset \code{municipios_sp}, and includes:
#' \itemize{
#' \item \code{Codmun7}
#' \item \code{municipio}
#' \item \code{municipio_clean}
#' \item \code{pop2019}
#' \item \code{regiao_admnisitrativa}
#' \item \code{regiao_governo}
#' \item \code{regiao_metropolitana}
#' \item \code{aglomeracao_urbana}
#' }
#'
#' \strong{This function supports cities from the State of Sao Paulo only.
#' Thus, it is recommended to filter the input dataframe beforehand.}
#'
#' @param df A dataframe. It must contain a numeric column
#'   \code{codigo_municipio}, with the 6-digit city code.
#'
#' @return A dataframe with additional columns.
#'
#' @examples
#' \dontrun{
#' read_caged() %>%
#'   join_dados_municipios()
#' }
join_municipios_sp <- function(df) {

  joined <- df %>%
    left_join(municipios_sp %>%
                select(Codmun7, codigo, municipio, municipio_clean, pop2019,
                       regiao_administrativa, regiao_governo, regiao_metropolitana),
              by = c('codigo_municipio' = 'codigo')) %>%
    mutate(regiao_administrativa = as.factor(regiao_administrativa),
           regiao_governo = as.factor(regiao_governo),
           regiao_metropolitana = as.factor(regiao_metropolitana),
           municipio = as.factor(municipio),
           municipio_clean = as.factor(municipio_clean))

}


#' Decodes numeric-encoded categoric variables from CAGED micro-data
#'
#' Creates new variables with proper labels, given a dataframe with
#' numeric-encoded categoric variables.
#'
#' Supported variables: \itemize{ \item
#' \code{codigo_cnae} \item \code{codigo_grau_instrucao} \item
#' \code{codigo_raca_cor} \item \code{codigo_faixa_tamanho_estab} \item
#' \code{codigo_sexo} }
#'
#' @param df A dataframe with encoded categoric variables.
#' @param vars A character vector with supported variable names or "all".
#'
#' @return A dataframe, with additional columns for the decoded variables.
decode_variables <- function(df, vars = 'all') {

  if (vars == 'all') {
    vars <- c('codigo_cnae',
              'codigo_grau_instrucao',
              'codigo_raca_cor',
              'codigo_faixa_tamanho_estab',
              'codigo_sexo')
  }

  if ('codigo_cnae' %in% vars) {
    df <- df %>%
      left_join(cnae_classes, by = c('codigo_cnae' = 'codigo')) %>%
      mutate(classe_cnae = as.factor(classe_cnae))
  }

  if ('codigo_grau_instrucao' %in% vars) {
    df <- df %>%
      left_join(grau_instrucao, by = c('codigo_grau_instrucao' = 'codigo')) %>%
      mutate(grau_instrucao = as.factor(grau_instrucao))
  }

  if ('codigo_raca_cor' %in% vars) {
    df <- df %>%
      left_join(raca_cor, by = c('codigo_raca_cor' = 'codigo')) %>%
      mutate(raca_cor = as.factor(raca_cor))
  }

  if ('codigo_faixa_tamanho_estab' %in% vars) {
    df <- df %>%
      left_join(faixa_tamanho_estab, by = c('codigo_faixa_tamanho_estab' = 'codigo')) %>%
      mutate(faixa_tamanho_estab = as.factor(faixa_tamanho_estab))
  }

  if ('codigo_sexo' %in% vars) {
    df$sexo <- NA
    df <- df %>%
      mutate(sexo = case_when(codigo_sexo == 1 ~ 'M',
                              codigo_sexo == 2 ~ 'F')) %>%
      mutate(sexo = as.factor(sexo))
  }

}

#' Drops columns containing numeric-encoded categoric variables
#'
#' Removes the following variables from a dataframe: \itemize{ \item
#' \code{codigo_uf} \item \code{codigo_raca_cor} \item \code{codigo_sexo}
#' \item \code{codigo_grau_instrucao} \item \code{codigo_sexo} }
#'
#' @param df A dataframe containing the numeric-encoded variables listed above.
#'
#' @return The same dataframe, but without the listed variables.
#'
#' @examples
#' \dontrun{
#' read_caged() %>%
#'   decode_variables('all') %>%
#'   drop_codes()
#' }
drop_codes <- function(df) {
  df %>%
    select(-codigo_uf, -codigo_raca_cor, -codigo_sexo,
           -codigo_grau_instrucao, -codigo_sexo)
}
