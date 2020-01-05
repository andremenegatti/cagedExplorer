#' Returns a dataframe with job creation data on highlighted CNAE sectors
#'
#' Identifies best- or worst-performing CNAE sectors with respect to job creation
#' and returns a dataframe with selected data.
#'
#' @param df A dataframe containing summarized job creation data. It must have
#'   columns \code{setor} and \code{saldo}.
#' @param positivos \code{logical}. If \code{FALSE}, function returns
#'   worst-performing observations.
#' @param n_destaques Number of observations to include as highlights.
#' @param filtrar_setor \code{FALSE} or one of the 'big-five' IBGE sectors. If a
#'   sector is provided, the function returns only highlights for that specific
#'   sector.
#'
#' @return A dataframe with job creation data on CNAE sectors with best (or
#'   worst) performance.
#'
#' @examples
#' \dontrun{
#' read_caged('CAGED_2018_10.txt') %>%
#'   prepare_caged() %>%
#'   filter(municipio_clean == 'RIBEIRAO PRETO') %>%
#'   compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
#'   find_cnae_highlights(setor == 'Serviços')
#' }
find_cnae_highlights <- function(df,
                                positivos = TRUE,
                                n_destaques = 5,
                                filtrar_setor = FALSE) {

  if (filtrar_setor) {
    df <- filter(df, setor == filtrar_setor)
  }

  df <- group_by(df, setor)

  if (positivos) {
    df <- arrange(df, desc(saldo))
  } else {
    df <- arrange(df, saldo)
  }

    df <- slice(df, 1:n_destaques) %>%
    ungroup()

    df

}
