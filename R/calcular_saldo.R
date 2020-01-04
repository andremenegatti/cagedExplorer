#' Computes job creation
#'
#' Computes job creation, job loss and balance, given a dataframe with CAGED
#' micro-data. The input dataframe must have a column named \code{movimento},
#' whose values take values 1 (job creation) or -1 (job destruction).
#'
#' Allows for aggregated summaries by passing grouping variables in \code{...}
#'
#' @param df A dataframe with CAGED micro-data. Must have a \code{movimento}
#'   column.
#' @param ... Additional variables for aggretated summaries.
#'
#' @return A dataframe.
calcular_saldo <- function(df, ...) {

  df %>%
    group_by(...) %>%
    summarise(vagas_criadas = sum(movimento == 1),
              vagas_destruidas = sum(movimento == -1),
              saldo = sum(movimento)) %>%
    ungroup()

}
