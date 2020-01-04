#' Adds a geometry column to dataframe with region-level data
#'
#' Joins provided dataframe with another containing region-level
#' \code{POLYGON} geospatial data. The key used to perform the join is the
#' character column \emph{regiao_governo}.
#'
#' @param df A dataframe containing data from Sao Paulo's regions
#' (\emph{Regiões de Governo}). It must contains a column named
#' \emph{regiao_governo}.
#'
#' @return A dataframe containing the original data and an additional
#'   \emph{geometry} column with region-level geospatial data.
#'
#' @examples
#' municipios_sp %>%
#'   count(regiao_governo) %>%
#'   add_geometry_municipios()
add_geometry_regioes_gov <- function(df) {
  polygons_regioes_gov_sp %>%
    right_join(df, by = 'regiao_governo')
}

#' Adds a geometry column to dataframe with municipality-level data
#'
#' Joins provided dataframe with another containing municipality-level
#' \code{MULTIPOLYGON} geospatial data. The key used to perform the join is the
#' numeric column \emph{Codmun7}, which must contain cities' 7-digit code, as
#' used by IBGE.
#'
#' @param df A dataframe containing data from cities belonging to the state of
#'   Sao Paulo. The dataframe must have a numeric column named \emph{Codmun7}
#'   with cities' IBGE 7-digit code.
#'
#' @return A dataframe containing the original data and an additional
#'   \emph{geometry} column with municipality-level geospatial data.
#'
#' @examples
#' municipios_sp %>%
#'   add_geometry_municipios()
add_geometry_municipios <- function(df) {
  polygons_municipios_sp %>%
    select(-NM_MUNICIP) %>%
    right_join(df,
               by = 'Codmun7')
}
