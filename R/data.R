#' Set of blue HEX codes
#'
#' A character vector containing HEX codes of 9 intensities of blue,
#' from lightest to darkest. Useful when adjusting plots colors manually.
#'
#' @format A character vector.
#' @source Created using \pkg{RColorBrewer}.
"blues_full"

#' Set of red HEX codes
#'
#' A character vector containing HEX codes of 11 intensities of red,
#' from darkest to ligthest. Useful when adjusting plots colors manually.
#'
#' @format A character vector.
#' @source Created using \pkg{RColorBrewer}.
"reds_full"


#' Lookup table of CNAE classes codes and names
#'
#' A dataframe with numeric codes and names of economic activities,
#' according to the National Classification of Economic Activities
#'  - CNAE.
#'
#' @format A dataframe with 673 rows and 2 variables:
#' \describe{
#' \item{codigo}{CNAE numeric code}
#' \item{classe_cnae}{CNAE class}
#' }
#' @source \url{https://concla.ibge.gov.br/agencia-sala-de-imprensa/classificacoes/download-concla/8265-download.html}
"cnae_classes"


#' Lookup table of codes and labels of company sizes
#'
#' In CAGED microdata, companies are binned according to the number of
#' people they employ. This dataframe contains codes and labels for
#' each bin.
#'
#' @format A dataframe with 10 rows and 2 variables:
#' \describe{
#' \item{codigo}{Numeric code, ranging from 1 (up to 4 employees) to 9 (more than 1000 employees) (-1 for \code{NA})}
#' \item{faixa_tamanho_estab}{Company size bin}
#' }
#'
#' @source \url{ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/CAGEDEST_layout_Atualizado.xls}
"faixa_tamanho_estab"

#' Lookup table of education levels codes and labels
#'
#' A dataframe with codes and labels of education levels used
#' in CAGED microdata.
#'
#' @format A dataframe with 12 rows and 2 variables:
#' \describe{
#' \item{codigo}{Numeric code, ranging from 1 (illiterate) to 11 (PhD) (-1 for \code{NA})}
#' \item{grau_instrucao}{Education level, in Portuguese}
#' }
#'
#' @source \url{ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/CAGEDEST_layout_Atualizado.xls}
"grau_instrucao"

#' Lookup table of month abbreviations in Portuguese
#'
#' A dataframe with month numbers and abbreviations in Portuguese.
#'
#' @format A dataframe with 12 rows and 2 variables:
#' \describe{
#' \item{numero}{Month number, from 1 to 12}
#' \item{abrev}{Month abbreviation, in Portuguese}
#' }
"meses"

#' Dataframe with information regarding cities belongin to the State of Sao Paulo
#'
#' A dataset with information regarding the 645 Brazilian cities from
#'  the State of Sao Paulo. Useful in joins, to find cities' population
#'  and regions, using city codes or names as keys.
#'
#' @format A dataframe with 645 rows and 9 variables:
#' \describe{
#' \item{Codmun7}{City 7-digit code}
#' \item{codigo}{City 6-digit code}
#' \item{municipio}{City name}
#' \item{municipio_clean}{City name, all uppercase, without accents, cedillas and tildes}
#' \item{pop}{City population in 2019}
#' \item{regiao_administrativa}{City's Admnistrative Region}
#' \item{regiao_governo}{City's Government Region}
#' \item{regiao_metropolitana}{City's Metropolitan Area, if applicable}
#' \item{aglomeracao_urbana}{City's urban agglomeration, if applicable}
#' }
#' @source IBGE
"municipios_sp"

#' \pkg{sf} dataframe with geospatial data on cities from the State of Sao Paulo
#'
#' A dataset of boundaries for each of the 645 Brazilian cities belonging
#' to the State of Sao Paulo.
#'
#' @format A \pkg{sf} dataframe with 645 features (rows), 2 fields (variables),
#'  and geometry:
#' \describe{
#' \item{NM_MUNICIP}{City name, all uppercase, with accents, tildes and cedillas}
#' \item{Codmun7}{City 7-digit code}
#' \item{geometry}{\code{MULTIPOLYGON} geometry with coordinates of city's boundaries}
#' }
"polygons_municipios_sp"

#' \pkg{sf} dataframe with geospatial data on regions from the State of Sao Paulo
#'
#' A dataset of boundaries for each of the 43 Government Regions from the
#' State of Sao Paulo.
#'
#' @format A \pkg{sf} dataframe with 43 features (rows), 1 field (variable),
#'  and geometry:
#' \describe{
#' \item{NM_MUNICIP}{Region name, title case, with accents, tildes and cedillas}
#' \item{geometry}{Coordinates of region boundaries}
#' }
"polygons_regioes_gov_sp"


#' Lookup table of race/color codes and labels
#'
#' A dataframe with numeric codes and labels of races and colors,
#' as used in CAGED microdata.
#'
#' @format A dataframe with 7 rows and 2 variables:
#' \describe{
#' \item{codigo}{Numeric code, ranging from 1 to 9 (-1 for \code{NA})}
#' \item{raca_cor}{Person's race/color}
#' }
#' @source \url{ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/CAGEDEST_layout_Atualizado.xls}
"raca_cor"

#' Lookup table of IBGE's subsectors of economic activities
#'
#' A dataframe with numeric codes and labels of IBGE's
#'  economic subsectors.
#'
#' @format A dataframe with 21 rows and 2 variables:
#' \describe{
#' \item{codigo}{Subsector numeric code}
#' \item{subsetor_ibge}{Subsector name}
#' }
#' @source IBGE
"subsetor_ibge"

#' Lookup table of disability types
#'
#' A dataframe with numeric codes and labels of
#'  disability types as used in CAGED microdata.
#'
#' @format A dataframe with 8 rows and 2 variables:
#' \describe{
#' \item{codigo}{Numeric code}
#' \item{tipo_deficiencia}{Type of disability}
#' }
#' @source \url{ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/CAGEDEST_layout_Atualizado.xls}
"tipo_deficiencia"

#' Lookup table of admission/termination categories
#'
#' A dataframe with numeric codes and labels of
#'  admission/termination categories
#'
#' @format A dataframe with 15 rows and 2 variables:
#' \describe{
#' \item{codigo}{Numeric code}
#' \item{tipo_mov_desag}{Admission or termination type}
#' }
#' @source \url{ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/CAGEDEST_layout_Atualizado.xls}
"tipo_mov_desag"


#' Lookup table of Brazilian Federative Units (UFs) codes and abbreviations
#'
#' A dataframe with numeric codes and abbreviations of Brazilian States and
#' Federal District.
#'
#' @format A dataframe with 27 rows and 2 variables:
#' \describe{
#' \item{codigo}{Numeric code}
#' \item{sigla_uf}{Abbreviation}
#' }
"uf"
