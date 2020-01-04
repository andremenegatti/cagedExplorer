#' Returns string in upper-case without accents, tildes and cedillas
#'
#' \code{clean_text} converts the provided string to upper-case
#' and removes accents, tildes and cedillas.
#'
#' @param string A character string
#'
#' @return A clean, upper-case version of provided \code{string}
#'
#' @examples
#' clean_text('São Paulo')
#' clean_text('Mogi-Mirim')
#' clean_text(c('Carapicuíba', 'Tibiriçá', 'SERTÃOZINHO'))
clean_text <- function(string) {
  toupper(string) %>%
    str_replace_all('\u00c3', 'A') %>%
    str_replace_all('\u00d5', 'O') %>%
    str_replace_all('\u00c9', 'E') %>%
    str_replace_all('\u00c1', 'A') %>%
    str_replace_all('\u00d3', 'O') %>%
    str_replace_all('\u00c7', 'C') %>%
    str_replace_all('\u00cd', 'I') %>%
    str_replace_all('\u00c2', 'A') %>%
    str_replace_all('\u00ca', 'E') %>%
    str_replace_all('\u00d4', 'O') %>%
    str_replace_all('\u00da', 'U')
}
