#' Converts character string to uppercase and removes accents, tildes and cedillas
#'
#' @param x string
#'
#' @return string
#' @export
#'
#' @examples
clean_text <- function(x) {
  toupper(x) %>%
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
