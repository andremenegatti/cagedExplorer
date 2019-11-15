get_grande_setor_ibge <- function(x = codigo_subsetor_ibge, return_factor = TRUE) {

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

