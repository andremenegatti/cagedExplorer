decodificar_variaveis <- function(df, vars = 'all') {

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
