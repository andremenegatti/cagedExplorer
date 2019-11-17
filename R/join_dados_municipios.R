join_dados_municipios <- function(df) {

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
