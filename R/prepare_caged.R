prepare_caged <- function(df) {
  df %>%
    filter(codigo_uf == 35) %>%
    mutate(setor = get_grande_setor_ibge(codigo_subsetor_ibge)) %>%
    join_dados_municipios() %>%
    decodificar_variaveis() %>%
    drop_codigos()
}
