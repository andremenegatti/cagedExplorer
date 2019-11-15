calcular_saldo <- function(df, ...) {

  df %>%
    group_by(...) %>%
    summarise(vagas_criadas = sum(movimento == 1),
              vagas_destruidas = sum(movimento == -1),
              saldo = sum(movimento)) %>%
    ungroup()

}
