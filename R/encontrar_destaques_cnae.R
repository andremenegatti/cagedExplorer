encontrar_destaques_cnae <- function(df, positivos = TRUE, n_destaques = 5, filtrar_setor = FALSE) {

  if (filtrar_setor) {
    df <- filter(df, setor == filtrar_setor)
  }

  df <- group_by(df, setor)

  if (positivos) {
    df <- arrange(df, desc(saldo))
  } else {
    df <- arrange(df, saldo)
  }

    df <- slice(df, 1:n_destaques) %>%
    ungroup()

    df

}
