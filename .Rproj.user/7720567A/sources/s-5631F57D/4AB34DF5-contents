read_caged <- function(txt_file, pretty = TRUE, ...) {

  df_caged <-
    read_delim(file = txt_file,
               delim = ";",
               trim_ws = TRUE,
               escape_double = FALSE,
               locale = locale(
                 decimal_mark = ",",
                 grouping_mark = ".",
                 encoding = "ISO-8859-1"
               ),
               col_types = cols_only(
                 `Município` = col_integer(),
                 `CNAE 2.0 Classe` = col_integer(),
                 `Faixa Empr Início Jan` = col_integer(),
                 `Grau Instrução` = col_integer(),
                 `Qtd Hora Contrat` = col_integer(),
                 `IBGE Subsetor` = col_integer(),
                 `Idade` = col_integer(),
                 `Raça Cor` = col_integer(),
                 `Salário Mensal` = col_double(),
                 `Saldo Mov` = col_integer(),
                 `Sexo` = col_integer(),
                 `Tempo Emprego` = col_double(),
                 `UF` = col_integer()
               ), ...
    )

  if (pretty) {
    df_caged <- df_caged %>%
      select(codigo_municipio = `Município`,
             codigo_uf = `UF`,
             movimento = `Saldo Mov`,
             codigo_subsetor_ibge = `IBGE Subsetor`,
             codigo_cnae = `CNAE 2.0 Classe`,
             codigo_faixa_tamanho_estab = `Faixa Empr Início Jan`,
             salario = `Salário Mensal`,
             codigo_grau_instrucao = `Grau Instrução`,
             horas_contrato = `Qtd Hora Contrat`,
             tempo_emprego = `Tempo Emprego`,
             codigo_raca_cor = `Raça Cor`,
             codigo_sexo = `Sexo`,
             idade = `Idade`)
  }

  df_caged

}
