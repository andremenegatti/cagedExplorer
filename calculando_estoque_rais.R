
txt_file <- 'C:/Users/Dell/Desktop/USP_Municipios/Dados/RAIS/RAIS_VINC_PUB_SP.txt'

df_rais_sp <-
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
               `Vínculo Ativo 31/12` = col_integer(),
               `Município` = col_integer(),
               `Mun Trab` = col_integer(),
               `CNAE 2.0 Classe` = col_integer(),
               `Idade` = col_integer(),
               `Faixa Etária` = col_integer(),
               `Qtd Hora Contr` = col_integer(),
               `Faixa Hora Contrat` = col_integer(),
               `Raça Cor` = col_integer(),
               `Sexo Trabalhador` = col_integer(),
               `Tamanho Estabelecimento` = col_integer(),
               `Vl Remun Dezembro Nom` = col_double(),
               `Vl Remun Dezembro (SM)` = col_double(),
               `Vl Remun Média Nom` = col_double(),
               `Vl Remun Média (SM)` = col_double(),
               `Faixa Remun Dezem (SM)` = col_integer(),
               `Faixa Remun Média (SM)` = col_integer(),
               `Tipo Vínculo` = col_integer(),
               `IBGE Subsetor` = col_integer(),
               `Tipo Salário` = col_integer(),
               `Mês Admissão` = col_integer(),
               `Mês Desligamento` = col_integer()
             )
  ) %>%
  filter(str_detect(Município, '35\\d{4}'))

df_rais_sp_ativos <- df_rais %>%
  filter(`Vínculo Ativo 31/12` == 1)

df_rais_sp_ativos2 <- df_rais_sp_ativos %>%
  select(codigo_municipio = `Município`,
         codigo_muncipio_trab = `Mun Trab`,
         faixa_etaria = `Faixa Etária`,
         faixa_horas = `Faixa Hora Contrat`,
         faixa_remun_dez_sm = `Faixa Remun Dezem (SM)`,
         horas_contrato = `Qtd Hora Contr`,
         idade = `Idade`,
         mes_admissao = `Mês Admissão`,
         mes_desligamento = `Mês Desligamento`,
         remun_dez_nom = `Vl Remun Dezembro Nom`,
         remun_dez_sm = `Vl Remun Dezembro (SM)`,
         remun_med_nom = `Vl Remun Média Nom`,
         remun_med_sm = `Vl Remun Média (SM)`,
         codigo_raca_cor = `Raça Cor`,
         codigo_sexo = `Sexo Trabalhador`,
         codigo_faixa_tamanho_estab = `Tamanho Estabelecimento`,
         codigo_tipo_vinculo = `Tipo Vínculo`,
         codigo_subsetor_ibge = `IBGE Subsetor`,
         codigo_cnae = `CNAE 2.0 Classe`,
         tipo_salario = `Tipo Salário`
  )

saveRDS(df_rais_sp_ativos2,
        'C:/Users/Dell/Desktop/USP_Municipios/Dados/RAIS/vinculos_ativos_31_12_2017_sp_clean.rds')

df_rais_sp_ativos2 <- df_rais_sp_ativos2 %>%
  left_join(municipios_sp %>%
              select(codigo_municipio = codigo,
                     municipio_clean,
                     regiao_governo),
            by = 'codigo_municipio')

df_rais_sp_ativos2 <- df_rais_sp_ativos2 %>%
  mutate(setor = get_grande_setor_ibge(codigo_subsetor_ibge, return_factor = FALSE))

df_estoque_municipios_setor <- df_rais_sp_ativos2 %>%
  group_by(codigo_municipio, municipio_clean, regiao_governo, setor) %>%
  tally() %>%
  ungroup()

saveRDS(df_estoque_municipios_setor, 'C:/Users/Dell/Desktop/USP_Municipios/Dados/RAIS/df_estoque_municipios_setor.rds')

df_estoque_regioes_setor <- df_estoque_municipios_setor %>%
  group_by(regiao_governo, setor) %>%
  summarise(n = sum(n)) %>%
  ungroup()

df_estoque_regioes_setor %>%
  filter(setor == 'Agropecuária') %>%
  select(n) %>%
  unlist() %>%
  sum()

saveRDS(df_estoque_regioes_setor, 'C:/Users/Dell/Desktop/USP_Municipios/Dados/RAIS/df_estoque_regioes_setor.rds')

