library(cagedExplorer)

# Carregando bases para acumulado 12 meses
df_ano_atual <- read_caged_rds(data_folder = '~/Desktop/Clean', inicio = '2018-11', fim = '2019-10')
df_ano_anterior <- read_caged_rds(data_folder = '~/Desktop/Clean', inicio = '2017-11', fim = '2018-10')

# Separando dados do mes
df_mes_ano_atual <- filter(df_ano_atual, mes == mes_atual)
df_mes_ano_anterior <- filter(df_ano_anterior, mes == mes_atual)

# Definindo variaveis
mes_atual <- 10
ano_atual <- 2019
ano_anterior <- 2018
output_folder <- '~/Desktop/Tabelas'
mun <- 'RIBEIRAO PRETO'

#### MUNICIPIOS ####

# Mes, ano atual
saldo_mes_ano_atual <- df_mes_ano_atual %>%
  filter(municipio_clean == mun) %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_atual <- encontrar_destaques_cnae(df = saldo_mes_ano_atual)
destaques_negativos_mes_ano_atual <- encontrar_destaques_cnae(df = saldo_mes_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_atual, str_c(output_folder, '/destaques_positivos_', mun, '_', ano_atual, '_', mes_atual, '.csv'))
write.csv(destaques_negativos_mes_ano_atual, str_c(output_folder, '/destaques_negativos_', mun, '_', ano_atual, '_', mes_atual, '.csv'))

# Mes, ano anterior
saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
  filter(municipio_clean == mun) %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_anterior <- encontrar_destaques_cnae(df = saldo_mes_ano_anterior)
destaques_negativos_mes_ano_anterior <- encontrar_destaques_cnae(df = saldo_mes_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_anterior, str_c(output_folder, '/destaques_positivos_', mun, '_', ano_anterior, '_', mes_atual, '.csv'))
write.csv(destaques_negativos_mes_ano_anterior, str_c(output_folder, '/destaques_negativos_', mun, '_', ano_anterior, '_', mes_atual, '.csv'))

# Acumulado 12 meses, ano atual
saldo_acum_ano_atual <- df_ano_atual %>%
  filter(municipio_clean == mun) %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_atual <- encontrar_destaques_cnae(df = saldo_acum_ano_atual)
destaques_negativos_acum_ano_atual <- encontrar_destaques_cnae(df = saldo_acum_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual, str_c(output_folder, '/destaques_positivos_acumulado_', mun, '_', ano_atual, '.csv'))
write.csv(destaques_negativos_acum_ano_atual, str_c(output_folder, '/destaques_negativos_acumulado_', mun, '_', ano_atual, '.csv'))

# Acumulado 12 meses, ano anterior
saldo_acum_ano_anterior <- df_ano_anterior %>%
  filter(municipio_clean == mun) %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_anterior <- encontrar_destaques_cnae(df = saldo_acum_ano_anterior)
destaques_negativos_acum_ano_anterior <- encontrar_destaques_cnae(df = saldo_acum_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual, str_c(output_folder, '/destaques_positivos_acumulado_', mun, '_', ano_anterior, '.csv'))
write.csv(destaques_negativos_acum_ano_atual, str_c(output_folder, '/destaques_negativos_acumulado_', mun, '_', ano_anterior, '.csv'))

#### REGIAO ADM RIBEIRAO ####

# Mes, ano atual
saldo_mes_ano_atual <- df_mes_ano_atual %>%
  filter(regiao_administrativa == 'Ribeirão Preto') %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_atual <- encontrar_destaques_cnae(df = saldo_mes_ano_atual)
destaques_negativos_mes_ano_atual <- encontrar_destaques_cnae(df = saldo_mes_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_atual, str_c(output_folder, '/destaques_positivos_REGIAO ADM RIBEIRAO_', ano_atual, '_', mes_atual, '.csv'))
write.csv(destaques_negativos_mes_ano_atual, str_c(output_folder, '/destaques_negativos_REGIAO ADM RIBEIRAO_', ano_atual, '_', mes_atual, '.csv'))

# Mes, ano anterior
saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
  filter(regiao_administrativa == 'Ribeirão Preto') %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_anterior <- encontrar_destaques_cnae(df = saldo_mes_ano_anterior)
destaques_negativos_mes_ano_anterior <- encontrar_destaques_cnae(df = saldo_mes_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_anterior, str_c(output_folder, '/destaques_positivos_REGIAO ADM RIBEIRAO_', ano_anterior, '_', mes_atual, '.csv'))
write.csv(destaques_negativos_mes_ano_anterior, str_c(output_folder, '/destaques_negativos_REGIAO ADM RIBEIRAO_', ano_anterior, '_', mes_atual, '.csv'))

# Acumulado 12 meses, ano atual
saldo_acum_ano_atual <- df_ano_atual %>%
  filter(regiao_administrativa == 'Ribeirão Preto') %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_atual <- encontrar_destaques_cnae(df = saldo_acum_ano_atual)
destaques_negativos_acum_ano_atual <- encontrar_destaques_cnae(df = saldo_acum_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual, str_c(output_folder, '/destaques_positivos_acumulado_REGIAO ADM RIBEIRAO_', ano_atual, '.csv'))
write.csv(destaques_negativos_acum_ano_atual, str_c(output_folder, '/destaques_negativos_acumulado_REGIAO ADM RIBEIRAO_', ano_atual, '.csv'))

# Acumulado 12 meses, ano anterior
saldo_acum_ano_anterior <- df_ano_anterior %>%
  filter(regiao_administrativa == 'Ribeirão Preto') %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_anterior <- encontrar_destaques_cnae(df = saldo_acum_ano_anterior)
destaques_negativos_acum_ano_anterior <- encontrar_destaques_cnae(df = saldo_acum_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual, str_c(output_folder, '/destaques_positivos_acumulado_REGIAO ADM RIBEIRAO_', ano_anterior, '.csv'))
write.csv(destaques_negativos_acum_ano_atual, str_c(output_folder, '/destaques_negativos_acumulado_REGIAO ADM RIBEIRAO_', ano_anterior, '.csv'))


#### ESTADO SP ####

# Mes, ano atual
saldo_mes_ano_atual <- df_mes_ano_atual %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_atual <- encontrar_destaques_cnae(df = saldo_mes_ano_atual)
destaques_negativos_mes_ano_atual <- encontrar_destaques_cnae(df = saldo_mes_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_atual, str_c(output_folder, '/destaques_positivos_ESTADO SP_', ano_atual, '_', mes_atual, '.csv'))
write.csv(destaques_negativos_mes_ano_atual, str_c(output_folder, '/destaques_negativos_ESTADO SP_', ano_atual, '_', mes_atual, '.csv'))

# Mes, ano anterior
saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_anterior <- encontrar_destaques_cnae(df = saldo_mes_ano_anterior)
destaques_negativos_mes_ano_anterior <- encontrar_destaques_cnae(df = saldo_mes_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_anterior, str_c(output_folder, '/destaques_positivos_ESTADO SP_', ano_anterior, '_', mes_atual, '.csv'))
write.csv(destaques_negativos_mes_ano_anterior, str_c(output_folder, '/destaques_negativos_ESTADO SP_', ano_anterior, '_', mes_atual, '.csv'))

# Acumulado 12 meses, ano atual
saldo_acum_ano_atual <- df_ano_atual %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_atual <- encontrar_destaques_cnae(df = saldo_acum_ano_atual)
destaques_negativos_acum_ano_atual <- encontrar_destaques_cnae(df = saldo_acum_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual, str_c(output_folder, '/destaques_positivos_acumulado_ESTADO SP_', ano_atual, '.csv'))
write.csv(destaques_negativos_acum_ano_atual, str_c(output_folder, '/destaques_negativos_acumulado_ESTADO SP_', ano_atual, '.csv'))

# Acumulado 12 meses, ano anterior
saldo_acum_ano_anterior <- df_ano_anterior %>%
  calcular_saldo(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_anterior <- encontrar_destaques_cnae(df = saldo_acum_ano_anterior)
destaques_negativos_acum_ano_anterior <- encontrar_destaques_cnae(df = saldo_acum_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual, str_c(output_folder, '/destaques_positivos_acumulado_ESTADO SP_', ano_anterior, '.csv'))
write.csv(destaques_negativos_acum_ano_atual, str_c(output_folder, '/destaques_negativos_acumulado_ESTADO SP_', ano_anterior, '.csv'))
