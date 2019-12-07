devtools::load_all('.')
library(tmap)
library(sp)
library(sf)

#### IMPORTANDO DADOS ####
df_caged <- read_caged_rds(data_folder = 'C:/Users/Dell/Desktop/USP_Municipios/Dados/CAGED/Clean',
                           inicio = '2018-11',
                           fim = '2019-10',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = FALSE)

#### AGREGANDO E CALCULANDO VARIAVEIS DE INTERESSE ####

df_destaques_mun <- df_caged %>%
  calcular_saldo(municipio) %>%
  arrange(desc(saldo)) %>%
  slice(1:5, 641:645)

df_destaques_setor_mun <- df_caged %>%
  calcular_saldo(municipio, setor) %>%
  group_by(municipio) %>%
  mutate(saldo_total = sum(saldo)) %>%
  ungroup() %>%
  arrange(desc(saldo_total)) %>%
  filter(municipio %in% df_destaques_mun$municipio)

# df_destaques_setor_mun %>% write.csv('C:/Users/Dell/Desktop/destaques_municipios.csv', row.names = FALSE)

df_destaques_regioes <- df_caged %>%
  calcular_saldo(regiao_governo) %>%
  arrange(desc(saldo)) %>%
  slice(1:5, 39:43)

df_destaques_regioes_setor <- df_caged %>%
  calcular_saldo(regiao_governo, setor) %>%
  group_by(regiao_governo) %>%
  mutate(saldo_total = sum(saldo)) %>%
  ungroup() %>%
  arrange(desc(saldo_total)) %>%
  semi_join(df_destaques_regioes, by = 'regiao_governo')

# df_destaques_regioes_setor %>% write.csv('C:/Users/Dell/Desktop/destaques_regioes.csv', row.names = FALSE)

df_saldo_sede_setor <- df_caged %>%
  calcular_saldo(municipio, regiao_governo, setor) %>%
  filter(as.character(municipio) == as.character(regiao_governo)) %>%
  group_by(municipio) %>%
  mutate(saldo_total_sede = sum(saldo)) %>%
  ungroup() %>%
  rename(vagas_criadas_setor_sede = vagas_criadas,
         vagas_destruidas_setor_sede = vagas_destruidas,
         saldo_setor_sede = saldo) %>%
  select(-municipio)

df_destaques_regioes_setor <- df_destaques_regioes_setor %>%
  rename(vagas_criadas_setor_regiao = vagas_criadas,
         vagas_destruidas_setor_regiao = vagas_destruidas,
         saldo_setor_regiao = saldo,
         saldo_total_regiao = saldo_total) %>%
  left_join(df_saldo_sede_setor, by = c('regiao_governo', 'setor'))

# df_destaques_regioes_setor %>% write.csv('C:/Users/Dell/Desktop/destaques_regioes.csv', row.names = FALSE)
