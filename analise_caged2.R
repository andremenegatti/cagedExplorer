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
# Saldo por municipio
municipios_acumulado_12_meses <- df_caged %>%
  calcular_saldo(municipio, Codmun7, regiao_governo, pop2019) %>%
  arrange(desc(saldo))

# Agregando por regiao de governo
sp_sf_gov <- municipios_acumulado_12_meses %>%
  group_by(regiao_governo) %>%
  summarise(`Saldo de empregos` = sum(saldo),
            `Empregos gerados/100mil hab.` = sum(saldo) / sum(pop2019) * 100e+3,
            pop2019 = sum(pop2019)) %>%
  add_geometry_regioes_gov()

# Salario medio/massa salarial das vagas criadas
reg_gov_criadas <- df_caged %>%
  filter(movimento == 1) %>%
  group_by(regiao_governo) %>%
  summarise(vagas_criadas = sum(abs(movimento)),
            massa_salarial_criadas = sum(salario),
            salario_medio_criadas = massa_salarial_criadas / vagas_criadas) %>%
  ungroup()

# Salario medio/massa salarial das vagas destruidas
reg_gov_destruidas <- df_caged %>%
  filter(movimento == -1) %>%
  group_by(regiao_governo) %>%
  summarise(vagas_destruidas = sum(abs(movimento)),
            massa_salarial_destruidas = sum(salario),
            salario_medio_destruidas = massa_salarial_destruidas / vagas_destruidas) %>%
  ungroup()

# Juntando ultimos dois DF e criando novas variaveis
reg_gov_salario <- reg_gov_criadas %>%
  inner_join(reg_gov_destruidas, by = 'regiao_governo') %>%
  mutate(saldo_vagas = vagas_criadas - vagas_destruidas,
         saldo_massa_salarial = massa_salarial_criadas - massa_salarial_destruidas,
         diferenca_salario_medio = salario_medio_criadas - salario_medio_destruidas) %>%
  inner_join(sp_sf_gov %>% as.data.frame() %>%  select(regiao_governo, pop2019),
             by = 'regiao_governo') %>%
  mutate(var_perc_massa_salarial = saldo_massa_salarial / massa_salarial_destruidas * 100) %>%
  mutate(sentido_saldo = ifelse(saldo_vagas > 0, 'Positivo', 'Negativo') %>%
           as.factor())

#### DOTPLOTS ####
# Saldo
reg_gov_salario %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, var_perc_massa_salarial)) %>%
  ggplot() +
  geom_point(aes(x = var_perc_massa_salarial,
                 y = regiao_governo,
                 size = massa_salarial_criadas/1e+6,
                 col = sentido_saldo),
             alpha = 0.8) +
  custom_theme() +
  geom_vline(xintercept = 0, color = 'darkred', alpha = 0.5) +
  labs(x = '',
       y = 'Região de governo',
       title = 'Soma dos salários das vagas criadas como \npercentual da soma dos salários das vagas destruídas',
       subtitle = 'Comparação entre Regiões de Governo de SP - Nov/2018 a Out/2019') +
  scale_size_continuous(name = 'Total de salários criados',
                        breaks = c(500, 1000, 3000, 5000),
                        labels = c('R$ 500 milhões', 'R$ 1 bilhão', 'R$ 3 bilhões', 'R$ 5 bilhões')) +
  scale_color_manual(name = 'Saldo de vagas',
                     # values = c("#D6604D", "#2171B5"),
                     values = c('red', 'steelblue')) +
  scale_x_continuous(labels = function(x) str_c(x, '%'))

# ggsave('C:/Users/Dell/Desktop/variacao_massa_salarial.png', height = 8, width = 6)

# Saldo 100 mil hab
saldo_100mil_breaks <- get_breaks(sp_sf_gov$`Empregos gerados/100mil hab.`)
saldo_100mil_palette <- get_palette(saldo_100mil_breaks)
saldo_100mil_breaks2 <- c(floor(min(sp_sf_gov$`Empregos gerados/100mil hab.`)), saldo_100mil_breaks[c(2:5, 7:10)])
saldo_100mil_palette2 <- get_palette(saldo_100mil_breaks2)
saldo_100mil_palette3 <- c(saldo_100mil_palette2[c(1:4, 6)], "#6BAED6","#2171B5", "#08306B")

sp_sf_gov %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, `Empregos gerados/100mil hab.`)) %>%
  mutate(
    grupo_saldo = group_by_breaks(`Empregos gerados/100mil hab.`,
                                  breaks = saldo_100mil_breaks2) %>%
      as.factor() %>%
      fct_reorder(`Empregos gerados/100mil hab.`)
    ) %>%
  ggplot() +
  geom_point(
    aes(
      x = `Empregos gerados/100mil hab.`,
      y = regiao_governo,
      fill = grupo_saldo
    ),
    size = 5,
    shape = 21,
    col = 'gray'
  ) +
  geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
  scale_fill_manual(values = saldo_100mil_palette3) +
  custom_theme() +
  theme(legend.position = 'none') +
  labs(
    y = 'Região de Governo',
    title = 'Geração de Empregos por 100 mil habitantes',
    subtitle = 'Comparação entre Regiões de Governo de SP - Nov/2018 a Out/2019'
  )

# ggsave('C:/Users/Dell/Desktop/dotplot_empregos_100mil_hab.png', height = 8, width = 6)



# Saldo vagas
saldo_breaks <- get_breaks(sp_sf_gov$`Saldo de empregos`)
saldo_palette <- get_palette(saldo_breaks)
saldo_breaks2 <- saldo_breaks[c(1:5, 7:10)]
saldo_palette2 <- get_palette(saldo_breaks2)
saldo_palette3 <- c(saldo_palette2[c(1:4, 6)], "#6BAED6","#2171B5", "#08306B")

sp_sf_gov %>%
  mutate(
    grupo_saldo = group_by_breaks(sp_sf_gov$`Saldo de empregos`, saldo_breaks2) %>%
      as.factor() %>%
      fct_reorder(`Saldo de empregos`)
  ) %>%
  filter(regiao_governo != 'São Paulo') %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, `Saldo de empregos`)) %>%
  ggplot() +
  geom_point(
    aes(
      x = `Saldo de empregos`/1000,
      y = regiao_governo,
      fill = grupo_saldo
    ),
    size = 5,
    shape = 21,
    col = 'gray'
  ) +
  geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
  scale_fill_manual(values = saldo_palette3) +
  custom_theme() +
  theme(legend.position = 'none') +
  labs(
    x = 'Saldo de empregos (milhares de vagas)',
    y = 'Região de Governo',
    title = 'Geração de Empregos em São Paulo em 2019',
    subtitle = 'Comparação entre Regiões de Governo (exceto Capital)'
  )

# ggsave('C:/Users/Dell/Desktop/dotplot_saldo_vagas_12meses.png', height = 8, width = 6)


#### MAPAS ####
# Modo estático
tmap_mode("plot")

# Saldo de salários
map1 <- reg_gov_salario %>%
  mutate(`Saldo (milhares de R$)` = saldo_massa_salarial / 1e+3) %>%
  add_geometry_regioes_gov() %>%
  mapa_regiao_saldo(var_saldo = 'Saldo (milhares de R$)',
                    map_title = 'Regiões Gov. SP - Saldo de Salários - Acumulado Nov/2018 a Out/2019',
                    map_palette = c("#67001F", "#D6604D", "#F4A582", "#FDDBC7", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#2171B5", "#08306B"))
# tmap_save(map1, filename = 'C:/Users/Dell/Desktop/mapa_massa_salarial_absoluta.png', width = 7, height = 5.5)

# Saldo de vagas
map2 <- mapa_regiao_saldo(sp_sf_gov,
                          var_saldo = "Saldo de empregos",
                          map_title = 'Regiões Gov. SP - Geração de empregos - Acumulado Nov/2018-Out/2019',
                          map_breaks = saldo_breaks2,
                          map_palette = saldo_palette3)
# tmap_save(map2, filename = 'C:/Users/Dell/Desktop/mapa_saldo.png', width = 7, height = 5.5)

# Saldo de vagas/100 mil hab
map3 <- mapa_regiao_saldo(sp_sf_gov,
                          var_saldo = "Empregos gerados/100mil hab.",
                          map_title = 'Regiões Gov. SP - Empregos gerados/100mil hab - Nov/2018 a Out/2019',
                          map_breaks = saldo_100mil_breaks2,
                          map_palette = saldo_100mil_palette3)
# tmap_save(map3, filename = 'C:/Users/Dell/Desktop/mapa_saldo_100mil_hab_12_meses.png', width = 7, height = 5.5)

# Total salários vagas criadas
map4 <- reg_gov_salario %>%
  mutate(`Salários criados (milhões de R$)` = massa_salarial_criadas / 1e+6) %>%
  add_geometry_regioes_gov() %>%
  mapa_regiao_salario(var_saldo = 'Salários criados (milhões de R$)',
                      map_title = 'Regiões Gov. SP - Soma dos salários das vagas criadas - Nov/2018 a Out/2019')
# tmap_save(map4, filename = 'C:/Users/Dell/Desktop/mapa_salarios_criados.png', width = 7, height = 5.5)
