devtools::load_all('.')
library(tmap)
library(sp)
library(sf)

#### IMPORTANDO DADOS ####
df_caged <- read_caged_rds(data_folder = 'C:/Users/Dell/Desktop/USP_Municipios/Dados/CAGED/Clean',
                           inicio = '2018-01',
                           fim = '2019-09',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = FALSE)

df_caged <- df_caged %>%
  filter(mes %in% 1:9)

#### AGREGANDO E CALCULANDO VARIAVEIS DE INTERESSE ####
# Saldo por municipio
municipios_acumulado <- df_caged %>%
  calcular_saldo(ano, municipio, Codmun7, regiao_governo, pop2019) %>%
  arrange(desc(saldo))

# Agregando por regiao de governo
sp_sf_gov <- municipios_acumulado %>%
  group_by(ano, regiao_governo) %>%
  summarise(saldo = sum(saldo)) %>%
  ungroup() %>%
  spread(key = ano, value = saldo) %>%
  rename(saldo2018 = `2018`,
         saldo2019 = `2019`) %>%
  mutate(var_perc_saldo = (saldo2019 - saldo2018) / saldo2018 * 100) %>%
  add_geometry_regioes_gov() %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, var_perc_saldo)) %>%
  mutate(sentido_variacao = ifelse(var_perc_saldo > 0, 'Positiva', 'Negativa') %>%
           as.factor())

#### DOTPLOTS ####
# Variacao percentual saldo primeiros 9 meses
t_breaks3 <- c(-134, -95, -32, -20, 0, 24, 64, 240, 431)
t_palette3 <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#DEEBF7", "#9ECAE1", "#4292C6", "#08519C")

sp_sf_gov %>%
  mutate(grupo = group_by_breaks(sp_sf_gov$var_perc_saldo, breaks = t_breaks3) %>%
           as.factor() %>%
           fct_reorder(sp_sf_gov$var_perc_saldo)) %>%
  ggplot() +
  geom_point(aes(x = var_perc_saldo,
                 y = regiao_governo,
                 fill = grupo),
             alpha = 0.8,
             size = 5,
             shape = 21,
             col = 'darkgray') +
  custom_theme() +
  theme(legend.position = 'none') +
  geom_vline(xintercept = 0, color = 'darkred', alpha = 0.5) +
  labs(x = 'Variação do saldo acumulado',
       y = 'Região de governo',
       title = 'Geração de empregos em SP nos primeiros 9 meses do ano',
       subtitle = 'Saldo acumulado entre jan-set/2019 como \npercentual do saldo acumulado entre jan-set/2018') +
  scale_fill_manual(values = t_palette3) +
  scale_x_continuous(breaks = seq(-100, 400, by = 100), labels = function(x) str_c(x, '%'))

# ggsave('C:/Users/Dell/Desktop/dotplot_comparacao_9meses.png', height = 8, width = 6.5)

#### MAPAS ####
# Modo estático
tmap_mode("plot")

# Saldo de salários
map1 <- sp_sf_gov %>%
  mutate(`Variação Percentual (%)` = var_perc_saldo) %>%
  mapa_regiao_saldo(var_saldo = "Variação Percentual (%)",
                    map_title = 'Regiões Gov. SP \nSaldo acumulado em jan-set/2019 como percentual do acumulado jan-set/2018',
                    map_breaks = t_breaks3,
                    map_palette = t_palette3)

# tmap_save(map1, filename = 'C:/Users/Dell/Desktop/mapa_comparacao_9meses.png', width = 7, height = 5.5)

