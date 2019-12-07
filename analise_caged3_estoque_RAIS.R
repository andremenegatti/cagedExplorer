devtools::load_all(".")

# Estoque RAIS dez/2018
df_estoque_regioes <- readRDS('C:/Users/Dell/Desktop/USP_Municipios/Dados/RAIS/df_estoque_regioes.rds')
df_estoque_regioes_setor <- readRDS('C:/Users/Dell/Desktop/USP_Municipios/Dados/RAIS/df_estoque_regioes_setor.rds')

# CAGED
df_caged <- read_caged_rds(data_folder = 'C:/Users/Dell/Desktop/USP_Municipios/Dados/CAGED/Clean',
                           inicio = '2019-01',
                           fim = '2019-10',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = FALSE)

# Saldo acumulado 2019 por regiao CAGED
saldo_2019_regioes <- df_caged %>%
  calcular_saldo(regiao_governo) %>%
  mutate(regiao_governo = as.character(regiao_governo)) %>%
  select(regiao_governo, saldo2019 = saldo)

# Calculando saldo mensal CAGED por regiao
saldo_mensal_regioes <- df_caged %>%
  calcular_saldo(regiao_governo, mes) %>%
  mutate(regiao_governo = as.character(regiao_governo))

# Juntando dados de estoque RAIS com os dados do CAGED
# Total de empregos no início do ano foi incluido na coluna 'saldo', mas para mes = 0
saldo_mensal_regioes2 <- df_estoque_regioes %>%
  mutate(mes = 0) %>%
  select(regiao_governo, mes, saldo = n) %>%
  bind_rows(saldo_mensal_regioes) %>%
  arrange(regiao_governo, mes)

# Criando variavel com estoque mensal acumulado
df_regioes <- saldo_mensal_regioes2 %>%
  split(saldo_mensal_regioes2$regiao_governo) %>%
  map(.f = ~ .x %>% mutate(estoque = accumulate(saldo, sum))) %>%
  bind_rows() %>%
  # Criando variaveis de variacao percentual
  group_by(regiao_governo) %>%
  mutate(
    var_perc_estoque_inicial = (estoque - first(estoque)) / first(estoque) * 100,
    var_perc_estoque_mensal = (estoque - lag(estoque)) / lag(estoque) * 100
    ) %>%
  ungroup()

#### LINE PLOTS - FACETED ####
# df_regioes %>%
#   ggplot() +
#   geom_line(
#     aes(
#       x = mes,
#       y = estoque,
#       group = 1
#     )
#   ) +
#   facet_wrap(~ regiao_governo, scales = 'free')

#### DOTPLOTS ####
plot1 <- df_regioes %>%
  filter(mes == 9) %>%
  left_join(saldo_2019_regioes, by = 'regiao_governo') %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, var_perc_estoque_inicial)) %>%
  mutate(sentido_saldo = ifelse(saldo2019 > 0, 'Positivo', 'Negativo')) %>%
  ggplot() +
  geom_point(
    aes(
      x = var_perc_estoque_inicial,
      y = regiao_governo,
      col = sentido_saldo
    ),
    size = 4,
    alpha = 0.7
  ) +
  geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
  custom_theme() +
  theme(legend.position = 'bottom') +
  scale_color_manual(name = 'Saldo de vagas em 2019', values = c('red', 'steelblue')) +
  scale_x_continuous(
    # breaks = seq(-3, 9, by = 1.5),
    labels = function(x) formatC(x, digits = 2, big.mark = '.', decimal.mark = ',') %>% str_c('%')
    ) +
  labs(
    x = 'Variação do estoque de empregos formais em relação a dez/2018',
    y = 'Região de Governo',
    title = 'Variação percentual do estoque de empregos formais',
    subtitle = 'Comparação do estoque em Out/2019 com o estoque em Nov/2018'
    # caption = 'Fonte: elaboração própria a partir de dados do CAGED e da RAIS.'
  )

plot1
# ggsave('C:/Users/Dell/Desktop/variacao_percentual_estoque.png', height = 7, width = 6)

# Cores de acordo com o mapa
map_breaks <- c(-3.3, 0, 2, 4, 6, 8.5)
map_palette <- c( "#B2182B", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B")

plot2 <- df_regioes %>%
  filter(mes == 9) %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, var_perc_estoque_inicial)) %>%
  mutate(grupo = group_by_breaks(var_perc_estoque_inicial,
                                 breaks = map_breaks) %>%
           as.factor() %>%
           fct_reorder(var_perc_estoque_inicial)) %>%
  ggplot() +
  geom_point(
    aes(
      x = var_perc_estoque_inicial,
      y = regiao_governo,
      fill = grupo
    ),
    size = 4,
    alpha = 0.9,
    shape = 21,
    col = 'gray'
  ) +
  geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
  custom_theme() +
  theme(legend.position = 'none') +
  scale_fill_manual(values = map_palette) +
  scale_x_continuous(
    # breaks = seq(-3, 9, by = 1.5),
    labels = function(x) formatC(x, digits = 2, big.mark = '.', decimal.mark = ',') %>% str_c('%')
  ) +
  labs(
    x = 'Variação do estoque de empregos formais em relação a dez/2018',
    y = 'Região de Governo',
    title = 'Variação percentual do estoque de empregos formais',
    subtitle = 'Comparação do estoque em Out/2019 com o estoque em Dez/2018'
    # caption = 'Fonte: elaboração própria a partir de dados do CAGED e da RAIS.'
  )

plot2
# ggsave('C:/Users/Dell/Desktop/variacao_percentual_estoque.png', height = 7, width = 6)


#### MAPAS ####
library(sf)
library(tmap)

map1 <- df_regioes %>%
  filter(mes == 9) %>%
  add_geometry_regioes_gov() %>%
  mutate(`Variação Estoque (%)` = var_perc_estoque_inicial) %>%
  mapa_regiao_saldo(var_saldo = 'Variação Estoque (%)',
                    map_breaks = map_breaks,
                    map_palette = map_palette,
                    map_title = 'Regiões Gov. SP - Variação Estoque de Empregos Formais - Jan-Out/2019')

# tmap_save(map1, filename = 'C:/Users/Dell/Desktop/mapa_estoque.png', width = 7, height = 5.5)
