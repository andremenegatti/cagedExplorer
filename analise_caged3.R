devtools::load_all(".")

# Estoque RAIS dez/2018
df_estoque_regioes <- readRDS('C:/Users/Dell/Desktop/USP_Municipios/Dados/RAIS/df_estoque_regioes.rds')
df_estoque_regioes_setor <- readRDS('C:/Users/Dell/Desktop/USP_Municipios/Dados/RAIS/df_estoque_regioes_setor.rds')

# CAGED
df_caged <- read_caged_rds(data_folder = 'C:/Users/Dell/Desktop/USP_Municipios/Dados/CAGED/Clean',
                           inicio = '2019-01',
                           fim = '2019-09',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = FALSE)

# Calculando saldo CAGED por regiao
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
    var_perc_estoque_inicial = (estoque - lag(estoque)) / first(estoque) * 100,
    var_perc_estoque_mensal = (estoque - lag(estoque)) / lag(estoque) * 100
    ) %>%
  ungroup()

#### LINE PLOTS - FACETED ####
df_regioes %>%
  ggplot() +
  geom_line(
    aes(
      x = mes,
      y = estoque,
      group = 1
    )
  ) +
  facet_wrap(~ regiao_governo, scales = 'free')

#### DOTPLOTS ####
plot1 <- df_regioes %>%
  filter(mes == 9) %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, var_perc_estoque_inicial)) %>%
  mutate(sentido_saldo = ifelse(saldo > 0, 'Positivo', 'Negativo')) %>%
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
    breaks = seq(-0.5, 1.5, by = 0.25),
    labels = function(x) formatC(x, digits = 2, big.mark = '.', decimal.mark = ',') %>% str_c('%')
    ) +
  labs(
    x = 'Variação do estoque de empregos formais em relação a dez/2018',
    y = 'Região de Governo',
    title = 'Variação percentual do estoque de empregos formais',
    subtitle = 'Comparação do estoque em Set/2019 com o estoque em Dez/2018'
    # caption = 'Fonte: elaboração própria a partir de dados do CAGED e da RAIS.'
  )

plot1
ggsave('C:/Users/Dell/Desktop/variacao_percentual_estoque.png', height = 7, width = 6)



# Municipios
municipios_acumulado_12_meses <- df_caged %>%
  calcular_saldo(municipio, Codmun7, regiao_governo, pop2019) %>%
  arrange(desc(saldo))

# Agregando por regiao de governo
sp_sf_gov <- municipios_acumulado_12_meses %>%
  group_by(regiao_governo) %>%
  summarise(`Saldo de empregos` = sum(saldo),
            `Empregos gerados/100mil hab.` = sum(saldo) / sum(pop2019) * 100e+3,
            pop2019 = sum(pop2019)) %>%
  mutate(sentido_saldo = ifelse(`Saldo de empregos` > 0, 'Positivo', 'Negativo')) %>%
  add_geometry_regioes_gov()

plot2 <- sp_sf_gov %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, `Empregos gerados/100mil hab.`)) %>%
  ggplot() +
  geom_point(
    aes(
      x = `Empregos gerados/100mil hab.`,
      y = regiao_governo
    ),
    size = 4,
    col = 'steelblue'
  ) +
  geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
  custom_theme() +
  theme(legend.position = 'none')

plot2
ggplotly(plot2)

plot3 <- sp_sf_gov %>%
  filter(regiao_governo != 'São Paulo') %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, `Saldo de empregos`)) %>%
  ggplot() +
  geom_point(
    aes(
      x = `Saldo de empregos` / 1000,
      y = regiao_governo,
      col = sentido_saldo
    ),
    size = 4,
    alpha = 0.7
  ) +
  geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
  custom_theme() +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('red', 'steelblue')) +
  labs(
    x = 'Saldo de empregos (milhares de vagas)',
    y = 'Região de Governo',
    title = 'Geração de empregos em São Paulo em 2019',
    subtitle = 'Comparação entre as Regiões de Governo (exceto Capital)'
    # caption = 'Fonte: elaboração própria a partir de dados do CAGED e da RAIS.'
  )

plot3
ggsave('C:/Users/Dell/Desktop/dotplot_saldo_regioes_2019.png', height = 7, width = 6)
