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

df_destaques <- df_caged %>%
  calcular_saldo(municipio) %>%
  arrange(desc(saldo)) %>%
  slice(1:5, 641:645)

df_destaques_setor <- df_caged %>%
  calcular_saldo(municipio, setor) %>%
  group_by(municipio) %>%
  mutate(saldo_total = sum(saldo)) %>%
  ungroup() %>%
  arrange(desc(saldo_total)) %>%
  filter(municipio %in% df_destaques$municipio)

df_destaques_setor %>% write.csv('C:/Users/Dell/Desktop/destaques_municipios.csv', row.names = FALSE)

municipios_acumulado_12_meses <- municipios_acumulado_12_meses %>%
  add_geometry_municipios()




reds_full <- c("#67001F", "#B2182B", "#D6604D",
               "#F4A582", "#FDDBC7", "#F7F7F7",
               "#D1E5F0", "#92C5DE", "#4393C3",
               "#2166AC", "#053061")

blues_full <- c("#F7FBFF", "#DEEBF7", "#C6DBEF",
                "#9ECAE1", "#6BAED6", "#4292C6",
                "#2171B5", "#08519C", "#08306B")



df <- municipios_acumulado_12_meses
df$`Saldo de Empregos` <- df$saldo

var_saldo = "Saldo de Empregos"
map_title = "Geração de Emprego nos Municípios Paulistas - Nov/2018 a Out/2019"

map_breaks <- c(min(df$`Saldo de Empregos`), -500, -100, 0, 100, 500, 2000, 4000, max(df$`Saldo de Empregos`))
map_palette <- c(reds_full[c(2, 4, 5)], blues_full[4:7], blues_full[9])

map_mun <- tm_shape(df) +
  tm_style("beaver",
           legend.format = list(fun = format_map_legend,
                                text.separator = " a ")) +
  tm_fill(var_saldo,
          palette = map_palette,
          style = 'fixed',
          breaks = map_breaks,
          alpha = 1,
          id = "regiao_governo") +
  tm_layout(main.title.size = 1.2,
            fontfamily = 'serif',
            main.title.fontface = 'bold',
            scale = 1.1,
            bg.color = "white",
            inner.margins = c(.1, .1, .1, .1),
            main.title = map_title) +
  tm_compass(north = 0,
             type = "8star",
             size = 2,
             position = c("right", "bottom")) +
  tm_scale_bar(text.size = 0.6,
               text.color = NA,
               lwd = 1,
               color.dark = "black",
               color.light = "white") +
  tm_legend(legend.position = c(0.01,0.08)) +
  tm_borders(col = "black", lwd = 0.3)


tmap_save(map_mun, filename = 'C:/Users/Dell/Desktop/mapa_saldo_vagas_municipios.png', width = 7, height = 5.5)
