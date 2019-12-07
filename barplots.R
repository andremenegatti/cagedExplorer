library(cagedExplorer)

#### IMPORTANDO DADOS ####
df_caged <- read_caged_rds(data_folder = 'C:/Users/Dell/Desktop/USP_Municipios/Dados/CAGED/Clean',
                           inicio = '2015-11',
                           fim = '2019-10',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = TRUE)



barplot_mes <- barplot_mensal_sp(df_caged, inicio = '2019-01', fim = '2019-10', incluir_ano_x = FALSE) +
  theme(panel.grid.major.x = element_blank())

saveRDS(barplot_mes, 'C:/Users/Dell/Desktop/barplot_setor_sp_mensal.rds')

ggsave(barplot_mes, filename = 'C:/Users/Dell/Desktop/barplot_setor_sp_mensal.png', width = 8, height = 7)


barplot_sp <- barplot_12meses_por_setor_sp(df = df_caged) +
  theme(panel.grid.major.x = element_blank())

saveRDS(barplot_sp, 'C:/Users/Dell/Desktop/barplot_setor_setor_sp_12meses.rds')

ggsave(barplot_sp, filename = 'C:/Users/Dell/Desktop/barplot_setor_sp_12meses.png', width = 8, height = 7)
