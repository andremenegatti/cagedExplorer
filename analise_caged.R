# Carregando pacotes
devtools::load_all(".")

# Pasta de dados
data_folder <- 'C:/Users/Dell/Desktop/USP_Municipios/Dados/CAGED'

#### LENDO DADOS DE APENAS UM MES ####

# Nome do arquivo txt do CAGED
filename_caged <- str_c(data_folder, "/Raw/CAGEDEST_092019.txt")

# Leitura do CAGED (pode demorar um pouco...)
dataset <- read_caged(filename_caged)

# Preparando base de dados
df_sp <- dataset %>%
  prepare_caged()

#### LENDO DADOS DE TODOS OS MESES ####

# Grade com todas as combinacoes ano-mes de interesse
df_ref <- crossing(ano = as.character(2017:2019),
                   mes = str_pad(1:12, width = 2, side = 'left', pad = '0')) %>%
  filter(!(ano == '2019' & as.numeric(mes) > 9))

# Iterando entre as combinacoes ano-mes
# Lendo, preparando e salvando bases mensais em arquivos .rds separados
for (i in 1:nrow(df_ref)) {
  filename_caged = str_c(data_folder, '/Raw/CAGEDEST_',
                         df_ref$mes[i], df_ref$ano[i], '.txt')

  message(str_c(i, '\n ', filename_caged, '\n'))

  read_caged(filename_caged) %>%
    prepare_caged() %>%
    saveRDS(file = str_c(data_folder, '/Clean/CAGED_', df_ref$ano[i], '_', df_ref$mes[i], '.rds'))

}
