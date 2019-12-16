library(cagedExplorer)

# Carregando dados (atencao para inicio e fim)
df_acum_ano_atual <- read_caged_rds(data_folder = '~/Desktop/Clean', inicio = '2018-11', fim = '2019-10')
df_acum_ano_anterior <- read_caged_rds(data_folder = '~/Desktop/Clean', inicio = '2017-11', fim = '2018-10')

# Vetor com nomes das colunas das tabelas finais
colunas <- c('Setores',
             'Out./2018',
             'Acumulado Nov./2017 a Out/2018',
             'Out./2019',
             'Acumulado Nov./2018 a Out./2019')

# Pasta onde as tabelas serao salvas
output_folder <- '~/Desktop/Tabelas'

#### MUNICIPIOS ####
for (mun in c('RIBEIRAO PRETO', 'SERTAOZINHO', 'CAMPINAS', 'FRANCA', 'SAO JOSE DO RIO PRETO')) {

  criar_tabela_municipio(df_ano_atual = df_acum_ano_atual,
                         df_ano_anterior = df_acum_ano_anterior,
                         mes_atual = 10,
                         municipio = mun,
                         colunas = colunas,
                         csv_output = str_c(output_folder, '/', mun, '.csv'))
}

#### REGIAO ADM RIBEIRAO ####
criar_tabela_regiao_adm(df_ano_atual = df_acum_ano_atual,
                        df_ano_anterior = df_acum_ano_anterior,
                        mes_atual = 10, colunas = colunas,
                        csv_output = str_c(output_folder, '/REGIAO_ADM_RIBEIRAO.csv'))

#### ESTADO SP ####
criar_tabela_estado(df_ano_atual = df_acum_ano_atual,
                    df_ano_anterior = df_acum_ano_anterior,
                    mes_atual = 10, colunas = colunas,
                    csv_output = str_c(output_folder, '/ESTADO_SP.csv'))
