add_geometry_municipios <- function(df) {
  polygons_municipios_sp %>%
    select(-NM_MUNICIP) %>%
    right_join(df,
              by = 'Codmun7')
}
