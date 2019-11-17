custom_theme <- function() {
  theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          # panel.grid.major.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          text = element_text(family = 'serif'),
          plot.title = element_text(face = 'bold'),
          axis.title = element_text(face = 'bold'))
}
