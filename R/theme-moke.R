library(ggplot2)
library(showtext)

# Supply custom fonts using `showtext` 
font <- 'Roboto'
font_add_google(font)
showtext_auto(enable = TRUE)

theme_moke <- function(base = 8, family = font) {
  theme_minimal(base_size = base, base_family = font) + 
    theme(
      axis.title.x = element_text(margin = margin(0,0,0,0)),
      axis.title.y = element_text(margin = margin(r=5)),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour="black", fill=NA, size=.25), 
      plot.background = element_rect(fill = "white", color = NA)
    )
}
