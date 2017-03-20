library(RColorBrewer)

#General ggplot2 theme
theme_general <- function() {
  theme_classic() +
    theme(
      text = element_text(size = 15),
      plot.title = element_text(hjust = 0.5),
      panel.border=element_rect(fill=NA)
    )
}
# Standard colors to use
brewercolors <- brewer.pal( 5, name =  "Spectral")
names(brewercolors) <- c("Red", "Orange", "Yellow", "Green", "Blue")