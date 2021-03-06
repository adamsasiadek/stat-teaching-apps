library(RColorBrewer)

#General ggplot2 theme
theme_general <- function() {
  theme_classic() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 13),
      panel.border=element_rect(fill=NA)
    )
}
# Standard colors to use
brewercolors <- brewer.pal( 5, name =  "Spectral")
brewercolors[3] <- "#ffff00"
names(brewercolors) <- c("Red", "Orange", "Yellow", "Green", "Blue")