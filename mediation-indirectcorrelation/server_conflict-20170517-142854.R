library(ggplot2)
library(shiny)
# Correlations and standardized coefficients that are in line with the example data file readers.sav:
# Pol. Interest - Age: 0.12
# Pol. Interest - Educ: 0.28
# Pol. Interest - News: 0.01
# Reading Time - Age: 0.88
# Reading Time - Educ: -0.12
# Reading Time - News: -0.84
# Pol. Interest -> Reading Time (simple): 0.14
# Pol. Interest -> Reading Time with Age: 0.04
# Pol. Interest -> Reading Time with Educ: 0.19
# Pol. Interest -> Reading Time with News: 0.15
shinyServer(function(input, output) {
  
  # TEXT/LABEL ELEMENT POSITIONS
  elpos <- data.frame(x = c(.15,.5,.5,.5,.85), y = c(.15,.55,.75,.95,.15))
  
  # LABEL TEXT
  labels <- c(" Pol interest ",
              "        Age        ",
              " News site use", 
              "   Education   ",
              "Reading time"
  )
  # POSSIBLE LABEL COLOURS
  labelcolswitch <- list(age = c("black","black","darkgrey","darkgrey","black"),
                         news = c("black","darkgrey","black","darkgrey","black"),
                         edu = c("black","darkgrey","darkgrey","black","black"))
  # ARROW POSITIONS
  arrowpos <- data.frame(x = c(.15,.15,.15,.66,.66,.66),
                         xend = c(.34,.34,.34,.85,.85,.85),
                         y = c(.15,.15,.15,.5,.71,.91),
                         yend = c(.5,.71,.91,.2,.2,.2))
 # POSSIBLE ARROW COLOURS
  arrcolswitch <-
    list(
      age = c("blue", "darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey"),
      news = c("darkgrey", "blue", "darkgrey", "darkgrey", "blue", "darkgrey"),
      edu = c("darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey", "blue")
    )
  
  # ARROW TEXT LABEL POSITIONS
  arrowlabpos <- data.frame(x = c(.38,.38,.38,.62,.62,.62),
                            y = c(.46,.67,.88,.46,.67,.88)
                            )
  # ARROW TEXT LABEL LABELS
  arrlablabs <- c(0.12, 0.01,0.28,0.88,-0.84,-0.12)
  
  # POSSIBLE ARROW TEXT LABEL COLURS
  arrlabcolswitch <-  list(
    age = c("blue", "darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey"),
    news = c("darkgrey", "blue", "darkgrey", "darkgrey", "blue", "darkgrey"),
    edu = c("darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey", "blue")
  )

  # FUNCTION FOR CALCULATING LINE WIDTH
  lwdfunc <- function(sbeta){
    if(sbeta == 0) return(0)
    if(abs(sbeta) >= 1.5) return(return(0.3 * abs(1.5) * 10 + 0.01 * abs(sbeta) * 10))
    else(return(0.5 * abs(sbeta) * 10))
  }
  # ARROW SIZE 
  arrsize <- numeric()
  for (i in arrlablabs) arrsize <- c(arrsize,lwdfunc(i))

  #ARROW SIZES FOR DIRECT INDIRECT
  #MAIN PLOT
  output$mainplot <- renderPlot({
    
   arrowcolour <- switch(input$confradbut,
                          news = arrcolswitch$news,
                          age = arrcolswitch$age,
                          edu = arrcolswitch$edu
                  )
   
   labelcolour <- switch(input$confradbut,
                         news = labelcolswitch$news,
                         age = labelcolswitch$age,
                         edu = labelcolswitch$edu
                  )
   
   arrlabcolour <- switch(input$confradbut,
                          news = arrlabcolswitch$news,
                          age = arrlabcolswitch$age,
                          edu = arrlabcolswitch$edu
                    )
   
   partialsize <- switch(input$confradbut,
                         news = 0.15,
                         age =  0.04,
                         edu = 0.19)
                          
                          
    ggplot(elpos, aes(x = x, y = y)) +
    coord_cartesian(xlim = c(0,1), ylim = c(0,1)) + 
    #All segments running 
    geom_segment(data = arrowpos, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = arrowcolour, alpha = 1,
                 arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
                 size = arrsize) + 
    geom_segment(x = .15, xend = .7,
                y = .15,
                yend = .15,
                alpha = .4,
                arrow = arrow(length = unit(0.03, "npc"),type = "closed"),
                size = lwdfunc(0.14)) + 
    geom_segment(x = .15, xend = .73, y = .15, yend = .15, alpha = .7, colour = "blue", size = lwdfunc(partialsize)*2) +
    geom_label(aes(x = x, y = y), label = labels, colour = labelcolour, fill = "white",
               label.r = unit(0,"lines"),label.padding = unit(.4, "lines"),  size  = 5) +
    geom_text(data = arrowlabpos, aes(x = x, y = y), label = arrlablabs,colour = arrlabcolour) +
    geom_text(x = .5, y = .2, label = "Simple: 0.14") +
    geom_text(x = .5, y = .1, label = paste("Partial:",partialsize))
  })
  
})
