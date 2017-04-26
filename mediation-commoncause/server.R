library(shiny)
library(diagram)

shinyServer(function(input, output,session) {
  
  updateSliderInput(session,
                    "agepolslider",
                    value = round(runif(n = 1, min = -.7, max = .7), digits = 2)
                    )
  updateSliderInput(session,
                    "agereadslider",
                    value = round(runif(n = 1, min = -.7, max = .7), digits = 2)
  )
  updateSliderInput(session,
                    "polreadslider",
                    value = round(runif(n = 1, min = -.7, max = .7), digits = 2)
  )
  
  output$mainplot <- renderPlot({
    #correlation between Predictor and mediator.
    r_PM <- input$agepolslider
    #correlation between mediator and Outcome.
    r_MO <- input$polreadslider
    #correlation between Predictor and Outcome.
    r_PO <- input$agereadslider
    # Partial standardized regression coefficients.
    b_PM <- r_PM
    b_PO <- r_PO
    b_MO <- round((r_MO - r_PM*r_PO)/(1 - r_PM^2), digits = 2)
    b_indirect <- round(b_PM * b_MO, digits = 2)
    
    par(mar = c(1,1,1,1))
    openplotmat()
    labels <- c("Pol. interest","Age", "Readingtime")
    elpos<-matrix(data =c(.5,.75,.25,.25,.75,.25),
                  nrow = 3, ncol = 2,
                  byrow = TRUE)
    arc23 <- curvedarrow(from =elpos[2,],
                         to=elpos[3,],
                         curve=-0.2,
                         lty=2,
                         lcol="red"
                         )
    arr21 <- straightarrow(from = elpos[2,],
                           to = elpos[1,],
                           lty=1,
                           lcol=1,
                           arr.pos=.5
                           )
    arr23 <- straightarrow(from = elpos[2,],
                            to = elpos[3,],
                           lty=1,lcol=1,
                           arr.pos=.5
                           )
    arr13 <- straightarrow(from = elpos[1,],
                           to = elpos[3,],
                           lty=1,
                           lcol=1,
                           arr.pos=.5
                           )
    arr23 <- straightarrow(from = elpos[2,],
                           to = elpos[3,],
                           lty=1,
                           lcol=1,
                           arr.pos=.5
                           )
    text(arr21[1] - .06, arr21[2], b_PM)
    text(arr23[1], arr23[2] -.05, b_PO)
    text(arr13[1] + .06,arr13[2], r_MO)
    text(arc23[1],arr23[2] + .15, b_indirect,col = "red")
    for ( i in 1:3) textempty(elpos[i,],lab=labels[i],cex = 1.3)
    
  })
})
