library(shiny)
library(diagram)

shinyServer(function(input, output, session) {
  
  #Set slider inputs to random drawn values###
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
  
  #Function to calculate line and arrow widths
  lwdfunc <- function(sbeta){
    if(sbeta == 0) return(NULL)
    if(abs(sbeta) > 1) return(10)
    else(return(abs(sbeta) * 10))
  }
  ###MAIN PLOT###
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
    
    ##Set up plotting environment
    par(mar = c(1,1,1,1))
    #Open new canvas to plot to
    openplotmat()
    #Labels for nodes
    labels <- c("(Mediator)\nPol. interest","Age\n(Predictor)", "Readingtime\n(Outcome)")
    #Positions for nodes (canvas is 1 by 1)
    elpos<-matrix(data =c(.5,.85,.15,.15,.85,.15), #Space more fully used
                  nrow = 3, ncol = 2,
                  byrow = TRUE)
    #Curved arrow from Predictor to Outcome
    arcind <- curvedarrow(from =elpos[2,] + c(0.03, 0),
                         to=elpos[3,] + c(-0.03, 0),
                         curve=-0.5,
                         lty=2,
                         lcol="red",
                         dr = .5,
                         lwd = lwdfunc(b_indirect),
                         arr.lwd = lwdfunc(b_indirect)
                         )
    #Straightarrow from Predictor to Mediator
    arrPM <- straightarrow(from = elpos[2,] + c(0, 0.08),
                           to = elpos[1,] + c(-0.05, -0.08),
                           lty=1,
                           lcol=1,
                           arr.pos=.5,
                           lwd = lwdfunc(b_PM),
                           arr.lwd = lwdfunc(b_PM)
                           )
    #Straightarrow from Predictor to Outcome
    arrPO <- straightarrow(from = elpos[2,] + c(0.12, 0),
                            to = elpos[3,] + c(-0.12, 0),
                           lty=1,
                           lcol=1,
                           arr.pos=.5,
                           lwd = lwdfunc(b_PO),
                           arr.lwd = lwdfunc(b_PO)
                           )
    #Straightarrow from Mediator to Outcome
    arrMO <- straightarrow(from = elpos[1,] + c(0.05, -0.08),
                           to = elpos[3,] + c(0, 0.08),
                           lty=1,
                           lcol=1,
                           arr.pos=.5,
                           lwd = lwdfunc(b_MO),
                           arr.lwd = lwdfunc(b_MO)
                           )
    #Text next to arrows
    text(arrPM[1] - .1, arrPM[2], bquote(beta[PM] == .(b_PM)))
    text(arrPO[1], arrPO[2] -.05, bquote(beta[PO] == .(b_PO)))
    text(arrMO[1] + .12, arrMO[2], bquote(beta[MO] == .(b_MO)))
    text(arcind[1], arcind[2] + .05, bquote(beta[indirect] == .(b_indirect)), col = "red")
    #Draw labels
    for (i in 1:3) textempty(elpos[i,],lab=labels[i],cex = 1.3)
  })
})
