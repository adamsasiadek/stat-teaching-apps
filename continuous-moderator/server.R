library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 source("../plottheme/styling.R", local = TRUE)
  n <- 85 
  # Create predictor.
  set.seed(4932)
  exposure <- runif(n) * 10
  # Create moderator.
  set.seed(4321)
  contact <- 0.12 * (10 - exposure) + rnorm(n, mean = 4.5, sd = 2)
  # Create outcome.
  set.seed(390)
  attitude <-
    -0.26 * exposure + 0.15 * contact + 0.04 * exposure * contact + rnorm(n, mean = 2, sd = 0.5) 
  #Function of outcome for plotting
  attfun <- function(exposure,contact){
    -0.26*exposure + 0.15*contact + 0.04*exposure*contact + 2
  }
  output$headtext <-renderText(paste("Relationship between Exposure & Attitude with Contact at",
                                             input$modvalueslider))
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    df <- data.frame(x = c(-1, 6))
    scatter <- data.frame(attitude = attitude, exposure = exposure)
 
   ggplot(df, aes(x = x)) +
     stat_function(
       fun = attfun,
       args = list(contact = input$modvalueslider),
       n = 500) +
     geom_point(data = scatter, aes(x = exposure, y = attitude)) +
     coord_cartesian(xlim = c(0, 6), ylim = c(0, 5)) +
     ylab("Attitude") +
     xlab("Exposure") +
     theme_general()
       
   })
  ##FORMULA OUTPUT##
  output$formulaui <- renderUI({
    withMathJax(
      helpText(
        paste("$$attitude = 2 + (-0.26 + 0.04 * \\color{blue}{",
              input$modvalueslider,
              "})*exposure + 0.15*\\color{blue}{",
              input$modvalueslider,
              "}$$")
      )
    )
  })
})
