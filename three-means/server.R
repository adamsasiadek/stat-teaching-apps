

library(shiny)


shinyServer(function(input, output) {
  n <- 10   #Sample size
  N <- 1000 #Number of samples
  sdpop <- 1 #Population sd
  
  #Populationplot.
  output$populationplot <- renderPlot({
    
    ggplot(data = data.frame(x = c(0, 6)), aes(x = x)) +
      stat_function(
        fun = dnorm,
        n = 200,
        args = list(mean = input$populationslider, sd = sdpop)
      ) +
      geom_vline(
        aes(xintercept = input$populationslider),
        color = brewercolors["Red"],
        linetype = "dashed",
        size = 1
      ) +
      scale_x_continuous(limits = c(0, 6)) +
      theme_classic()
  })
  
  #Samplingdistplot
  output$samplingdistplot <- renderPlot({
    #Sample N times from population, take the mean of each sample.
    data.df <-
      data.frame(x = replicate(n = 500, expr = mean(
        rnorm(10, mean = input$populationslider,
              sd = sdpop)
      )))
    
    ggplot(data = data.df, aes(x =x)) +
      geom_histogram(aes(y = ..density..)) +
      geom_vline(
        aes(xintercept = mean(data.df$x)),
        color = brewercolors["Red"],
        linetype = "dashed",
        size = 1
      ) +
      stat_function(
        fun = dnorm,
        n = 101,
        args = list(mean = input$populationslider, sd = sdpop/sqrt(n))
      )+ 
      scale_x_continuous(limits = c(0, 6)) +
      theme_classic()
  })
  
  #Sampleplot
  output$sampleplot <- renderPlot({
    
    #sample n form population?
    smple <- data.frame(x = rnorm(n, input$sampleslider, sdpop))
    
    ggplot(data = smple, aes(x = x)) +
      geom_histogram() +
      geom_vline(
        aes(xintercept = mean(smple$x)),
        color = brewercolors["Red"],
        linetype = "dashed",
        size = 1
      ) +
      scale_x_continuous(limits = c(0, 6)) +
      scale_y_continuous(limits = c(0, 10)) +
      theme_classic()
    
  })
})
