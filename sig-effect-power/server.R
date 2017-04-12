library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
 source("../plottheme/styling.R", local = TRUE)

  mean <- 2.8
  sdpop <- 0.6
  
  sample <- reactiveValues(lastsample = numeric(),
                           SE = numeric())
  
  observeEvent(input$sampbutton,ignoreNULL = FALSE,{
    temp <-
      replicate(rnorm(input$ssizeslider, mean = input$savslider, sd = sdpop),
                n = 5)
    sample$lastsample <<- data.frame(means = apply(temp,2,mean))
    sample$SE <<- sdpop/sqrt(input$ssizeslider)
  })
  
  strengthlab <- c("Strong\n2.32",
                   "Moderate\n2.5",
                   "Weak\n2.68",
                   "H0\n2.8",
                   "Weak\n2.92",
                   "Moderate\n3.1",
                   "Strong\n3.28")
  ticks <- c(2.32,2.5,2.68,2.8,2.92,3.1,3.28)
  
  
  output$mainplot <- renderPlot({
    
    
    df <- sample$lastsample
    SE <- sample$SE 
    
    error <- qnorm(1 - .025) * SE # Distance from mean
    left <- mean - error #Left confidence interval border
    right <- mean + error #Right confidence interval border
    
    dots <- sample$lastsample 
    print(dots)
    ggplot(data.frame(x = c(0,6)), aes(x = x)) +
      #Left area under curve
      stat_function(fun = dnorm,xlim = c(-10,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = SE),
                    n = 1000) + 
      #Right area under curve
      stat_function(fun = dnorm,
                    xlim = c(right,10),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = SE),
                    n = 1000) +
      #Normal function line 
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = SE),
                    n = 1000) +
      
      geom_point(data = dots, aes(x = means, y = .1)) +
      # Scale x breaks definition
      scale_x_continuous(breaks = ticks, labels = strengthlab) + 
      #Defining x axis zoom
      coord_cartesian(xlim = c(mean - sdpop,mean + 1.5*sdpop)) +
      #Title and labels for axes
      ggtitle("Sampling distribution") + 
      theme_general() + 
      theme(axis.text.x = element_text(size = 9))
  })
})
