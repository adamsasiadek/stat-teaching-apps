
library(shiny)

shinyServer(function(input, output) {
   source("../plottheme/styling.R", local = TRUE)
  
  mean <- 5.5 #mean of t dist
  sd <- 0.4 #sd of  dist
  
  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)/sd
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    
    df <- input$sampsizeslider - 1 #df
    
    #Calculating the right and left threshold
    right <- mean + sd * qt(0.025, df, lower.tail = FALSE)
    left <- mean - sd * qt(0.025, df, lower.tail = FALSE)

    #PLOT#
    ggplot(data.frame(x = c(0,8)), aes(x = x)) + 
      #Left area under curve
      stat_function(fun = dtshift,
                    xlim = c(1,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = "black",
                    args = list(mean = mean, sd = sd, df = df),
                    n = 1000) +
      #Right area under curve
      stat_function(fun = dtshift,
                    xlim = c(right,10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = sd, df = df),
                    n = 1000) +
      #T distribution function
      stat_function(fun = dtshift,
                    args = list(mean = mean, sd = sd, df = df),
                    n = 1000) +
      #2,5% label right
      geom_text(label = "2.5%",
                aes(x = right * 1.02 ,
                    y =  dtshift(right, mean, sd, df) * 1.25),
                hjust = 0,
                size = 5) +
      #2.5% label left
      geom_text(label = "2.5%",
                aes(x = left * 0.98 ,
                    y =  dtshift(left, mean, sd, df) * 1.25),
                hjust = 1,
                size = 5) +
      #Mean vline
      geom_vline(aes(xintercept = mean,
                     linetype = "Mean")) +
      #Left vline
      geom_vline(aes(xintercept = left,
                     linetype = "Threshold")) +
      #Right vline
      geom_vline(aes(xintercept = right,
                     linetype = "Threshold")) +
      #Definition of linetypes
      scale_linetype_manual(name = "",
                            values = c("Mean" = "solid", "Threshold" = "dashed")) +
      #Scaling and double axis definitions
      coord_cartesian(xlim = c(3,8)) +
      scale_x_continuous(breaks = 0:8,
                         sec.axis = sec_axis(~. - 5.5,
                                             breaks = seq(-5,5,by = .5),
                                             name = "T value")) + 
      #Axis labels and theme                                       
      xlab("Average media literacy") + 
      ylab("Density") + 
      theme_general() + 
      guides(linetype = FALSE)
  })
})
