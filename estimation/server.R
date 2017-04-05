library(shiny)
library(ggplot2)

shinyServer(function(input, output,session) {

  source("../plottheme/styling.R", local = TRUE)
  
  mean <- 2.8
  sd <- 0.5
  ###### CONNECTION BETWEEN BOTH SLIDERS#########
  #If nslider moves, change seslider
  observeEvent(input$nslider,{
        invalidateLater(50, session)
        updateSliderInput(session, inputId = "seslider",
                          value = sd/sqrt(input$nslider),
                          step = NULL
                          )
  })
  #If seslider moves, change nslider
  observeEvent(input$seslider,{
    invalidateLater(50, session)
    updateSliderInput(session,
                      inputId = "nslider",
                      value = (sd/input$seslider)^2,
                      step = NULL
                     )
  })
  
  ####### MAIN PLOT #############################
    output$mainplot <- renderPlot({
    
      #Validations
      validate(
        need(
          input$cfintslider != 100,
          "Selecting 100% leads to an infinitely wide confidence interval"
        )
      )
      
      tailarea <- (1 - input$cfintslider / 100) / 2 #calculates value for qnorm
    

   

    
    error <- qnorm(1 - tailarea) * input$seslider #Distance for interval  
    left <- mean - error #Left confidence interval border
    right <- mean + error #Right confidence interval border
    
    ggplot(data.frame(x = c(0,6)), aes(x = x)) + 
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd)) + 
      #X scale definition and generation of z score scale on top
      scale_x_continuous(breaks = seq(0,6,by = .5),
                         sec.axis = sec_axis((~./sd - mean/sd),
                                              breaks = -4:4,
                                              name = "zscore")
                         ) +
      #Left vline
      geom_vline(aes(xintercept = left,
                     linetype = "left margin")) +
      #Right vline
      geom_vline(aes(xintercept = right,
                     linetype = "right margin")) +
      #Mean vline
      geom_vline(aes(xintercept = mean,
                     linetype = "mean")) + 
      #Defining types of lines
      scale_linetype_manual(name = "",
                            values = c("left margin" = "dashed",
                                       "right margin" = "dashed",
                                       "mean" = "solid")) + 
      #Center text label
      geom_text(label = paste(input$cfintslider, "%", sep = ""),
                aes(x=mean,
                    y = dnorm(mean, mean = mean, sd = sd)/2,
                    vjust = .5
                )) +
      #Left text label
      geom_text(label = paste((100 - input$cfintslider)/2, "%", sep = ""),
                aes(x=left,
                    y = dnorm(mean, mean = mean, sd = sd)/3,
                    hjust = 1)) +
      #Right text label
      geom_text(label = paste((100 - input$cfintslider)/2, "%", sep = ""),
                aes(x=right,
                    y = dnorm(mean, mean = mean, sd = sd)/3,
                    hjust = 0)) +
      #Left arrow
      geom_segment(aes(x = mean,
                       xend = left,
                       y = dnorm(mean, mean = mean, sd = sd) * .8,
                       yend = dnorm(mean, mean = mean, sd = sd) * .8,
                       colour = "Interval estimate"), 
                   size = 1,
                   arrow = arrow(length = unit(.3,"cm"))) + 
      #Right arrow
      geom_segment(aes(x = mean,
                       xend = right,
                       y = dnorm(mean, mean = mean, sd = sd) * .8,
                       yend = dnorm(mean, mean = mean, sd = sd) * .8,
                       colour =  "Interval estimate"), 
                   size = 1,
                   arrow = arrow(length = unit(.3,"cm"))) +
      
      #Specifing legend for colour/arrow
      scale_colour_manual(guide = guide_legend(title = ""),
                          values = c("Interval estimate" = unname(brewercolors["Red"]))) + 
      #general theme
      theme_general()
  })

})
