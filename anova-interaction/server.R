library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  source("../plottheme/styling.R", local = TRUE)

  output$mainplot <- renderPlot({
    df <- data.frame(endorser = factor(c("Nobody","Clooney","Jolie","Nobody","Clooney","Jolie"),
                                        levels = c("Nobody","Clooney","Jolie")),
                     sex = as.factor(c(rep("male", 3), rep("female", 3))),
                     willingness_av = c(input$mennobody,
                                        input$menclooney,
                                        input$menjolie,
                                        input$wonobody,
                                        input$woclooney,
                                        input$wojolie))
   n <- 10 # size of each group
   SD <- 1.5 #Sd of each group
   k <- 6 #cells
   
   avnob <- mean(df$willingness_av[df$endorser == "Nobody"])
   avcloon <- mean(df$willingness_av[df$endorser == "Clooney"])
   avjol <- mean(df$willingness_av[df$endorser == "Jolie"])
   avmen <-mean(df$willingness_av[df$sex == "male"])
   avwom <- mean(df$willingness_av[df$sex == "female"])
   avtot <- mean(df$willingness_av)
   SSsexend <- ((input$mennobody - avmen - avnob + avtot)^2 +
               (input$menclooney - avmen - avcloon + avtot)^2 +
               (input$menjolie - avmen - avjol + avtot)^2 +
               (input$wonobody - avwom -avnob + avtot)^2 +
               (input$woclooney - avwom - avcloon + avtot)^2 +
               (input$wojolie - avwom - avjol + avtot)^2) * k
   print(paste("SSsexend:",SSsexend))
   SSwith <- (SD^2 * n) * k
   print(paste("SSwith:",SSwith))
   MSsexend <- SSsexend/2
   print(paste("MSsexend:",MSsexend))
   MSwith <-SSwith/((n * k) - k)
   print(paste("MSwith:",MSwith))
   F <- MSsexend/MSwith
   print(F)
   
   print(pf(F, df1 = 2, df2 = (n * k)- k))
    ggplot(df, aes(x = endorser,
                  y = willingness_av,
                  group = sex,
                  colour = sex)) +
      geom_point(aes(shape = sex),
                 size = 3) + 
      geom_line(aes(linetype = sex),
                size = .7) + 
      scale_y_continuous(limits = c(0,10),
                         breaks = seq(1,10)) + 
      ylab("Willingness") + 
      xlab("") + 
      ggtitle("Average willingness per condition") + 
      theme_general()
  })
  
  
  output$totnobtext <- renderText(
    as.character(input$wonobody + input$mennobody))
  
  output$totclotext <- renderText(
    as.character(input$woclooney + input$menclooney))
  
  output$totjoltext <- renderText(
    as.character(input$wojolie + input$menjolie))
  
  output$totwomtext <- renderText(
    as.character(input$wonobody + input$woclooney + input$wojolie))
  
  output$totmentext <- renderText(
    as.character(input$mennobody + input$menclooney + input$menjolie))
})
