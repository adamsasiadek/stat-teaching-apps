library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(scales)


shinyServer(function(input, output) {
  # Handy candy variables
  candy <- list(
    proportions = rep(0.2, 5),
    colornames = c("Red", "Orange", "Yellow", "Green", "Blue")
  )
  
  #Colorpalette variables
  brewercolors <-
    brewer.pal(length(candy$colornames), name =  "Spectral")
  names(brewercolors) <- candy$colornames
  
  #Container for all samples taken
  candy.sample.history.df <- character()
  
  # Plot of proportions
  output$populationproportions <-  renderPlot({
    ggplot(as.data.frame(candy),
           aes(x = colornames, y = proportions, fill = colornames)) +
      geom_col() +
      scale_y_continuous(
        name = "Proportion",
        breaks = seq(0, 1, by = 0.2),
        limits = c(0, 1)
      ) +
      scale_x_discrete(name = "Candy" , breaks = candy$colornames) +
      scale_fill_manual(values = brewercolors) +
      ggtitle("Candy proportions in Population") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
  })
  
  # Events triggered by small sample button
  observeEvent(input$smallsample, {
    #Take sample of candies
    candy.sample.df <-
      data.frame(candy.sample = factor(
        sample(
          candy$colornames,
          prob = candy$proportions,
          replace = TRUE,
          size = 10
        ),
        levels = sort(candy$colornames)
      ))
    
    candy.sample.history.df <<-
      rbind(candy.sample.history.df,
            as.data.frame(
              table(candy.sample.df$candy.sample,
                    exclude = candy$colornames[candy$colornames != "Yellow"])
            ))
    
    #Dotplot
    output$countplot <- renderPlot({
      ggplot(candy.sample.df) +
        geom_dotplot(
          mapping = aes(candy.sample, fill = candy.sample),
          method = "dotdensity",
          dotsize = 2
        ) +
        scale_fill_manual(values = brewercolors,
                          limits = candy$colornames) +
        scale_y_continuous(name = NULL, breaks = NULL) +
        scale_x_discrete(name = "Candy color",
                         breaks = candy$colornames,
                         drop = FALSE) +
        ggtitle("Last sample") +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")
    })
    
    #Sampling-distribution plot
    output$samplingstatisticplot <- renderPlot({
      ggplot(candy.sample.history.df, aes(x = Freq)) +
        geom_bar(fill = brewercolors["Yellow"]) +
        scale_x_continuous(name = "",
                           breaks = 0:10,
                           limits = c(-1, 10)) +
        scale_y_continuous(
          breaks =  function (x)
            floor(pretty(seq(1, max(
              x
            ) + 1)))
        ) +
        ggtitle("Sampling distribution") +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
  })
  
  #Events triggered by large sample button
  observeEvent(input$largesample, {
    for (i in 1:1000) {
      candy.sample.df <-
        data.frame(candy.sample = factor(
          sample(
            candy$colornames,
            prob = candy$proportions,
            replace = TRUE,
            size = 10
          ),
          levels = sort(candy$colornames)
        ))
      
      candy.sample.history.df <<-
        rbind(candy.sample.history.df,
              as.data.frame(
                table(candy.sample.df$candy.sample,
                      exclude = candy$colornames[candy$colornames != "Yellow"])
              ))
    }
    
    
    
    #Dotplot
    output$countplot <- renderPlot({
      ggplot(candy.sample.df) +
        geom_dotplot(
          mapping = aes(candy.sample, fill = candy.sample),
          method = "dotdensity",
          dotsize = 2
        ) +
        scale_fill_manual(values = brewercolors,
                          limits = candy$colornames) +
        scale_y_continuous(name = NULL, breaks = NULL) +
        scale_x_discrete(name = "Candy color",
                         breaks = candy$colornames,
                         drop = FALSE) +
        ggtitle("Last sample") +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")
    })
    
    #Sampling-distribution plot
    output$samplingstatisticplot <- renderPlot({
      ggplot(candy.sample.history.df, aes(x = Freq)) +
        geom_bar(fill = brewercolors["Yellow"]) +
        scale_x_continuous(name = "",
                           breaks = 0:10,
                           limits = c(-1, 10)) +
        scale_y_continuous(
          breaks =  function (x)
            floor(pretty(seq(1, max(
              x
            ) + 1)))
        ) +
        ggtitle("Sampling distribution") +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    
  })
})
