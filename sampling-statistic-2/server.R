library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)


shinyServer(function(input, output){

  candies <- list(
  colors = c("Yellow", "Blue", "Red", "Green"),
  counts = c(50, 50, 50, 50))

  coordinates <- list(
  min = 0,
  max = 10)
  

  candie.sample.coordinates.complete.df <- numeric()
  
  candie.coordinates.df = data.frame(candie.color = rep(candies$colors, candies$counts),
     x = runif(sum(candies$counts),coordinates$min, coordinates$max),
     y = runif(sum(candies$counts),coordinates$min, coordinates$max))
  
  output$populatieplot <- renderPlot({
  
    ggplot(candie.coordinates.df, aes(x = x, y = y, color = candie.color)) +
      geom_point(size = 5) +
      scale_color_manual(values = levels(candie.coordinates.df$candie.color)) + 
      theme(
        rect             = element_blank(),
        line             = element_blank(),
        text             = element_blank(),
        legend.position  = "none"
      )
      
    })
    
  observeEvent(input$smallsample, {
    
    candie.sample.coordinates.df <- candie.coordinates.df[sample(dim(candie.coordinates.df)[1],size = 10, replace = FALSE),]
    
    candie.sample.coordinates.complete.df <<- rbind(candie.sample.coordinates.complete.df,
                                                    as.data.frame(table(candie.sample.coordinates.df$candie.color,
                                                                    exclude = candies$colors[candies$colors != "Yellow"])))
    
    output$populatieplot <- renderPlot({
      
      ggplot(candie.coordinates.df, aes(x = x, y = y, color = candie.color)) +
        geom_point(size = 5) +
        scale_color_manual(values = levels(candie.coordinates.df$candie.color)) + 
        geom_point(data = candie.sample.coordinates.df, size = 7, colour = "black", shape = "O", aes(x = x, y = y)) + 
        theme(
          rect             = element_blank(),
          line             = element_blank(),
          text             = element_blank(),
          legend.position  = "none")
      
      
    })
  
      output$countplot <- renderPlot({
        
        ggplot(candie.sample.coordinates.df, aes(x = candie.color, fill = candie.color)) + 
          geom_dotplot(method = "dotdensity",dotsize = 2) + 
          scale_fill_manual(values = levels(candie.sample.coordinates.df$candie.color)) +
          scale_y_continuous(name = NULL, breaks = NULL) +
          scale_x_discrete(name = "Candie color") + 
          theme(legend.position = "none")
      })
      
      output$samplingstatisticplot <- renderPlot({
        
        ggplot(candie.sample.coordinates.complete.df, aes(x = Freq)) + 
          geom_bar() +
          scale_x_continuous(breaks = 0:10,limits = c(-1,10))
        
      })
  
  })
  

})

