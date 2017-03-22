library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)

#Load general plot theme and colors for color brewer
source("C:/Users/asasiad1/surfdrive/rprojects/stat-teaching-apps/plottheme/styling.R")

#Function that returns coordinates of dots, so that they stack nicely
#on top of each other. 
dotcoordinates <- function(numberofdots, dotsinrow = 5, xscale = 1, yscale = 1){
  xtemp <- numeric()
  ytemp <- numeric()
  
  if(numberofdots == 0) {
    xtemp <- numeric(0)
    ytemp <- numeric(0)
  }
  if (numberofdots <= dotsinrow & numberofdots > 0) {
    xtemp <- seq(1, numberofdots)
    ytemp <- rep(1, numberofdots)
  }
  if (numberofdots > dotsinrow) {
    xtemp <- rep(c(1:dotsinrow), floor(( numberofdots / dotsinrow)))
    if((numberofdots %% dotsinrow) != 0){
      xtemp <- c(xtemp, 1:(numberofdots %% dotsinrow))
    }
    ytemp <- rep(1:floor(numberofdots / dotsinrow), each = dotsinrow)
    ytemp <-
      c(ytemp, rep(ceiling(numberofdots / dotsinrow), numberofdots %% dotsinrow))
  }
  xtemp <- xtemp * xscale
  ytemp <- ytemp * yscale
  return(cbind(xtemp,ytemp))
}

shinyServer(function(input, output) {
  
  # Reactive container for samples
  N <- 50 #size of a single sample
  reps <- 1000 #size of repetitions
  
  initialsample <- sample(1:5, size = 50, replace = TRUE)
    #round(runif(n = N, min = 1 , max = 5), digits = 0)

  samples <- 
    reactiveValues(
      firstsample = initialsample,
      hist = factor(),
      lastsample = factor()
    )
  

  # #Take a sample to work with in all later trials.
  # samples$firstsample <- round(runif(n = N, min = 1 , max = 5), digits = 0)
  # # samples$firstsample <- factor(samples$firstsample,
  # #                               levels = c(1:5),
  #                               labels = sort(c("Red",
  #                                               "Orange",
  #                                               "Yellow",
  #                                               "Green",
  #                                               "Blue")))
  # samples$firstsample <- sort(samples$firstsample)
  # Observe buttonpress event and store either a small or a big sample in
  # container.
  observeEvent(input$bootstrapsmallaction,
                                   {
                                     newsample <-
                                       replicate(sample(x = samples$firstsample,
                                              size = N,
                                              replace = TRUE), n = 1)
                                     samples$lastsample <<- newsample
                                     newprop <-
                                       prop.table(table(newsample))["5"]
                                     newprop[is.na(newprop)] <- 0
                                     samples$hist <<- c(samples$hist, newprop)
                                   })
  observeEvent(input$bootstraplargeaction,
                                   {
                                    newsample <-
                                     replicate(sample(x = samples$firstsample,
                                            size = N,
                                            replace = TRUE),
                                            n = reps)
                                    samples$lastsample <<- 
                                      newsample[ , reps]
                                    newprop <-
                                      apply(X = newsample,
                                            MARGIN = 2,
                                            function(x) prop.table(table(x))["5"])
                                    newprop[is.na(newprop)] <- 0
                                    samples$hist <<- c(samples$hist, newprop)
                                   })
  
  output$sampleplot <- renderPlot({
    
    sample <- samples$firstsample
    sample <- factor(sample,
              levels = c(1:5),
              labels = sort(c("Red",
                             "Orange",
                             "Yellow",
                             "Green",
                             "Blue")))
    sample <- sort(sample)
    #Make coordinates for all five categories
    tempcoord <- numeric()
    coordinates <- numeric()
    for (i in 1:length(levels(sample))) {
      data <- sample[sample == levels(sample)[i]]
      tempcoord <- dotcoordinates(length(data),yscale = 1.5)
      tempcoord[, 1] <- tempcoord[, 1] + (((i - 1) * 6))
      coordinates <- rbind(coordinates, tempcoord)
    }
    
    df <- data.frame(sample,coordinates)

    ggplot(data = df, aes(x = xtemp, y = ytemp, color = sample)) +
      geom_point(size = 4) +
      scale_color_manual(values = brewercolors) +
      scale_x_continuous(
        name = "",
        breaks = c(2.5, 8.5, 15.5, 21.5, 28.5),
        limits = c(0, 30),
        labels = sort(names(brewercolors))
      ) +
      scale_y_continuous(name = "",
                         labels = c(),
                         limits = c(0, 15)) +
      ggtitle("Initial Sample") + 
      theme_general() + 
      theme(line = element_blank(),
            legend.position  = "none")
  })

  output$bootstrappedplot <- renderPlot({
    if (length(samples$lastsample) != 0)
    {
      sample <- samples$lastsample
      sample <- factor(sample,
                       levels = c(1:5),
                       labels = sort(c("Red",
                                       "Orange",
                                       "Yellow",
                                       "Green",
                                       "Blue")))
      sample <- sort(sample)
      #Make coordinates for all five categories
      tempcoord <- numeric()
      coordinates <- numeric()
      for (i in 1:length(levels(sample))) {
        data <- sample[sample == levels(sample)[i]]
        tempcoord <- dotcoordinates(length(data),yscale = 1.5)
        tempcoord[, 1] <- tempcoord[, 1] + (((i - 1) * 6))
        coordinates <- rbind(coordinates, tempcoord)
      }
      
      df <- data.frame(sample,coordinates)
      
      ggplot(data = df, aes(x = xtemp, y = ytemp, color = sample)) +
        geom_point(size = 4) +
        scale_color_manual(values = brewercolors) +
        scale_x_continuous(
          name = "",
          breaks = c(2.5, 8.5, 15.5, 21.5, 28.5),
          limits = c(0, 30),
          labels = sort(names(brewercolors))
        ) +
        scale_y_continuous(name = "",
                           labels = c(),
                           limits = c(0, 15)) +
        ggtitle("Last drawn sample") + 
        theme_general() +
        theme(line = element_blank(),
              legend.position  = "none")
    }
  })
  output$sampdistplot <- renderPlot({
    if(length(samples$hist) != 0)
    {
      
      df <- data.frame(prop = samples$hist)
      print(df)
      ggplot(df, aes(x = prop)) + 
      geom_histogram(aes(y = ..count../sum(..count..))) + 
      ggtitle("Proportions of yellow candies in all samples") +
      scale_x_continuous(name = "Proportion of yellow candies",
                        limits = c(-0.2,1))+
        scale_y_continuous(name = "Density",
                           limits = c(0,1.1),
                           breaks = seq(0,1,by = 0.2))+
      theme_general()
    }
  })

})
