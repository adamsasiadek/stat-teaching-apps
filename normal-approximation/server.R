library(shiny)
library(ggplot2)
library(RColorBrewer)
# Define server logic required to draw a histogram
source("C:/Users/asasiad1/surfdrive/rprojects/stat-teaching-apps/plottheme/styling.R", local = TRUE)
shinyServer(function(input, output) {
  N <- 500 #sample size
  popmean <- 2.8 # mean of sample
  binwidth <- .1 # binwidth
  minpopsd <- 0.2
  maxpopsd <- 0.8
  #reactive value holding sample
  data <- reactiveValues(sample = as.numeric(),
                         sd = as.numeric()
                         )
  
  
  
  observeEvent(input$sample, {
    data$sd <<- runif(n = 1,
                      min = minpopsd,
                      max = maxpopsd)
    data$sample <<- rnorm(N,
                          mean =  2.8,
                          sd = data$sd
                          )
  })
  
  output$mainplot <- renderPlot({
    validate(
      need(data$sample != "", "Please draw a sample")
    )
    #values indicating the lower and upper thresholds for the plot
    lowerthres <-
      qnorm(
        p = 0.025,
        mean = popmean,
        sd = data$sd,
        lower.tail = TRUE
      )
    upperthres <-
      qnorm(
        p = 0.025,
        mean = 2.8,
        sd = data$sd,
        lower.tail = FALSE
      )
    
    datahist <- data.frame(candyweight = data$sample)
    datahist$colours <- cut(datahist$candyweight, breaks = c(0,lowerthres,upperthres,6), include.lowest = TRUE)
    brk <-levels(datahist$colours)
    colors <- unname(c(brewercolors["Red"],brewercolors["Blue"], brewercolors["Green"]))
    ##GGPLOT
    ggplot(datahist, aes(x = candyweight, fill = colours)) +
      geom_histogram(stat = "bin",
                     binwidth = binwidth) +
      stat_function(
        fun = function(x, mean, sd, n, bw) {
          dnorm(x = x, mean = mean, sd = sd) * n * bw
        },
        args = c(
          mean = mean(datahist$candyweight),
          sd = sd(datahist$candyweight),
          n = N,
          bw = binwidth
        )
      ) +
      geom_vline(
        aes(xintercept = lowerthres),
            colour = brewercolors["Red"],
            linetype = "dashed",
            size = 1
      ) +
      geom_vline(
        aes(xintercept = upperthres),
        linetype = "dashed",
        colour = brewercolors["Green"],
        size = 1
      ) +
      geom_text(
        label = "Lower 2.5%",
        aes(x = lowerthres,
            y = 20),
        colour = brewercolors["Red"],
        hjust = 1
      ) +
      geom_text(
        label = "Upper 2.5%",
        aes(x = upperthres,
            y = 20),
        colour = brewercolors["Green"],
        hjust = 0
      ) +
      xlim(c(0,6)) +
      scale_fill_manual("", breaks = brk, values = colors, drop = FALSE)+
      guides(fill = FALSE,
             colour = FALSE) +
      theme_general()
    
  })
})

