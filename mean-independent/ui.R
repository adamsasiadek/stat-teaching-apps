library(shiny)
fig.width = 300
fig.height = 150
shinyUI(
  fluidPage(
    verticalLayout(
     fluidRow(align = "center",
              column(width = 6,
                     plotOutput("redpopplot",
                                width = fig.width,
                                height = fig.height + 130)
                     ),
              column(width = 6,
                     plotOutput("yellowpopplot",
                                width = fig.width,
                                height = fig.height + 130)
                     )
              ),
     fluidRow(align = "center",
              column(width = 6,
                     plotOutput("redsampplot",
                                width = fig.width,
                                height = fig.height)
              ),
              column(width = 6,
                     plotOutput("yellowsampplot",
                                width = fig.width,
                                height = fig.height)
              )
     ),
     fluidRow(align = "center",
              strong(htmlOutput("calculationtext"))),
     
     fluidRow(align = "center",
              plotOutput("sampdistplot",
                         height = fig.height,
                         width = 2 * fig.width)
              ),
     fluidRow(align = "center",
              actionButton("smallsamplebutton",
                            label = "Draw single sample"
                           ),
              actionButton("largesamplebutton",
                           label = "Draw 1000 samples"
                           ),
              actionButton("resetbutton",
                           label = "Reset"
                          )
              )
    )
  )
)
