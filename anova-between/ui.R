library(shiny)
fig.width = 500
fig.height = 400

shinyUI(
  fluidPage(
    verticalLayout(
    fluidRow(align = "center",
             plotOutput("mainplot",
                        width = fig.width,
                        height = fig.height
                        )
             ),
    fluidRow(align = "center",
             div(style = "display:inline-block",
               numericInput("clooneynumin",
                          label = "Clooney",
                          value = 6.4,
                          min = 0,
                          max = 10,
                          width = 60
                          )
               ),
             div(style = "display:inline-block",
              numericInput("jolienumin",
                            label = "Jolie",
                            value = 6.8,
                            min = 0,
                            max = 10,
                            width = 60)
              ),
             div(style = "display:inline-block",
               numericInput("endorsernumin",
                            label = "No endorser",
                            value = 3.3,
                            min = 0,
                            max = 10,
                            width = 60
                            )
             )
      
    ),
    fluidRow(align = "center",
             actionButton("newsampbut",
                          label = "New sample"
                          )
             )
    )

  )
)
