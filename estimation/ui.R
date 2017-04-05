library(shiny)
fig.width = 600
fig.height = 300

shinyUI(fluidPage(
  verticalLayout(
    fluidRow(align = "center",
             plotOutput("mainplot",
                        height = fig.height,
                        width = fig.width
                        )
             ),
    fluidRow(align = "center",
             inputPanel(
                        sliderInput("cfintslider",
                                    label = "Confidence Interval",
                                    value = 95,
                                    min = 50,
                                    max = 100,
                                    step = 1,
                                    post = "%",
                                    round = FALSE
                                    ),
                        sliderInput("nslider",
                                    label = "Sample size",
                                    value = 25,
                                    min = 1,
                                    max = 30,
                                    step = 1,
                                    round = FALSE
                                    ),
                        sliderInput("seslider",
                                    label = "Standard error",
                                    value = .5/sqrt(25),
                                    min = .09,
                                    max = .5,
                                    step = .001,
                                    round = -2
                                    )
                       )
             )
    )
))
