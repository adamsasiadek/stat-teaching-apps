library(shiny)
fig.width = 400
fig.height = 400
# Define UI for application that draws a histogram
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
             sliderInput("agepolslider",
                         label = "Correlation Age w. Pol. interest",
                         min = -0.9,
                         max = 0.9,
                         step = .1,
                         value = 0
                         ),
             sliderInput("agereadslider",
                         label = "Correlation Age w Readingtime",
                         min = -0.9,
                         max = 0.9,
                         step = .1,
                         value = 0
                         ),
             sliderInput("polreadslider",
                         label ="Pol.Interest_Readingtime",
                         min = -0.9,
                         max = 0.9,
                         step = .1,
                         value = 0
                         )
             )
    )
  )
)
