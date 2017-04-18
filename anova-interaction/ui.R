library(shiny)
fig.width = 400
fig.height = 150
shinyUI(
  fluidPage(
    #CSS Styling for table vjust to be in middle
    tags$style(
      HTML(
        ".table > tbody > tr > td {
          vertical-align: middle;
        }
        .table > tbody > tr > th {
          text-align: center; 
        }
        " 
      )
    ),
    
    verticalLayout(
      fluidRow(
        align = "center",
        plotOutput("mainplot",
                   width = fig.width,
                   height = fig.height)
      ),
      fluidRow(
        align = "center",
        
        tags$table(
          style="width: auto;",
          class="table  table-condensed",
          tags$tr(
            tags$th(""),
            tags$th("Nobody"),
            tags$th("Clooney"),
            tags$th("Jolie"),
            tags$th("Total:")
          ),
          tags$tr(
            tags$td(
              "Women"
            ),
            tags$td(
              numericInput("wonobody",
                           label = "",
                           value = 4.5,
                           min = 0,
                           max = 10,
                           width = 50
              )
            ),
            tags$td(
              numericInput("woclooney",
                           label = "",
                           value = 6.5,
                           min = 0,
                           max = 10,
                           width = 50
              )            
            ),
            tags$td(
              numericInput("wojolie",
                           label = "",
                           value = 8.5,
                           min = 0,
                           max = 10,
                           width = 50
              )            
            ),
            tags$td(
              "what"  
            )
          ),
          tags$tr(
            tags$td(
              "Men"
            ),
            tags$td(
              numericInput("mennobody",
                           label = "",
                           value = 3,
                           min = 0,
                           max = 10,
                           width = 50
              )             
            ),
            tags$td(
              numericInput("menclooney",
                           label = "",
                           value = 5,
                           min = 0,
                           max = 10,
                           width = 50
              )
            ),
            tags$td(
              numericInput("menjolie",
                           label = "",
                           value = 7,
                           min = 0,
                           max = 10,
                           width = 50
              )
            ),
            tags$td(
              "Test"
            )
          ),
          tags$tr(
            tags$td(
              strong("Total:")
            ),
            tags$td(),
            tags$td(),
            tags$td(),
            tags$td()
          ) 
        )
      )
    )
  )
)

