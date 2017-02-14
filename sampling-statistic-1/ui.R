library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  verticalLayout(
    
    plotOutput("populatieplot"),
    
    plotOutput("countplot"),
    
    fluidRow(
      column(4),
      
      column(4,
        
        actionButton("smallsample", "Take a random sample")
         
        ),
      column(4)
  
    )

  )
  
    
  )
)  
