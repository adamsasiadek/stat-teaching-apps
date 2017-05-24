library(shiny)

fig.width = 400
fig.height = 300

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
               
               radioButtons(
                 "confradbut",
                 label = "Select Confound",
                 choices = c("Age" = "age",
                             "News site use" = "news",
                             "Education" = "edu")
                 ,
                 inline = TRUE
               )
      ) 
    )  
  )
)
