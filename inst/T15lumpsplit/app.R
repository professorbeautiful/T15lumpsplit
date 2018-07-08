library(shiny)
source('inclRmd.R', local=T)

ui <- fluidPage(
  div(uiOutput('lumpsplitUI'))
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  session <<- session
  input <<- input
  output <<- output
  output[['lumpsplitUI']] <- renderUI({
    inclRmd("Bias,variance,smoothing,shrinking.Rmd")
  })
}


# Run the application
shinyApp(ui = ui, server = server)

