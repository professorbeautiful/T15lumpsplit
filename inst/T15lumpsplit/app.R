library(shiny)
source('inclRmd.R', local=T)

# Define UI for application that draws a histogram
ui <- fluidPage(
  div(uiOutput('lumpsplitUI'))
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  session <<- session
  input <<- input
  output <<- output
  output[['lumpsplitUI']] <- renderUI({
    tagList(inclRmd("LumpSplit-with-notebook.Rmd") )})
}


# Run the application
shinyApp(ui = ui, server = server)

