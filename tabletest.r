require('shiny')
require('xtable')
app =  shinyApp(
  list(ui=bootstrapPage(
         div(h1('hello'),
             uiOutput('m')))),
       server = function(input,output ) {
         output$m = renderUI({
           df <- mtcars[1:10, 1:5]
           x = kableExtra::kbl(df)
           print((x))
           HTML(x)
         })
       }
)
runApp(app)
