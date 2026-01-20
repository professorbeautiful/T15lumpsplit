require('shiny')
require('xtable')
app =  shinyApp(
  list(ui=bootstrapPage(
         div(h1('hello'),
             uiOutput('m'),
             uiOutput('m2')))),
       server = function(input,output ) {
         wrapCSS = function(x, css) {
           paste('<span style="', css, ';"> ', x, '</span>')
         }

         output$m = renderUI({
           df <- mtcars[1:10, 1:5]
           df[4,3] =  wrapCSS('df43', "color:red")
           print(df[4,3])
           x = kableExtra::kbl(df)  %>%
             kable_styling(full_width = F)
           where_df43 = gregexpr('df43', x[1])[[1]]
           cat(substr(x[1], where_df43-10, where_df43+10), '\n\n')
           x[1] = gsub("&gt;", '>', x[1])
           x[1] = gsub("&lt;", '<', x[1])
           cat(substr(x[1], where_df43-10, where_df43+10), '\n\n')
           print(x)
           #HTML(x)  # not needed.
         })
         output$m2 = renderUI({
           df <- mtcars[1:10, 1:5]
           x = kableExtra::kbl(df) %>%
             kable_styling(full_width = F)
           print(x)
           #HTML(x)  # not needed.
         })
       }
)
runApp(app)
