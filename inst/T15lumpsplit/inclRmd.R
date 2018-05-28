require("magrittr")
inclRmd <- function(path) {
  tee = function(x, outputpath) {
    writeLines(capture.output(x), con=outputpath)
    return(x)
  }
  if(!file.exists(path))
    return(paste("inclRmd: file ", path, " not found"))
  knitrOutput = paste(readLines(path, warn = FALSE), collapse = '\n') %>%
    knitr::knit2html(quiet=TRUE,
                     text = ., fragment.only = TRUE, options = ""
                     #,stylesheet=file.path(r_path,"../www/empty.css"

    # rmarkdown::render(quiet=TRUE,output_format = 'html_document',
                      # input = path, runtime='shiny'
    ) %>%
    gsub("&lt;!--/html_preserve--&gt;","",.) %>%
    gsub("&lt;!--html_preserve--&gt;","",.) %>%
    HTML %>%
    withMathJax %>%
  #print(str(knitrOutput))
  #return(knitrOutput)
  tee(., paste0(path, '.inclOutput.Rmd'))
}
