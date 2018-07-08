### RunModule.R

options(servr.daemon = TRUE)
#### Still not working--- the shiny and the TeX don't process.
#### Update: won't work at all now. Can't find output etc from shinyServer

homeDir = getwd()
setwd('inst/T15lumpsplit')
# knitr::knit(input = 'Bias,variance,smoothing,shrinking.Rmd',
#             output = 'Bias,variance,smoothing,shrinking.html')
rmarkdown::render(input = 'Bias,variance,smoothing,shrinking.Rmd',
            #output_format='all',
            output_file = 'Bias,variance,smoothing,shrinking.html',
            runtime='shiny',
            clean=FALSE, quiet=FALSE)

browseURL('Bias,variance,smoothing,shrinking.html',
          browser = '/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome')

setwd(homeDir)
