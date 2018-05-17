homeDir = getwd()
setwd('inst/T15lumpsplit')
knitr::knit(input = 'Bias,variance,smoothing,shrinking.Rmd',
            output = 'Bias,variance,smoothing,shrinking.html')

browseURL('Bias,variance,smoothing,shrinking.html',
          browser = '/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome')

setwd(homeDir)
