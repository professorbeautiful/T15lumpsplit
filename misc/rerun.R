# rmarkdown::render(
#   output_file = 'BVout.html',
#   input =
#     'inst/T15lumpsplit/Bias-variance-smoothing-shrinking.Rmd')
# browseURL('BVout.html')

.rerun = function()
  rmarkdown::run("inst/T15lumpsplit/Bias-variance-smoothing-shrinking.Rmd",
               shiny_args = list(launch.browser = TRUE))


