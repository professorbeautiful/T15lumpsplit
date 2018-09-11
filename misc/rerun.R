# rmarkdown::render(
#   output_file = 'BVout.html',
#   input =
#     'inst/T15lumpsplit/Bias-variance-smoothing-shrinking.Rmd')
# browseURL('BVout.html')
.rerun = function(file = "inst/T15lumpsplit/Bias-variance-smoothing-shrinking.Rmd")
  rmarkdown::run(
    file = file,
    default_file = gsub(".*/", "", file),
    shiny_args = list(launch.browser = TRUE)
  )


