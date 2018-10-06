# rmarkdown::render(
#   output_file = 'BVout.html',
#   input =
#     'inst/T15lumpsplit/Bias-variance-smoothing-shrinking.Rmd')
# browseURL('BVout.html')
.rerun = function(filenum = 1) {
  file = c("inst/T15lumpsplit/Bias-variance-smoothing-shrinking.Rmd",
           "inst/T15lumpsplit/Bias-variance-smoothing-shrinking-testing.Rmd")[filenum]
  rmarkdown::run(file=file,
    default_file = gsub(".*/", "", file),
    shiny_args = list(launch.browser = TRUE)
  )
}


