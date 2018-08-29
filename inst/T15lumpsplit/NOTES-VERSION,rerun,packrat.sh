grep Version /Library/Frameworks/R.framework/Versions/3.5/Resources/library/T15lumpsplit/DESCRIPTION
#  Version: 0.3.1 ?

grep Version DESCRIPTION
#  Version: 0.3.2

grep Version packrat/lib/x86_64-apple-darwin15.6.0/3.5.0/T15lumpsplit/DESCRIPTION
#  Version: 0.3.2

#  .rerun()  would run Bias-variance-smoothing-shrinking-new.Rmd
# even though the command referred to Bias-variance-smoothing-shrinking.Rmd,
# Even after the cache was cleared (in Firefox).
# After hiding Bias-variance-smoothing-shrinking-new.Rmd, it worked.
# It picked up the right file, wihtout "-new".
# But why was Bias-variance-smoothing-shrinking-new.Rmd being used?
# .rerun
# function()
#   rmarkdown::run("inst/T15lumpsplit/Bias-variance-smoothing-shrinking.Rmd",
#                shiny_args = list(launch.browser = TRUE))
# <bytecode: 0x111d1f610>
# OK, flawed rmarkdown::run that picks up the wrong file.
