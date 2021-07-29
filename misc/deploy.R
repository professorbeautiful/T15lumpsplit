#  As of 2021-07-26, the deployed version still works perfectly.
#  What if I redeploy though?
'Bias-variance-smoothing-shrinking.Rmd'

## WARNING:  with shiny_arg, and/or launch.browser,
##  results can be VERY misleading.

rerun = function(app=1,
                   launch.browser=TRUE) {
  if(app==1) app = 'Bias-variance-smoothing-shrinking.Rmd'
  if(app==2) app = 'TOC_test.Rmd'
  if(missing(launch.browser))  launch.browser =
      getOption("shiny.launch.browser", interactive())
  rmarkdown::run(
    file = paste0("inst/T15lumpsplit/",app),
    dir = "inst/T15lumpsplit/",
    shiny_arg = list(launch.browser=launch.browser)
  )
}
#appDir='inst/T15lumpsplit/',shiny_args)

## Yippee! this works.
# Bypasses the viewer... and the opening of all the PDFs.
# BUT, the TOC does not show up, and some HTML not processed.
##
## OK to run .installFromGithub,
# but TO DEPLOY Bias-variance-smoothing-shrinking,
# use the Publish or Republish buttons.
##################

# NOTE: I had tried to run rstan, and followed directions
# for soft links as in "NOTES on rstan for monte carlo markov chain.rtf"
# That messed up the deploy:
#   *** arch - R
# ERROR: sub-architecture 'R' is not installed
# *** arch - x86_64
# ERROR: sub-architecture 'x86_64' is not installed
# Solution remove the soft links especially the third one.

### NOTE:  do not include "inst" in the argument.

.installFromGithub = function(package='T15lumpsplit', ...) {
  #options(repos = c(BiocInstaller::biocinstallRepos() ) )
  options(repos = BiocManager::repositories() )
  ## works ok for qvalue
  devtools::install_github(
    paste0("professorbeautiful/", package),
    build_vignettes=TRUE, ...)
}

.deploy = function(app='T15lumpsplit', reInstall=TRUE){
  ## TODO: first check that the html files are created committed and pushed.
  if(reInstall)
    .installFromGithub(app)
  apps = app
  for (app in apps) {
    if(substr(app, 1, 5) == "inst/")
      warning(".deploy: do not include 'inst' in app name.")
    setwd(paste0("inst/", app))
    tryCatch({
      require("rsconnect")
      deployApp()
    },
    finally={
      cat("rsconnect::showLogs(
      appPath = ", paste0('"inst/', app, '")\n') )
      setwd("../..")
    })
  }
}

## ON 2021-07-26, it's demanding removal of browser() and browseURL() calls.

.runDeployed = function(app="T15lumpsplit"){
  system(paste("open https://trials.shinyapps.io/", app) )
  cat("rsconnect::showLogs(appPath = 'inst/", app, "')\n")
}

