
# NOTE: I had tried to run rstan, and followed directions
# for soft links as in "NOTES on rstan for monte carlo markov chain.rtf"
# That messed up the deploy:
#   *** arch - R
# ERROR: sub-architecture 'R' is not installed
# *** arch - x86_64
# ERROR: sub-architecture 'x86_64' is not installed
# Solution remove the soft links especially the third one.

### NOTE:  do not include "inst" in the argument.

.installFromGithub = function(package='T15lumpsplit', ...)
  devtools::install_github(
    paste0("professorbeautiful/", package),
    build_vignettes=TRUE, ...)


.deploy = function(app='T15lumpsplit', reInstall=TRUE){
  ## TODO: first check that the html files are created committed and pushed.
  if(reInstall)
    .installFromGithub(app)
  apps = app
  for (app in apps) {
    if(substr(app, 1, 5) == "inst/")
      warning(".deploy: do not include 'inst' in app name.")
    cat("wd is " %&% getwd() %&% "\n")
    cat("wd changing to " %&% "inst/" %&% app %&% "\n")
    setwd("inst/" %&% app)
    tryCatch({
      require("rsconnect")
      deployApp()
    },
    finally={
      cat("rsconnect::showLogs(appDir = 'inst/" %&% app %&% "')\n")
      setwd("../..")
    })
  }
}

.runDeployed = function(app="T15lumpsplit"){
  system("open https://trials.shinyapps.io/" %&% app)
  cat("rsconnect::showLogs(appDir = 'inst/" %&% app %&% "')\n")
}

