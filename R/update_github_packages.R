update_github_packages = function() {
  libs = dir('/Users/Roger/Library/R/3.4.todo/')
  install.packages('devtools')
  library('devtools')
  libs.to.do = setdiff(libs, installed.packages())
  more.libs.to.do = sapply (libs, function(lib) {
    try(install.packages(lib), silent = TRUE)
  }
  )

  #for(pac in libs.to.do) {
  pac = 'RTutor'
  DESC = readLines(paste0('/Users/Roger/Library/R/3.4.todo/',
                          pac, '/DESCRIPTION'))
  username = gsub('RemoteUsername: ', '',
                  grep('RemoteUsername', value = TRUE,
                       DESC)
  )
  install_github(paste(sep = '/', username, pac))
}
# 1] "dplyrExtras"  "DT"           "dtplyr"
# [4] "DUE"          "IRkernel"     "restorepoint"
# [7] "rNotebook"    "RTutor"       "shinyEvents"
# [10] "stringi"      "stringtools"  "T15lumpsplit"

# system(paste("mv ", libs.to.do, " ~/Library/R/3.4.todo" ))
update_github_pkgs <- function() {

  # check/load necessary packages
  # devtools package
  if (!("package:devtools" %in% search())) {
    tryCatch(require(devtools), error = function(x) {warning(x); cat("Cannot load devtools package \n")})
    on.exit(detach("package:devtools", unload=TRUE))
  }

  pkgs <- installed.packages(fields = "RemoteType")['lmtest',]
  github_pkgs <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]

  print(github_pkgs)
  lapply(github_pkgs, function(pac) {
    message("Updating ", pac, " from GitHub...")

    repo = packageDescription(pac, fields = "GithubRepo")
    username = packageDescription(pac, fields = "GithubUsername")

    install_github(repo = paste0(username, "/", repo))
  })
}

# system(paste("mv ", libs.to.do, " ~/Library/R/3.4.todo" ))
# update_github_pkgs <- function() {
#
#   # check/load necessary packages
#   # devtools package
#   if (!("package:devtools" %in% search())) {
#     tryCatch(require(devtools), error = function(x) {warning(x); cat("Cannot load devtools package \n")})
#     on.exit(detach("package:devtools", unload=TRUE))
#   }
#
#   pkgs <- installed.packages(fields = "RemoteType")['lmtest',]
#   github_pkgs <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]
#
#   print(github_pkgs)
#   lapply(github_pkgs, function(pac) {
#     message("Updating ", pac, " from GitHub...")
#
#     repo = packageDescription(pac, fields = "GithubRepo")
#     username = packageDescription(pac, fields = "GithubUsername")
#
#     install_github(repo = paste0(username, "/", repo))
#   })
# }
