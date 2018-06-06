linkout = function(fileName) {
  theFile = system.file(
  package="T15lumpsplit",
  'T15lumpsplit/', fileName)
  theCommand = paste0(
    "browseURL('", theFile, "')")
  eval(parse(text=theCommand))
#  "system('open ", theFile, "')")
}

# NOTE: for local anchors, the name is prepended with 'section-'
# at the target, but not at the clickbait.

linkoutLink = function(fileName, linkouttext) {
  # For opening a file in the package
  thisNumber = nextNumber(sequenceType = "linkoutLink")
  IdThisLinkout = paste0('linkoutLink', thisNumber)
  observeEvent(input[[IdThisLinkout]], linkout(fileName)
  )
  actionLink(IdThisLinkout,
             label=HTML(
               paste0('<font color=blue>', linkouttext, '</font>'))
  )
}

## This doesn't work yet.
## This address works: http://127.0.0.1:7767/Bias,variance,smoothing,shrinking.Rmd#section-a_crossvalidationPlot
##  ( in address bar)
linkinLink = function(anchorName, linktext) {
  # For going to a local anchor
  thisNumber = nextNumber(sequenceType = "linkinLink")
  IdThisLinkin = paste0('linkinLink', thisNumber)
  thisAnchorName <<- anchorName
  observeEvent(input[[IdThisLinkin]], {
    print(anchorName)
    print(force(anchorName))
    print(thisAnchorName)
    linkin(force(thisAnchorName))
  }
  )
  actionLink(IdThisLinkin,
             label=HTML(
               paste0('<font color=blue>', linktext, '</font>'))
  )
}

linkin = function(anchorName) {
  port = rValues$thisSession$clientData$url_port
  print(paste("port=", port))
  address = paste0(
    'http://127.0.0.1:', port,
    '/Bias,variance,smoothing,shrinking.Rmd#section-',
    anchorName)
  theCommand = paste0(
    'browseURL("', address, '")')
  print(paste('theCommand=', theCommand))
  eval(parse(text=theCommand))
}
#From session help:
#url_protocol, url_hostname, url_port, url_pathname, url_search,
#url_hash_initial and url_hash can be used to get the components of the URL that
#was requested by the browser to load the Shiny app page. These values are from
#the browser's perspective, so neither HTTP proxies nor Shiny Server will affect
#these values. The url_search value may be used with parseQueryString to access
#query string parameters.

