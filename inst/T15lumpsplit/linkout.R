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
  thisWhoPriorProbNumber = nextNumber(sequenceType = "WhoPriorProb")
  IdThisLinkout = paste0('linkoutLink', thisWhoPriorProbNumber)
  observeEvent(input[[IdThisLinkout]], linkout(fileName)
  )
  actionLink(IdThisLinkout,
             label=HTML(
               paste0('<font color=blue>', linkouttext, '</font>'))
  )
}

## This doesn't work yet.
linkinLink = function(anchorName, linktext) {
  # For going to a local anchor
  thisWhoPriorProbNumber = nextNumber(sequenceType = "WhoPriorProb")
  IdThisLinkout = paste0('linkoutLink', thisWhoPriorProbNumber)
  observeEvent(input[[IdThisLinkout]], {
               theCommand = paste0(
                 "browseURL('#", anchorName, "')")
               eval(parse(text=theCommand))
  }
  )
  actionLink(IdThisLinkout,
             label=HTML(
               paste0('<font color=blue>', linktext, '</font>'))
  )
}

