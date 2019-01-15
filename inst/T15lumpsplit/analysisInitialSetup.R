### First, append to jumpList:
##    jumpList = c(jumpList, plotLumpSplitPoints='Plot of lump & split points')
##  Then,
# source('analysisInitialSetup.R', local=TRUE); a(name=paste0('section-a_', analysisName) )

#cat('analysisInitialSetup.R: ')
analysisNumber <- length(jumpList)
analysisName <- names(jumpList)[[analysisNumber]]
#cat(  analysisName, '\n')

if(!exists('mapAnalysisToDTCnumber'))
  mapAnalysisToDTCnumber = numeric(0)
mapAnalysisToDTCnumber[analysisName] = getSequenceLength("DTC") + 1

getDTCnumber = function(analysisName)
  mapAnalysisToDTCnumber[analysisName]

### Keep the list of DLdata NOT reactive
DLdata[[mapAnalysisToDTCnumber[analysisName]]] <- DLdataOriginal

#cat("Creating DLdata[[analysisNumber]] for analysisNumber = ",
#    analysisNumber, '\n')
##  a(name=paste0('section-a_', analysisName) )
## Does not work inside a source() call.
