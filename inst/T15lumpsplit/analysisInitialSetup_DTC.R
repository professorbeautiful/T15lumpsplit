### First, append to jumpList:
##    jumpList = c(jumpList, plotLumpSplitPoints='Plot of lump & split points')
##  Then,
# source('analysisInitialSetup.R', local=TRUE); a(name=paste0('section-a_', analysisName) )

#cat('analysisInitialSetup.R: ')

analysisNumber <- length(jumpList_DTC)
analysisName <- names(jumpList_DTC)[[analysisNumber]]
#cat(  analysisName, '\n')


# get('jumpList_DTC', parent.frame(2)) works in the R box.
# assign('mapAnalysisToBDCnumber', 1234, env= pare  nt.frame(2))
# get('mapAnalysisToBDCnumber', env= parent.frame(2))  ## OK
#  Might need a different #.


if(!exists('mapAnalysisToDTCnumber'))
  mapAnalysisToDTCnumber = numeric(0)
if(!exists('mapAnalysisToBDCnumber'))
  mapAnalysisToBDCnumber = numeric(0)

### When the next DTC is displayed, this will be its number.
mapAnalysisToDTCnumber[analysisName] =
  currentDTCnumber =
  getSequenceLength("DTC") + 1

### Keep the list of DLdata NOT reactive
# DLdata[[mapAnalysisToDTCnumber[analysisName]]] <- DLdataOriginal

#cat("Creating DLdata[[analysisNumber]] for analysisNumber = ",
#    analysisNumber, '\n')
##  a(name=paste0('section-a_', analysisName) )
## Does not work inside a source() call.
