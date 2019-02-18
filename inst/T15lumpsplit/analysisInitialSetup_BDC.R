### First, append to jumpList:
##    jumpList = c(jumpList, plotLumpSplitPoints='Plot of lump & split points')
##  Then,
# source('analysisInitialSetup.R', local=TRUE); a(name=paste0('section-a_', analysisName) )

#### the "cat" output appears in the document.

printFromAnalysisInitialSetup_BDC =  FALSE

if(printFromAnalysisInitialSetup_BDC)
  cat('analysisInitialSetup_BDC.R: ')

analysisNumber <- length(jumpList_BDC)
analysisName <- names(jumpList_BDC)[[analysisNumber]]
if(printFromAnalysisInitialSetup_BDC)
  cat(  analysisName, ' ', analysisNumber)


# get('jumpList_BDC', parent.frame(2)) works in the R box.
# assign('mapAnalysisToBDCnumber', 1234, env= pare  nt.frame(2))
# get('mapAnalysisToBDCnumber', env= parent.frame(2))  ## OK
#  Might need a different #.

if(!exists('mapAnalysisToBDCnumber'))
  mapAnalysisToBDCnumber = numeric(0)

### When the bigDataComponent for this analysis is displayed, this will be its number.
mapAnalysisToBDCnumber[analysisName] =
  currentBDCnumber =
  getSequenceLength("BDC") + 1
### bigDataComponent will increment the BDC sequence when created.
if(printFromAnalysisInitialSetup_BDC)
  cat('  currentBDCnumber ', currentBDCnumber, '\n')
