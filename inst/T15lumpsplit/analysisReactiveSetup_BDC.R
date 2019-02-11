### analysisReactiveSetup_BDC.R
## Initialize data for this analysis if necessary.

### First, set analysisName before
#        source('analysisReactiveSetup.R', local=TRUE)        #  NO
#        source('inst/T15lumpsplit/analysisReactiveSetup.R', local=TRUE)  # YES
#  But this path will not work out of the installed package.
#  Currently I'm linking it to ../..
#cat('analysisReactiveSetup.R: ')
#cat( analysisName, '  ', system('pwd', intern=TRUE), '\n')

currentBDCnumber = mapAnalysisToBDCnumber[analysisName]
printFromAnalysisReactiveSetup_BDC = TRUE

if( ! exists(x = 'thisBigData')) thisBigData = '(not set yet)'
if( ! exists(x = 'BigDataLastUsed')) BigDataLastUsed = '(not set yet)'
### This nonsense was necessary to keep the app from disconnecting from a warning at
###    shinyapps.io.

## We need to trigger the  reaction!

thisBigDataID = paste0('BigData', currentBDCnumber)
if(is.null(
  rValues[[thisBigDataID ]]
) )
  rValues[[thisBigDataID ]] = BigDataOriginal

if(printFromAnalysisReactiveSetup_BDC) {
  cat('analysisReactiveSetup_BDC.R:', analysisName, '  currentBDCnumber=', currentBDCnumber)
  cat('\n    BEFORE:  ' )
  cat('thisBigData: ', paste(thisBigData), 'BigDataLastUsed: ', paste(BigDataLastUsed),
      'rValues BigDataLastUsed: ')
  paste(head(rValues[[paste0('BigDataLastUsed', currentBDCnumber) ]], 4))
}

isolate({
  if(is.null(
    rValues[[paste0('BigData', currentBDCnumber) ]]
  ) )
    rValues[[paste0('BigData', currentBDCnumber) ]] = BigDataOriginal

  thisBigData <<- BigDataLastUsed <<- rValues[[paste0('BigData', currentBDCnumber) ]]

  if(printFromAnalysisReactiveSetup_BDC) {
    cat('\n    AFTER:  ' )
    cat( 'analysisReactiveSetup_BDC.R: first 4 rows now = ')
    paste(head(rValues[[paste0('BigDataLastUsed', currentBDCnumber) ]], 4))
  }
})

