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

if( ! exists(x = 'thisData')) thisData = '(not set yet)'
if( ! exists(x = 'DLdataLastUsed')) DLdataLastUsed = '(not set yet)'
### This nonsense was necessary to keep the app from disconnecting from a warning at
###    shinyapps.io.
cat('analysisReactiveSetup_BDC.R:', analysisName, '  currentBDCnumber=', currentBDCnumber)
cat('\n    BEFORE:  ' )

isolate({
  thisTauTrueID = get_thisTauTrueID(currentBDCnumber)
  if(is.null(
    rValues[[ thisTauTrueID]]
  ) )
    rValues[[ thisTauTrueID ]] = 0
})



cat('\n    AFTER:  ' )
cat('tauTrue: ',
    paste(rValues[[ thisTauTrueID]]),
    '\n')



