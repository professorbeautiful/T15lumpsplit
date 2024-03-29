### analysisReactiveSetup_DTC.R
## Initialize data for this analysis if necessary.

### First, set analysisName before
#        source('analysisReactiveSetup.R', local=TRUE)        #  NO
#        source('inst/T15lumpsplit/analysisReactiveSetup.R', local=TRUE)  # YES
#  But this path will not work out of the installed package.
#  Currently I'm linking it to ../..
#cat('analysisReactiveSetup.R: ')
#cat( analysisName, '  ', system('pwd', intern=TRUE), '\n')


currentDTCnumber = mapAnalysisToDTCnumber[analysisName]
printFromAnalysisReactiveSetup_DTC = FALSE

if( ! exists(x = 'thisData')) thisData = '(not set yet)'
if( ! exists(x = 'DLdataLastUsed')) DLdataLastUsed = '(not set yet)'
### This nonsense was necessary to keep the app from disconnecting from a warning at
###    shinyapps.io.
if(printFromAnalysisReactiveSetup_DTC) {
  cat('analysisReactiveSetup_DTC.R:', analysisName, '  currentDTCnumber=', currentDTCnumber)
  cat('\n    BEFORE:  ' )
  cat('thisData: ', paste(thisData), 'DLdataLastUsed: ', paste(DLdataLastUsed),
      'rValues DLdata: ',
      paste(rValues[[paste0('DLdata', currentDTCnumber) ]]))
}
#isolate({
  if(is.null(
    rValues[[paste0('DLdata', currentDTCnumber) ]]
  ) )
    rValues[[paste0('DLdata', currentDTCnumber) ]] = DLdataOriginal
  thisData <<- DLdataLastUsed <<- rValues[[paste0('DLdata', currentDTCnumber) ]]

  # if(printFromAnalysisReactiveSetup_DTC) {
  #   cat('\n    AFTER:  ' )
  #   cat('thisData: ', paste(thisData), 'DLdataLastUsed: ', paste(DLdataLastUsed),
  #       'rValues DLdata: ')
  #   print(paste(rValues[[paste0('DLdata', currentDTCnumber) ]]))
  # }
#})
