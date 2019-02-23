### analysisReactiveSetup_BDC.R
## Initialize data for this analysis if necessary.

### First, set analysisName before
#        source('analysisReactiveSetup.R', local=TRUE)        #  NO
#        source('inst/T15lumpsplit/analysisReactiveSetup.R', local=TRUE)  # YES
#  But this path will not work out of the installed package.
#  Currently I'm linking it to ../..
#cat('analysisReactiveSetup.R: ')
#cat( analysisName, '  ', system('pwd', intern=TRUE), '\n')

printFromAnalysisReactiveSetup_BDC = FALSE


if( ! exists(x = 'thisBigData')) thisBigData <<- '(not set yet)'
if( ! exists(x = 'BigDataLastUsed')) BigDataLastUsed <<- '(not set yet)'
### This nonsense was necessary to keep the app from disconnecting from a warning at
###    shinyapps.io.

## We need to trigger the  reaction!

currentBDCnumber = mapAnalysisToBDCnumber[analysisName]
thisBigDataID = paste0('BigData', currentBDCnumber)
thisOmegaID = get_thisOmegaID(analysisName)
thisOmega = makeSureOmegaIsGood(input[[thisOmegaID]])
if(printFromAnalysisReactiveSetup_BDC)
  cat('\nanalysisReactiveSetup.R: ', analysisName, ' ', analysisNumber,
      ' ', thisOmegaID, ' ', thisOmega,
      ' currentBDCnumber: ', currentBDCnumber, '\n')

isolate({
  rValues[[thisBigDataID ]] <<-
    makeBigDataWithFeatures(DLdata = DLdataOriginal, Omega = thisOmega)

if(is.null(
  rValues[[thisBigDataID ]]
) )
  rValues[[thisBigDataID ]] = BigDataOriginal

thisBigData <<- BigDataLastUsed <<- rValues[[thisBigDataID ]]

if(printFromAnalysisReactiveSetup_BDC) {
  cat('analysisReactiveSetup_BDC.R:', analysisName,
      '  currentBDCnumber=', currentBDCnumber, '\n')
  #cat('\n    BEFORE:  ' )
  #cat('rValues BigDataLastUsed: ')
  print( rValues[[thisBigDataID ]]  [1:3, 1:5] )
}
})



Pvalues = generateAllPvalues(  BigData = BigDataLastUsed)

allChisqPs <<-  Pvalues$allChisqPs
allFisherPs <<- Pvalues$allFisherPs

if(printFromAnalysisReactiveSetup_BDC) {
  # cat('\n    AFTER:  ' )
  # cat( 'analysisReactiveSetup_BDC.R: first 4 Pvalues are = ')
  # print( allChisqPs  [1:4] )
}

if(FALSE)
  browser(text = paste('analysisReactiveSetup: ', analysisName))
# from the Browse> prompt, access with browserText()
#  condition arg and browserCondition() are of no extra use that I can see.


