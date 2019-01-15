### analysisReactiveSetup.R
## Initialize data for this analysis if necessary.

### First, set analysisName before
#        source('analysisReactiveSetup.R', local=TRUE)        #  NO
#        source('inst/T15lumpsplit/analysisReactiveSetup.R', local=TRUE)  # YES
#  But this path will not work out of the installed package.
#  Currently I'm linking it to ../..
#cat('analysisReactiveSetup.R: ')
#cat( analysisName, '  ', system('pwd', intern=TRUE), '\n')

thisDTCnum = mapAnalysisToDTCnumber[analysisName]

if(is.null(rValues$DLdata[[ thisDTCnum ]] ) )
  rValues$DLdata[[ thisDTCnum ]] = DLdataOriginal
thisData = rValues$DLdata[[ thisDTCnum ]]
rValues$DLdataLastUsed = thisData

cat('analysisReactiveSetup.R: ')
print(thisData)


