### analysisReactiveSetup.R
## Initialize data for this analysis if necessary.

### First, set analysisName before
#        source('analysisReactiveSetup.R', local=TRUE)        #  NO
#        source('inst/T15lumpsplit/analysisReactiveSetup.R', local=TRUE)  # YES
#  But this path will not work out of the installed package.

cat('analysisReactiveSetup.R: ')
cat( analysisName, '  ', system('pwd', intern=TRUE), '\n')
if(is.null(rValues$DLdata[[analysisName]]))
  rValues$DLdata[[analysisName]] = DLdata[[analysisName]]
thisData = rValues$DLdata[[analysisName]]
rValues$DLdataLastUsed = thisData
