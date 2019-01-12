### analysisReactiveSetup.R
## Initialize data for this analysis if necessary.

### First, set analysisName before
#        source('analysisReactiveSetup.R', local=TRUE)

cat('analysisReactiveSetup.R: ')
cat( analysisName, '\n')
if(is.null(rValues$DLdata[[analysisName]]))
  rValues$DLdata[[analysisName]] = DLdata[[analysisName]]
thisData = rValues$DLdata[[analysisName]]
rValues$DLdataLastUsed = thisData
