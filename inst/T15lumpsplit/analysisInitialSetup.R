### First, append to jumpList:
##    jumpList = c(jumpList, plotLumpSplitPoints='Plot of lump & split points')
##  Then,   source('analysisInitialSetup.R', local=TRUE)

analysisNumber = length(jumpList)
analysisName = names(jumpList)[[analysisNumber]]
DLdata[[analysisNumber]] = DLdataOriginal
cat("Creating DLdata[[analysisNumber]] for analysisNumber = ",
    analysisNumber, '\n')
a(name=paste0('section-a_', analysisName) )
