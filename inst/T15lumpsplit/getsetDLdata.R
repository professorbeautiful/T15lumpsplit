getDLdata = function(analysisName, myChoice=FALSE, DTCnumber) {
  if( ! missing(analysisName))
    DTCnumber = mapAnalysisToDTCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'DLdataMyChoice', 'DLdata'),
                 DTCnumber)
  #cat('getDLdata: ', index, '\n')
  rValues[[ index ]]
}

setDLdata = function(value, analysisName, myChoice=FALSE, DTCnumber) {
  if( ! missing(analysisName))
    DTCnumber = mapAnalysisToDTCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'DLdataMyChoice', 'DLdata'),
                 DTCnumber)
  #cat('setDLdata: ', index, '\n')
  rValues[[ index ]] = value
}

