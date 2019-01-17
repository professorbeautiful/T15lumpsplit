getDLdata = function(analysisName, myChoice=FALSE) {
  index = paste0(ifelse(myChoice, 'DLdataMyChoice', 'DLdata'),
                 mapAnalysisToDTCnumber[analysisName])
  #cat('getDLdata: ', index, '\n')
  rValues[[ index ]]
}

setDLdata = function(value, analysisName, myChoice=FALSE) {
  index = paste0(ifelse(myChoice, 'DLdataMyChoice', 'DLdata'),
         mapAnalysisToDTCnumber[analysisName])
  #cat('setDLdata: ', index, '\n')
  rValues[[ index ]] = value
}

