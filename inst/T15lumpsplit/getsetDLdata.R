getDLdata = function(analysisName, myChoice=FALSE, DTCnumber) {
  if( ! missing(analysisName))
    DTCnumber = mapAnalysisToDTCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'DLdataMyChoice', 'DLdata'),
                 DTCnumber)
  value = rValues[[ index ]]
  print(paste('\nGGGGGGGET getDLdata: ', index,  'myChoice:', myChoice,
              paste(collapse=',', unlist(value)),'\n'))
  rValues[[ index ]]
}

setDLdata = function(value, analysisName, myChoice=FALSE, DTCnumber) {
  if( ! missing(analysisName))
    DTCnumber = mapAnalysisToDTCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'DLdataMyChoice', 'DLdata'),
                 DTCnumber)
  print(paste('\nSSSSSSSET setDLdata: ', index,  'myChoice:', myChoice,
              paste(collapse=',', unlist(value)),'\n'))


  rValues[[ index ]] = value
}

