'#### Complication:
For DTC, the data in the numericInputs and DLdata are the same numbers.
For BDC, tauTrue does not determine BigData and vice versa.
So we have to manage them both.'

getBigData = function(analysisName, myChoice=FALSE, BDCnumber) {
  if( ! missing(analysisName))
    BDCnumber = mapAnalysisToBDCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'BigDataMyChoice', 'BigData'),
                 BDCnumber)
  rValues[[ index ]]
}
setBigData = function(value, analysisName, myChoice=FALSE, BDCnumber) {
  if( ! missing(analysisName))
    BDCnumber = mapAnalysisToBDCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'BigDataMyChoice', 'BigData'),
                 BDCnumber)
  rValues[[ index ]] = value
}

get_thisTauTrueID =  function(aN) {
  if(is.character(aN))
    aN  = mapAnalysisToBDCnumber[aN]
  paste0('BigDataController_ID_', aN)
}

getBigData_tauTrue = function(analysisName, myChoice=FALSE, BDCnumber) {
  if( ! missing(analysisName))
    BDCnumber = mapAnalysisToBDCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'BigData_tauTrue_MyChoice', 'BigData_tauTrue'),
                 BDCnumber)
  rValues[[ index ]]
}
setBigData_tauTrue = function(value, analysisName, myChoice=FALSE, BDCnumber) {
  if( ! missing(analysisName))
    BDCnumber = mapAnalysisToBDCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'BigData_tauTrue_MyChoice', 'BigData_tauTrue'),
                 BDCnumber)
  rValues[[ index ]] = value
}

