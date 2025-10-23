'#### Complication:
For DTC, the data in the numericInputs and DLdata are the same numbers.
For BDC, Omega does not determine BigData and vice versa.
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

get_thisOmegaID =  function(aN) {
  if(is.character(aN))
    aN  = mapAnalysisToBDCnumber[aN]
  paste0('BigDataController_ID_', aN)
}
get_thisRegenerateID =  function(aN) {
  if(is.character(aN))
    aN  = mapAnalysisToBDCnumber[aN]
  paste0('BigDataController_Regenerate_', aN)
}

getBigData_Omega = function(analysisName, myChoice=FALSE, BDCnumber) {
  if( ! missing(analysisName))
    BDCnumber = mapAnalysisToBDCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'BigData_Omega_MyChoice', 'BigData_Omega'),
                 BDCnumber)
  rValues[[ index ]]
}
setBigData_Omega = function(value, analysisName, myChoice=FALSE, BDCnumber) {
  if( missing(analysisName))
    analysisName = names(mapAnalysisToBDCnumber)[
      match(BDCnumber, mapAnalysisToBDCnumber)]
  if( ! missing(analysisName))
    BDCnumber = mapAnalysisToBDCnumber[analysisName]
  updateNumericInput(session, get_thisOmegaID( analysisName),
                     value=value)
  index = paste0(ifelse(myChoice, 'BigData_Omega_MyChoice', 'BigData_Omega'),
                 BDCnumber)
  rValues[[ index ]] = value
}

