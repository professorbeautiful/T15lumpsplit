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

