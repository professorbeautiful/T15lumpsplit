getTau = function(analysisName, myChoice=FALSE, BDCnumber) {
  if( ! missing(analysisName))
    BDCnumber = mapAnalysisToBDCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'tauTrueMyChoice', 'tauTrue'),
                 BDCnumber)
  #cat('gettauTrue: ', index, '\n')
  rValues[[ index ]]
}
setTau = function(value, analysisName, myChoice=FALSE, BDCnumber) {
  if( ! missing(analysisName))
    BDCnumber = mapAnalysisToBDCnumber[analysisName]
  index = paste0(ifelse(myChoice, 'tauTrueMyChoice', 'tauTrue'),
                 BDCnumber)
  #cat('tauTrue: ', index, '\n')
  rValues[[ index ]] = value
}

