getDLdata = function(analysisName, myChoice=FALSE)
  rValues[[ paste0(ifelse(myChoice, 'DLdataMyChoice', 'DLdata'),
                   mapAnalysisToDTCnumber[analysisName]) ]]

setDLdata = function(value, analysisName, myChoice=FALSE)
  rValues[[ paste0(ifelse(myChoice, 'DLdataMyChoice', 'DLdata'),
                   mapAnalysisToDTCnumber[analysisName]) ]] = value

