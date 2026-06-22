##  addAnalysis.R


###   Not yet used:  addAnalysis  .  Possibly a way to avoid a source().
addAnalysis = function(analysisName, analysisLabel, context='DTC') {
  jumpListName = paste0('jumpList_', context)
  analysisMapName = paste0('mapAnalysisTo', context, 'number')
  if(!exists(analysisMapName))
    assign(analysisMapName, numeric(0))

  command = paste0(jumpListName, ' = c(', jumpListName, ',',
                   analysisName, '=', analysisLabel)
  #from claude :  # BUG: eval() on a character string does nothing in R
  #Should be eval(parse(text = command)). The closing parenthesis is also missing from the pasted string.
  #THIS WAS NEVER COMPLETED so ignore this 'error'.  Claude didnt read "Not yet used".
  eval(command)
}
