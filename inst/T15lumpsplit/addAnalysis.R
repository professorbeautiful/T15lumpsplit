##  addAnalysis.R


###   Not yet used:  addAnalysis  .  Possibly a way to avoid a source().
addAnalysis = function(analysisName, analysisLabel, context='DTC') {
  jumpListName = paste0('jumpList_', context)
  analysisMapName = paste0('mapAnalysisTo', context, 'number')
  if(!exists(analysisMapName))
    assign(analysisMapName, numeric(0))

  command = paste0(jumpListName, ' = c(', jumpListName, ',',
                   analysisName, '=', analysisLabel)
  eval(command)
}
