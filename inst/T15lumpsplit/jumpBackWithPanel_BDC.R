# observeEvent(input$linktext, {
#   # update tauTrue for the corresponding analysis
#   analysisName = names(jumpList_BDC)[match(input$linktext, jumpList_BDC)]
#   if(!is.na(analysisName)) {
#     cat('observeEvent for linktext, analysisName is ', analysisName, '\n')
#     rValues$linkedAnalysisName = analysisName
#   }
# })

jumpBackWithPanel_BDC = function(analysisNumber, thisBDCNumber) {
  theJumpLinks =
    #tag('ul',
        tagList( lapply(1:length(jumpList_BDC), function(aN)
          tagList(
            linkinLink(paste0('a_', names(jumpList_BDC)[aN]),
                      paste0('▸ ', jumpList_BDC[[aN]]) ),
            br()
          )
        )
        )
    #) ## OK it works!
  labelString = print(paste('Jump to other analyses (100 features)', '___',
              #getSequenceLength(sequenceType="BDC"))
              thisBDCNumber) )
  conditionalPanelWithCheckbox(
    labelString = labelString,
    #html='HERE',
    html=theJumpLinks,
    #  html=tagList(theJumpLinks, '(Press ESC key to return here.)'),
    initialValue=FALSE)
}

