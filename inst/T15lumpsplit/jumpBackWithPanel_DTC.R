observeEvent(input$linktext, {
  # update the cells for the corresponding analysis
  analysisName = names(jumpList_DTC)[match(input$linktext, jumpList_DTC)]
  rValues$linkedAnalysisName = analysisName
})

jumpBackWithPaneL_DTC = function(analysisNumber, thisDTCNumber) {
  theJumpLinks =
    #tag('ul',
        tagList( lapply(1:length(jumpList_DTC), function(aN)
          tagList(
            linkinLink(paste0('a_', names(jumpList_DTC)[aN]),
                      paste0('▸ ', jumpList_DTC[[aN]]) ),
            br()
          )
        )
        )
    #) ## OK it works!
  labelString = print(paste('Jump to other analyses', '___',
              #getSequenceLength(sequenceType="DTC"))
              thisDTCNumber) )
  conditionalPanelWithCheckbox(
    labelString = labelString,
    #html='HERE',
    html=theJumpLinks,
    #  html=tagList(theJumpLinks, '(Press ESC key to return here.)'),
    initialValue=FALSE)
}

