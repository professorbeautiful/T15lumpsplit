observeEvent(input$linktext, {
  # update the cells for the corresponding analysis
  analysisName = names(jumpList)[match(input$linktext, jumpList)]
  rValues$linkedAnalysisName = analysisName
})

jumpBackWithPanel = function(analysisNumber, thisDTCNumber) {
  theJumpLinks =
    tag('ul',
        tagList( lapply(1:length(jumpList), function(aN)
          tagList(
            linkinLink(paste0('a_', names(jumpList)[aN]),
                       jumpList[[aN]]),
            br()
          )
        )
        )
    ) ## OK it works!
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

