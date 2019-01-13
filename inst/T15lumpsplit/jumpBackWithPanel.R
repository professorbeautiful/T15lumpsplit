jumpBackWithPanel = function(analysisNumber, thisDTCNumber) {
  theJumpLinks =
    tag('ul',
        tagList( lapply(1:(analysisNumber), function(aN)
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

