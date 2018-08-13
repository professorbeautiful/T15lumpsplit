
#### conditionalPanelWithCheckbox ####
showhideString = function(labelString, value)
  paste0(ifelse(value, "Hide: ", "Show: "), labelString )

conditionalPanelWithCheckboxPDF = function(labelString, filename, cbStringId) {
  observeEvent(
    input[[cbStringId]], {
      fullString = showhideString(labelString, input[[cbStringId]])
      updateCheckboxInput(
        session, cbStringId,
        label=fullString)
    }
  )
  wellPanel(
    checkboxInput(cbStringId,
                  strong(em(showhideString(labelString, FALSE)
                  )),
                  value=FALSE),
    conditionalPanel(condition = paste0('input.', cbStringId),
                     tags$iframe(style="height:600px; width:100%",
                                 src=filename)
    )
  )
}


conditionalPanelWithCheckbox = function(
  labelString,
  filename,
  html='',
  initialValue=FALSE
) {
  labelStringNoSpaces = gsub("[ .'?!]", "_", labelString)
  labelStringId = paste0(labelStringNoSpaces, 'Id')
  cbStringId = paste0('cb', labelStringId)
  if(!missing(filename))
    html = c(tagList(inclRmd(filename)), html)
  observeEvent(
    input[[cbStringId]], {
      fullString = showhideString(labelString, input[[cbStringId]])
      updateCheckboxInput(
        session, cbStringId,
        label=fullString)
    }
  )
  output[[labelStringId]] <- renderUI({
    wellPanel(
      checkboxInput(cbStringId,
                    strong(em(showhideString(labelString, initialValue)
                    )),
                    value=initialValue),
      conditionalPanel(condition = paste0('input.', cbStringId),
                       html )
      #### HERE ####
    )
  })
  uiOutput(labelStringId)
}
