
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
                     #https://stackoverflow.com/questions/37871590/rendering-pdf-in-in-chrome-as-iframe
                     #pdfobject.js
                     #PDFObject.embed("test.pdf", "#'here'");
                     tags$iframe(
                       #type="application/pdf",
                       style="transform: scale(1.0); height:600px; width:100%; frameBorder:0; border:none",
                       src= paste0(filename) #transform: scale(0.75)
                       #data=paste0(filename, '?#zoom=100%') #data=
                     )
    )
  )
}


conditionalPanelWithCheckbox = function(
  labelString,
  filename,
  html='',
  initialValue=FALSE,
  border = FALSE  ### include a border matching conditionalPanelWithCheckboxPDF
) {
  labelStringNoSpaces = gsub("[- .&():'?!]", "_", labelString)
  labelStringId = paste0(labelStringNoSpaces, 'Id')
  labelString = gsub('__.*', '', labelString)
  ## Remove extra numbering required to make labelStringId unique.
  cbStringId = paste0('cb', labelStringId)
  if(!missing(filename))
    html = (tagList(inclRmd(filename), html) )
  #else html = tagList(html)
  if(border)
    html = tags$iframe(style="height:600px; width:100%; border:5",
                                            html)
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
