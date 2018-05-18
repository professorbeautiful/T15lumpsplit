
updateTableCells = function(data, isResetting=FALSE) {
  DLdataMyChoice = rValues$DLdataMyChoice
  updateNumericInput(session, 'mRD', value=data['D','R'])
  updateNumericInput(session, 'mND', value=data['D','N'])
  updateNumericInput(session, 'mRL', value=data['L','R'])
  updateNumericInput(session, 'mNL', value=data['L','N'])
  if( ! isResetting)
    rValues$DLdataMyChoice = DLdataMyChoice
}

dataTableComponent = function() {
  thisDTCNumber = nextNumber(sequenceType = "DTC")
  outputIdThisDTC = paste0('outputDTC', thisDTCNumber)
  panelIdThisDTC = paste0('idPanelDTC', thisDTCNumber)
  resetIdThisDTC = paste0('idResetDTC', thisDTCNumber)
  myChoiceIdThisDTC = paste0('idMyChoiceDTC', thisDTCNumber)
  output[[outputIdThisDTC]] = renderUI({
    #### resetIdThisDTC ####
    observeEvent(
      eventExpr = input[[resetIdThisDTC]],
      handlerExpr =  {
        updateDLdataMyChoice$suspend()
        isolate({
          rValues$resetting = TRUE
          print('        rValues$resetting = TRUE')
          updateTableCells(data = DLdataOriginal, isResetting=TRUE)
        })
        updateDLdataMyChoice$resume()

      })
    #### myChoiceIdThisDTC ####
    observeEvent(
      eventExpr = input[[myChoiceIdThisDTC]],
      priority = 1,
      handlerExpr =  {
      isolate({
        updateTableCells(data = rValues$DLdataMyChoice, isResetting=FALSE)
      })
    })
    panelOfData(panelIdThisDTC=panelIdThisDTC,
                resetIdThisDTC=resetIdThisDTC,
                myChoiceIdThisDTC=myChoiceIdThisDTC)
  })
  uiOutput(outputId = outputIdThisDTC)
}
includeCSS('rotate-text.css')
dataRowLabel = function(html, angle=360-40, color='green') {
  ### rotating works wiht div and p but not with span
  HTML(paste("<div style='
               color:", color, ";",
             "height:80px; width:120px;
               vertical-align:bottom; horizontal-align:right;",
             paste0(collapse=" ",
                    "-", c("webkit","ms","moz","o"),
                    "-transform:rotate(", angle, "deg);"),
             "'>",
             html, "</div>"
  ))
}

### Do not call panelOfData() directly
panelOfData = function(panelIdThisDTC, resetIdThisDTC, myChoiceIdThisDTC) {
      span(
        actionButton(inputId = resetIdThisDTC, label = "Reset data to original"),
        actionButton(inputId = myChoiceIdThisDTC,
                     label = "Reset data to my choice"),
        conditionalPanelWithCheckbox(
          labelString = "Response by Predictor Table",
          html = div(
            # checkboxInput('toggleShowData', 'Show/Hide the Data Panel', FALSE),
            # conditionalPanel(
            #   'input.toggleShowData',
            splitLayout(style='color:green;', "",
                        HTML("Group '<strong>D</strong>'"),
                        HTML("Group '<strong>L</strong>'"),
                        cellWidths = c("40%",'30%','30%')),
            fluidRow(
              column(4, dataRowLabel( "<b>R</b>esponders")),
              column(4, numericInput('mRD', '#RD', DLdata[1,1])),
              column(4, numericInput('mRL', '#RL', DLdata[2,1]))
            ),
            fluidRow(
              column(4, dataRowLabel( "<b>N</b>onResponders")),
              column(4, numericInput('mND', '#ND', DLdata[1,2])),
              column(4, numericInput('mNL', '#NL', DLdata[2,2]))
            ),
            br(),
            uiOutput(outputId = panelIdThisDTC)
          )
      )
  )
}
