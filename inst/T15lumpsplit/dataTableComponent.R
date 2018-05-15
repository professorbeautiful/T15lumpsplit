dataTableComponent = function(...) {
  thisDTCNumber = nextNumber(sequenceType = "DTC")
  outputIdThisDTC = paste0('DTC', thisDTCNumber)
  panelIdThisDTC = paste0('id', outputIdThisDTC)
  output[[outputIdThisDTC]] = renderUI({
    observeEvent(eventExpr = input$resetData, handlerExpr =  {
      isolate({
        updateNumericInput(session, 'mRD', value=DLdata['D','R'])
        updateNumericInput(session, 'mRL', value=DLdata['L','R'])
        updateNumericInput(session, 'mND', value=DLdata['D','N'])
        updateNumericInput(session, 'mNL', value=DLdata['L','N'])
      })
    })
    panelOfData(panelIdThisDTC, ...)
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
panelOfData = function(...) {
  conditionalPanelWithCheckbox(
    labelString = "Response by Predictor Table",
    html =
      # checkboxInput('toggleShowData', 'Show/Hide the Data Panel', FALSE),
      # conditionalPanel(
      #   'input.toggleShowData',
      wellPanel(
        actionButton(inputId = 'resetData', label = "Reset data"),
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
        uiOutput(outputId = 'responseRates'),
        hr()
      )
  )
}
